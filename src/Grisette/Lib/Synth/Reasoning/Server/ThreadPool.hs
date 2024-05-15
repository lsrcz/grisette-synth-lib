{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Lib.Synth.Reasoning.Server.ThreadPool
  ( TaskHandle (taskId),
    Pool,
    newPool,
    addNewTask,
    pollTaskSTM,
    waitCatchTaskSTM,
    pollTask,
    waitCatchTask,
    cancelTaskWith,
    numOfRunningTasks,
    cancelAllTasksWith,
    alterTaskIfPending,
    startTimeSTM,
    endTimeSTM,
    elapsedTimeSTM,
    startTime,
    endTime,
    elapsedTime,
    maybeStartTimeSTM,
    maybeEndTimeSTM,
    maybeElapsedTimeSTM,
    maybeStartTime,
    maybeEndTime,
    maybeElapsedTime,
  )
where

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import Control.Concurrent.Async (Async, async, cancelWith)
import Control.Concurrent.STM
  ( STM,
    TMVar,
    atomically,
    newEmptyTMVarIO,
    readTMVar,
    tryPutTMVar,
    tryReadTMVar,
  )
import Control.Exception
  ( Exception (fromException, toException),
    SomeException,
    catch,
    mask,
    throwIO,
  )
import Control.Monad (when)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable (hashWithSalt))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Ordered as OM
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Typeable (Typeable, eqT, type (:~:) (Refl))
import GHC.IO (unsafePerformIO)

data CancellingException where
  CancellingException :: (Exception e) => e -> CancellingException

instance Show CancellingException where
  show (CancellingException e) = show e

instance Exception CancellingException

data PendingTask a = PendingTask
  { taskId :: TaskId,
    task :: IO a,
    taskResult :: TMVar (Either SomeException a),
    taskStartTime :: TMVar UTCTime,
    taskEndTime :: TMVar UTCTime
  }

data SomePendingTask where
  SomePendingTask :: (Typeable a) => PendingTask a -> SomePendingTask

taskIdCounter :: IORef Int
taskIdCounter = unsafePerformIO $ newIORef 0
{-# NOINLINE taskIdCounter #-}

type TaskId = Int

freshTaskId :: IO TaskId
freshTaskId = atomicModifyIORef' taskIdCounter (\x -> (x + 1, x))

data Pool = Pool
  { lock :: MVar (),
    poolSize :: Int,
    running :: IORef (HM.HashMap TaskId (Async ())),
    queue :: IORef (OM.OMap TaskId SomePendingTask)
  }

numOfRunningTasks :: Pool -> IO Int
numOfRunningTasks Pool {..} = HM.size <$> readIORef running

data TaskHandle a = TaskHandle
  { taskId :: TaskId,
    taskPool :: Pool,
    taskResult :: TMVar (Either SomeException a),
    taskStartTime :: TMVar UTCTime,
    taskEndTime :: TMVar UTCTime
  }

instance Eq (TaskHandle a) where
  TaskHandle {taskId = a} == TaskHandle {taskId = b} = a == b

instance Hashable (TaskHandle a) where
  hashWithSalt salt TaskHandle {taskId = taskId} =
    hashWithSalt salt taskId

instance Show (TaskHandle a) where
  show TaskHandle {taskId = taskId} = "TaskHandle " ++ show taskId

lockPool :: Pool -> IO ()
lockPool Pool {..} = takeMVar lock

unlockPool :: Pool -> IO ()
unlockPool Pool {..} = putMVar lock ()

withLockedPool :: Pool -> Bool -> IO b -> IO b
withLockedPool pool shouldCheckForStarts action = do
  lockPool pool
  result <-
    action `catch` \(e :: SomeException) -> do
      when shouldCheckForStarts $ startIfHaveSpaceImpl pool
      unlockPool pool
      throwIO e
  when shouldCheckForStarts $ startIfHaveSpaceImpl pool
  unlockPool pool
  return result

newPool :: Int -> IO Pool
newPool poolSize = do
  lock <- newMVar ()
  running <- newIORef HM.empty
  queue <- newIORef OM.empty
  return Pool {..}

taskFun :: Pool -> PendingTask a -> IO ()
taskFun pool@Pool {..} PendingTask {..} = mask $ \restore -> do
  let writeResult result = do
        oldRunning <- readIORef running
        writeIORef running $ HM.delete taskId oldRunning
        atomically $ tryPutTMVar taskResult result
  getCurrentTime >>= atomically . tryPutTMVar taskStartTime
  result <-
    restore task `catch` \(e :: SomeException) -> do
      getCurrentTime >>= atomically . tryPutTMVar taskEndTime
      case fromException e of
        Just (CancellingException e1) ->
          writeResult (Left $ toException e1) >> throwIO e1
        Nothing -> withLockedPool pool True $ writeResult (Left e) >> throwIO e
  withLockedPool pool True $ writeResult (Right result)
  getCurrentTime >>= atomically . tryPutTMVar taskEndTime
  return ()

startIfHaveSpaceImpl :: Pool -> IO ()
startIfHaveSpaceImpl pool@Pool {..} = do
  oldRunning <- readIORef running
  when (HM.size oldRunning < poolSize) $ do
    oldQueue <- readIORef queue
    case OM.elemAt oldQueue 0 of
      Just (_, SomePendingTask pendingTask@PendingTask {..}) -> do
        exception <- atomically $ tryReadTMVar taskResult
        writeIORef queue $ OM.delete taskId oldQueue
        case exception of
          Just _ -> return ()
          Nothing -> do
            taskAsync <- async $ taskFun pool pendingTask
            writeIORef running $ HM.insert taskId taskAsync oldRunning
        startIfHaveSpaceImpl pool
      _ -> return ()

addNewTask :: forall a. (Typeable a) => Pool -> IO a -> IO (TaskHandle a)
addNewTask taskPool@Pool {..} task = withLockedPool taskPool True $ do
  taskId <- freshTaskId
  taskResult <- newEmptyTMVarIO :: IO (TMVar (Either SomeException a))
  taskStartTime <- newEmptyTMVarIO :: IO (TMVar UTCTime)
  taskEndTime <- newEmptyTMVarIO :: IO (TMVar UTCTime)
  oldQueue <- readIORef queue
  writeIORef queue $ oldQueue OM.>| (taskId, SomePendingTask $ PendingTask {..})
  startIfHaveSpaceImpl taskPool
  return $ TaskHandle {..}

cancelTaskWithImpl :: (Exception e) => Pool -> e -> TaskId -> IO ()
cancelTaskWithImpl pool@Pool {..} e taskId = do
  oldRunning <- readIORef running
  oldQueue <- readIORef queue
  case HM.lookup taskId oldRunning of
    Just taskAsync -> do
      cancelWith taskAsync $ CancellingException e
      startIfHaveSpaceImpl pool
    Nothing -> case OM.lookup taskId oldQueue of
      Just (SomePendingTask PendingTask {..}) -> do
        time <- getCurrentTime
        atomically $ do
          tryPutTMVar taskResult (Left $ toException e)
          tryPutTMVar taskStartTime time
          tryPutTMVar taskEndTime time
        return ()
      _ -> return ()

cancelTaskWith :: (Exception e) => e -> TaskHandle a -> IO ()
cancelTaskWith e TaskHandle {taskPool = pool@Pool {..}, taskId} =
  withLockedPool pool True $ cancelTaskWithImpl pool e taskId

cancelAllTasksWith :: (Exception e) => Pool -> e -> IO ()
cancelAllTasksWith pool@Pool {..} e = withLockedPool pool True $ do
  oldPending <- readIORef queue
  mapM_
    ( \(SomePendingTask PendingTask {..}) -> do
        time <- getCurrentTime
        atomically $ do
          tryPutTMVar taskResult (Left $ toException e)
          tryPutTMVar taskStartTime time
          tryPutTMVar taskEndTime time
    )
    $ snd <$> OM.assocs oldPending
  oldRunning <- readIORef running
  mapM_ (`cancelWith` CancellingException e) $ HM.elems oldRunning

pollTaskSTM ::
  (Typeable a) => TaskHandle a -> STM (Maybe (Either SomeException a))
pollTaskSTM TaskHandle {taskResult} = tryReadTMVar taskResult

pollTask :: (Typeable a) => TaskHandle a -> IO (Maybe (Either SomeException a))
pollTask = atomically . pollTaskSTM

waitCatchTaskSTM :: (Typeable a) => TaskHandle a -> STM (Either SomeException a)
waitCatchTaskSTM TaskHandle {taskResult} = readTMVar taskResult

waitCatchTask :: (Typeable a) => TaskHandle a -> IO (Either SomeException a)
waitCatchTask = atomically . waitCatchTaskSTM

alterTaskIfPending :: forall a. (Typeable a) => TaskHandle a -> IO a -> IO ()
alterTaskIfPending
  TaskHandle {taskId, taskPool = pool@Pool {..}}
  newAction =
    withLockedPool pool False $ do
      oldQueue <- readIORef queue
      case OM.lookup taskId oldQueue of
        Just (SomePendingTask (PendingTask {..} :: PendingTask a1)) -> do
          case eqT @a @a1 of
            Just Refl -> do
              writeIORef queue $
                OM.alter
                  ( const $
                      Just $
                        SomePendingTask $
                          PendingTask {task = newAction, ..}
                  )
                  taskId
                  oldQueue
              startIfHaveSpaceImpl pool
            Nothing -> error "alterTaskIfPending: type mismatch"
        Nothing -> return ()

startTimeSTM :: TaskHandle a -> STM UTCTime
startTimeSTM TaskHandle {taskStartTime} = readTMVar taskStartTime

endTimeSTM :: TaskHandle a -> STM UTCTime
endTimeSTM TaskHandle {taskEndTime} = readTMVar taskEndTime

elapsedTimeSTM :: TaskHandle a -> STM NominalDiffTime
elapsedTimeSTM TaskHandle {taskStartTime, taskEndTime} = do
  startTime <- readTMVar taskStartTime
  endTime <- readTMVar taskEndTime
  return $ diffUTCTime endTime startTime

startTime :: TaskHandle a -> IO UTCTime
startTime = atomically . startTimeSTM

endTime :: TaskHandle a -> IO UTCTime
endTime = atomically . endTimeSTM

elapsedTime :: TaskHandle a -> IO NominalDiffTime
elapsedTime = atomically . elapsedTimeSTM

maybeStartTimeSTM :: TaskHandle a -> STM (Maybe UTCTime)
maybeStartTimeSTM TaskHandle {taskStartTime} = tryReadTMVar taskStartTime

maybeEndTimeSTM :: TaskHandle a -> STM (Maybe UTCTime)
maybeEndTimeSTM TaskHandle {taskEndTime} = tryReadTMVar taskEndTime

maybeElapsedTimeSTM :: TaskHandle a -> STM (Maybe NominalDiffTime)
maybeElapsedTimeSTM TaskHandle {taskStartTime, taskEndTime} = do
  maybeStartTime <- tryReadTMVar taskStartTime
  maybeEndTime <- tryReadTMVar taskEndTime
  case (maybeStartTime, maybeEndTime) of
    (Just startTime, Just endTime) ->
      return $ Just $ diffUTCTime endTime startTime
    _ -> return Nothing

maybeStartTime :: TaskHandle a -> IO (Maybe UTCTime)
maybeStartTime = atomically . maybeStartTimeSTM

maybeEndTime :: TaskHandle a -> IO (Maybe UTCTime)
maybeEndTime = atomically . maybeEndTimeSTM

maybeElapsedTime :: TaskHandle a -> IO (Maybe NominalDiffTime)
maybeElapsedTime = atomically . maybeElapsedTimeSTM
