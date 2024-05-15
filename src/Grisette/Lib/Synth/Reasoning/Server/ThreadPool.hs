{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Lib.Synth.Reasoning.Server.ThreadPool
  ( TaskHandle (taskHandleId),
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
import Data.Typeable (Typeable, eqT, type (:~:) (Refl))
import GHC.IO (unsafePerformIO)

data CancellingException where
  CancellingException :: (Exception e) => e -> CancellingException

instance Show CancellingException where
  show (CancellingException e) = show e

instance Exception CancellingException

data PendingTask a = PendingTask
  { task :: IO a,
    taskResultSlot :: TMVar (Either SomeException a)
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
  { taskHandleId :: TaskId,
    taskPool :: Pool,
    taskResult :: TMVar (Either SomeException a)
  }

instance Eq (TaskHandle a) where
  TaskHandle {taskHandleId = a} == TaskHandle {taskHandleId = b} = a == b

instance Hashable (TaskHandle a) where
  hashWithSalt salt TaskHandle {taskHandleId = taskId} =
    hashWithSalt salt taskId

instance Show (TaskHandle a) where
  show TaskHandle {taskHandleId = taskId} = "TaskHandle " ++ show taskId

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

taskFun :: Pool -> TaskId -> TMVar (Either SomeException a) -> IO a -> IO ()
taskFun pool@Pool {..} taskId resultTMVar task = mask $ \restore -> do
  let writeResult result = do
        oldRunning <- readIORef running
        writeIORef running $ HM.delete taskId oldRunning
        atomically $ tryPutTMVar resultTMVar result
  result <-
    restore task `catch` \(e :: SomeException) ->
      case fromException e of
        Just (CancellingException e1) ->
          writeResult (Left $ toException e1) >> throwIO e1
        Nothing -> withLockedPool pool True $ writeResult (Left e) >> throwIO e
  withLockedPool pool True $ writeResult (Right result)
  return ()

startIfHaveSpaceImpl :: Pool -> IO ()
startIfHaveSpaceImpl pool@Pool {..} = do
  oldRunning <- readIORef running
  when (HM.size oldRunning < poolSize) $ do
    oldQueue <- readIORef queue
    case OM.elemAt oldQueue 0 of
      Just (taskId, SomePendingTask (PendingTask {..})) -> do
        exception <- atomically $ tryReadTMVar taskResultSlot
        writeIORef queue $ OM.delete taskId oldQueue
        case exception of
          Just _ -> return ()
          Nothing -> do
            taskAsync <- async $ taskFun pool taskId taskResultSlot task
            writeIORef running $ HM.insert taskId taskAsync oldRunning
        startIfHaveSpaceImpl pool
      _ -> return ()

addNewTask :: forall a. (Typeable a) => Pool -> IO a -> IO (TaskHandle a)
addNewTask pool@Pool {..} task = withLockedPool pool True $ do
  taskId <- freshTaskId
  taskResultSlot <- newEmptyTMVarIO :: IO (TMVar (Either SomeException a))
  oldQueue <- readIORef queue
  writeIORef queue $ oldQueue OM.>| (taskId, SomePendingTask $ PendingTask {..})
  startIfHaveSpaceImpl pool
  return $ TaskHandle taskId pool taskResultSlot

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
        atomically $ tryPutTMVar taskResultSlot (Left $ toException e)
        return ()
      _ -> return ()

cancelTaskWith :: (Exception e) => e -> TaskHandle a -> IO ()
cancelTaskWith e taskHandle@TaskHandle {taskPool = pool@Pool {..}} =
  withLockedPool pool True $ cancelTaskWithImpl pool e $ taskHandleId taskHandle

cancelAllTasksWith :: (Exception e) => Pool -> e -> IO ()
cancelAllTasksWith pool@Pool {..} e = withLockedPool pool True $ do
  oldRunning <- readIORef running
  oldPending <- readIORef queue
  mapM_ (`cancelWith` CancellingException e) $ HM.elems oldRunning
  mapM_
    ( \(SomePendingTask PendingTask {..}) ->
        atomically $ tryPutTMVar taskResultSlot (Left $ toException e)
    )
    $ snd <$> OM.assocs oldPending
  startIfHaveSpaceImpl pool

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
  TaskHandle {taskHandleId, taskPool = pool@Pool {..}}
  newAction =
    withLockedPool pool False $ do
      oldQueue <- readIORef queue
      case OM.lookup taskHandleId oldQueue of
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
                  taskHandleId
                  oldQueue
              startIfHaveSpaceImpl pool
            Nothing -> error "alterTaskIfPending: type mismatch"
        Nothing -> return ()
