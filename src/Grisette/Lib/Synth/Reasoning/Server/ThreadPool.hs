{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Reasoning.Server.ThreadPool
  ( TaskHandle,
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
  )
where

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import Control.Concurrent.Async (Async, async, cancelWith)
import Control.Concurrent.STM
  ( STM,
    TMVar,
    TVar,
    atomically,
    modifyTVar',
    newEmptyTMVarIO,
    newTVarIO,
    readTMVar,
    readTVar,
    readTVarIO,
    tryPutTMVar,
    tryReadTMVar,
  )
import Control.Exception
  ( Exception (toException),
    SomeException,
    catch,
    mask,
    throwIO,
  )
import Control.Monad (void, when)
import Data.Dynamic (Dynamic, Typeable, fromDyn, toDyn)
import Data.Foldable (Foldable (toList))
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable (hashWithSalt))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Sequence (Seq ((:<|), (:|>)))

data PendingTask = PendingTask
  { taskId :: TaskId,
    task :: IO Dynamic
  }

type TaskId = Int

data Pool = Pool
  { lock :: MVar (),
    poolSize :: Int,
    running :: IORef (HM.HashMap TaskId (Async Dynamic)),
    queue :: IORef (Seq PendingTask),
    results :: TVar (HM.HashMap TaskId (TMVar (Either SomeException Dynamic))),
    nextTaskId :: IORef TaskId
  }

numOfRunningTasks :: Pool -> IO Int
numOfRunningTasks Pool {..} = HM.size <$> readIORef running

data TaskHandle a = TaskHandle {taskHandleId :: TaskId, taskPool :: Pool}

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
  results <- newTVarIO HM.empty
  queue <- newIORef mempty
  nextTaskId <- newIORef 0
  return Pool {..}

taskFun :: Pool -> TaskId -> IO Dynamic -> IO Dynamic
taskFun pool@Pool {..} taskId task = mask $ \restore -> do
  let writeResult result = withLockedPool pool True $ do
        oldRunning <- readIORef running
        writeIORef running $ HM.delete taskId oldRunning
        oldResults <- readTVarIO results
        atomically $ tryPutTMVar (oldResults HM.! taskId) result
  result <-
    restore task `catch` \(e :: SomeException) -> do
      writeResult (Left e) >> throwIO e
  writeResult (Right result)
  return result

startIfHaveSpaceImpl :: Pool -> IO ()
startIfHaveSpaceImpl pool@Pool {..} = do
  oldRunning <- readIORef running
  when (HM.size oldRunning < poolSize) $ do
    oldQueue <- readIORef queue
    case oldQueue of
      (PendingTask {..} :<| rest) -> do
        oldResults <- readTVarIO results
        result <- atomically $ tryReadTMVar (oldResults HM.! taskId)
        writeIORef queue rest
        case result of
          Just _ -> return ()
          Nothing -> do
            taskAsync <- async $ taskFun pool taskId task
            writeIORef running $ HM.insert taskId taskAsync oldRunning
        startIfHaveSpaceImpl pool
      _ -> return ()

addNewTask :: (Typeable a) => Pool -> IO a -> IO (TaskHandle a)
addNewTask pool@Pool {..} taskBase = withLockedPool pool True $ do
  taskId <- readIORef nextTaskId
  oldQueue <- readIORef queue
  writeIORef nextTaskId (taskId + 1)
  let task = toDyn <$> taskBase
  writeIORef queue $ oldQueue :|> PendingTask {..}
  result <- newEmptyTMVarIO
  atomically $ modifyTVar' results $ HM.insert taskId result
  startIfHaveSpaceImpl pool
  return $ TaskHandle taskId pool

cancelTaskWithImpl :: (Exception e) => Pool -> e -> TaskId -> IO ()
cancelTaskWithImpl Pool {..} e taskId = do
  oldRunning <- readIORef running
  oldResults <- readTVarIO results
  atomically $ tryPutTMVar (oldResults HM.! taskId) (Left $ toException e)
  case HM.lookup taskId oldRunning of
    Just taskAsync ->
      -- Cancellation is asynchronous, however, from the view of pollTask or
      -- waitCatchTask, the task is already cancelled.
      --
      -- This is to avoid deadlock.
      void $ async $ cancelWith taskAsync e
    Nothing -> return ()

cancelTaskWith :: (Exception e) => e -> TaskHandle a -> IO ()
cancelTaskWith e taskHandle@TaskHandle {taskPool = pool@Pool {..}} =
  withLockedPool pool True $ cancelTaskWithImpl pool e $ taskHandleId taskHandle

cancelAllTasksWith :: (Exception e) => Pool -> e -> IO ()
cancelAllTasksWith pool@Pool {..} e = withLockedPool pool True $ do
  oldRunning <- readIORef running
  oldPending <- readIORef queue
  mapM_ (cancelTaskWithImpl pool e) $
    HM.keys oldRunning <> (taskId <$> toList oldPending)

pollTaskSTM ::
  (Typeable a) => TaskHandle a -> STM (Maybe (Either SomeException a))
pollTaskSTM taskHandle@TaskHandle {taskPool = Pool {..}} = do
  oldResults <- readTVar results
  r <- tryReadTMVar (oldResults HM.! taskHandleId taskHandle)
  return $ fmap (fmap $ flip fromDyn (error "BUG: bad type")) r

pollTask :: (Typeable a) => TaskHandle a -> IO (Maybe (Either SomeException a))
pollTask = atomically . pollTaskSTM

waitCatchTaskSTM :: (Typeable a) => TaskHandle a -> STM (Either SomeException a)
waitCatchTaskSTM taskHandle@TaskHandle {taskPool = Pool {..}} = do
  oldResults <- readTVar results
  r <- readTMVar (oldResults HM.! taskHandleId taskHandle)
  return $ fmap (`fromDyn` error "BUG: bad type") r

waitCatchTask :: (Typeable a) => TaskHandle a -> IO (Either SomeException a)
waitCatchTask = atomically . waitCatchTaskSTM
