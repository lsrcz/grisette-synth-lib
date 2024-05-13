{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Reasoning.Server.ThreadPool
  ( TaskId,
    Pool,
    newPool,
    addNewTask,
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
  ( TMVar,
    atomically,
    newEmptyTMVarIO,
    readTMVar,
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
import Data.Foldable (Foldable (toList))
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Sequence (Seq ((:<|), (:|>)))

data PendingTask a = PendingTask
  { taskId :: TaskId,
    task :: IO a
  }

newtype TaskId = TaskId {unTaskId :: Int}
  deriving newtype (Eq, Ord, Show, Hashable)

data Pool a = Pool
  { lock :: MVar (),
    poolSize :: Int,
    running :: IORef (HM.HashMap TaskId (Async a)),
    queue :: IORef (Seq (PendingTask a)),
    results :: IORef (HM.HashMap TaskId (TMVar (Either SomeException a))),
    nextTaskId :: IORef TaskId
  }

numOfRunningTasks :: Pool a -> IO Int
numOfRunningTasks Pool {..} = HM.size <$> readIORef running

lockPool :: Pool a -> IO ()
lockPool Pool {..} = takeMVar lock

unlockPool :: Pool a -> IO ()
unlockPool Pool {..} = putMVar lock ()

withLockedPool :: Pool a -> Bool -> IO b -> IO b
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

newPool :: Int -> IO (Pool a)
newPool poolSize = do
  lock <- newMVar ()
  running <- newIORef HM.empty
  results <- newIORef HM.empty
  queue <- newIORef mempty
  nextTaskId <- newIORef $ TaskId 0
  return Pool {..}

taskFun :: Pool a -> TaskId -> IO a -> IO a
taskFun pool@Pool {..} taskId task = mask $ \restore -> do
  let writeResult result = withLockedPool pool True $ do
        oldRunning <- readIORef running
        writeIORef running $ HM.delete taskId oldRunning
        oldResults <- readIORef results
        atomically $ tryPutTMVar (oldResults HM.! taskId) result
  result <-
    restore task `catch` \(e :: SomeException) -> do
      writeResult (Left e) >> throwIO e
  writeResult (Right result)
  return result

startIfHaveSpaceImpl :: Pool a -> IO ()
startIfHaveSpaceImpl pool@Pool {..} = do
  oldRunning <- readIORef running
  when (HM.size oldRunning < poolSize) $ do
    oldQueue <- readIORef queue
    case oldQueue of
      (PendingTask {..} :<| rest) -> do
        oldResults <- readIORef results
        result <- atomically $ tryReadTMVar (oldResults HM.! taskId)
        writeIORef queue rest
        case result of
          Just _ -> return ()
          Nothing -> do
            taskAsync <- async $ taskFun pool taskId task
            writeIORef running $ HM.insert taskId taskAsync oldRunning
        startIfHaveSpaceImpl pool
      _ -> return ()

addNewTask :: Pool a -> IO a -> IO TaskId
addNewTask pool@Pool {..} task = withLockedPool pool True $ do
  taskId <- readIORef nextTaskId
  oldQueue <- readIORef queue
  writeIORef nextTaskId (TaskId $ unTaskId taskId + 1)
  writeIORef queue $ oldQueue :|> PendingTask {..}
  result <- newEmptyTMVarIO
  modifyIORef' results $ HM.insert taskId result
  startIfHaveSpaceImpl pool
  return taskId

cancelTaskWithImpl :: (Exception e) => Pool a -> e -> TaskId -> IO ()
cancelTaskWithImpl Pool {..} e taskId = do
  oldRunning <- readIORef running
  oldResults <- readIORef results
  atomically $ tryPutTMVar (oldResults HM.! taskId) (Left $ toException e)
  case HM.lookup taskId oldRunning of
    Just taskAsync ->
      -- Cancellation is asynchronous, however, from the view of pollTask or
      -- waitCatchTask, the task is already cancelled.
      --
      -- This is to avoid deadlock.
      void $ async $ cancelWith taskAsync e
    Nothing -> return ()

cancelTaskWith :: (Exception e) => Pool a -> e -> TaskId -> IO ()
cancelTaskWith pool e taskId =
  withLockedPool pool True $ cancelTaskWithImpl pool e taskId

cancelAllTasksWith :: (Exception e) => Pool a -> e -> IO ()
cancelAllTasksWith pool@Pool {..} e = withLockedPool pool True $ do
  oldRunning <- readIORef running
  oldPending <- readIORef queue
  mapM_ (cancelTaskWithImpl pool e) $
    HM.keys oldRunning
      <> (taskId <$> toList oldPending)

pollTask :: Pool a -> TaskId -> IO (Maybe (Either SomeException a))
pollTask pool@Pool {..} taskId = do
  oldResults <- withLockedPool pool False $ readIORef results
  atomically $ tryReadTMVar (oldResults HM.! taskId)

waitCatchTask :: Pool a -> TaskId -> IO (Either SomeException a)
waitCatchTask pool@Pool {..} taskId = do
  oldResults <- withLockedPool pool False $ readIORef results
  atomically $ readTMVar (oldResults HM.! taskId)
