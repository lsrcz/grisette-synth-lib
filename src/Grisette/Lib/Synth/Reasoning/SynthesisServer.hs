{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Reasoning.SynthesisServer
  ( SynthesisServer,
    TaskException (..),
    newSynthesisServer,
    endSynthesisServer,
    withSynthesisServer,
    TaskHandle (taskId),
    maybeTaskStartTime,
    maybeTaskEndTime,
    maybeTaskElapsedTime,
    taskStartTime,
    taskEndTime,
    taskElapsedTime,
    TaskSet,
    submitTask,
    submitTaskWithTimeout,
    pollTask,
    waitCatchTask,
    pollTasks,
    cancelTask,
    cancelTaskWith,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import qualified Control.Concurrent.Async.Pool as Pool
import Control.Concurrent.STM
  ( TMVar,
    TVar,
    atomically,
    newEmptyTMVarIO,
    newTVarIO,
    orElse,
    putTMVar,
    readTMVar,
    readTVar,
    tryPutTMVar,
    writeTVar,
  )
import Control.Exception (finally, mask)
import qualified Control.Exception as C
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable (hashWithSalt))
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Grisette (ConfigurableSolver)
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( SynthesisResult,
    SynthesisTask,
    runSynthesisTask,
  )

data TaskException = TaskCancelled | TaskTimeout
  deriving (Eq, Show)

instance C.Exception TaskException

type TaskId = Int

data SynthesisServer = SynthesisServer
  { _threadPool :: Pool.Pool,
    _taskGroup :: Pool.TaskGroup,
    _nextTaskId :: TVar TaskId,
    _mainThread :: Async ()
  }

-- | Create a new synthesis server.
newSynthesisServer ::
  Int -> IO SynthesisServer
newSynthesisServer numOfTasks = do
  pool <- Pool.createPool
  taskGroup <- Pool.createTaskGroup pool numOfTasks
  mainThread <- async $ Pool.runTaskGroup taskGroup
  nextTaskId <- newTVarIO 0
  return $ SynthesisServer pool taskGroup nextTaskId mainThread

endSynthesisServer :: SynthesisServer -> IO ()
endSynthesisServer (SynthesisServer _ taskGroup _ mainThread) = do
  Pool.cancelAll taskGroup
  cancel mainThread

withSynthesisServer :: Int -> (SynthesisServer -> IO a) -> IO a
withSynthesisServer numOfTasks =
  C.bracket (newSynthesisServer numOfTasks) endSynthesisServer

data TaskHandle conProg = TaskHandle
  { _taskAsync :: Pool.Async (SynthesisResult conProg),
    taskId :: Int,
    _taskStartTime :: TMVar UTCTime,
    _taskEndTime :: TMVar UTCTime,
    _cancelledResult :: TMVar (Either C.SomeException (SynthesisResult conProg))
  }

-- | Get the start time of a task. This function return Nothing if the task
-- haven't really started.
maybeTaskStartTime :: TaskHandle conProg -> IO (Maybe UTCTime)
maybeTaskStartTime (TaskHandle _ _ startTime _ _) =
  atomically $ (Just <$> readTMVar startTime) `orElse` return Nothing

-- | Get the start time of a task. This function blocks until the task is
-- really started.
taskStartTime :: TaskHandle conProg -> IO UTCTime
taskStartTime (TaskHandle _ _ startTime _ _) = atomically $ readTMVar startTime

-- | Get the end time of a task. This function return Nothing if the task
-- haven't finished.
maybeTaskEndTime :: TaskHandle conProg -> IO (Maybe UTCTime)
maybeTaskEndTime (TaskHandle _ _ _ endTime _) =
  atomically $ (Just <$> readTMVar endTime) `orElse` return Nothing

-- | Get the end time of a task. This function blocks until the task is
-- finished.
taskEndTime :: TaskHandle conProg -> IO UTCTime
taskEndTime (TaskHandle _ _ _ endTime _) = atomically $ readTMVar endTime

-- | Get the elapsed time of a task. This function returns Nothing if the task
-- haven't finished.
maybeTaskElapsedTime :: TaskHandle conProg -> IO (Maybe NominalDiffTime)
maybeTaskElapsedTime task = do
  startTime <- maybeTaskStartTime task
  endTime <- maybeTaskEndTime task
  return $ diffUTCTime <$> endTime <*> startTime

-- | Get the elapsed time of a task. This function blocks until the task is
-- finished.
taskElapsedTime :: TaskHandle conProg -> IO NominalDiffTime
taskElapsedTime task = do
  startTime <- taskStartTime task
  endTime <- taskEndTime task
  return $ endTime `diffUTCTime` startTime

instance Eq (TaskHandle conProg) where
  TaskHandle _ taskId1 _ _ _ == TaskHandle _ taskId2 _ _ _ = taskId1 == taskId2

instance Hashable (TaskHandle conProg) where
  hashWithSalt salt (TaskHandle _ taskId _ _ _) = hashWithSalt salt taskId

type TaskSet conProg = HS.HashSet (TaskHandle conProg)

submitTaskImpl ::
  (ConfigurableSolver config h) =>
  Maybe Int ->
  SynthesisServer ->
  config ->
  SynthesisTask conProg ->
  IO (TaskHandle conProg)
submitTaskImpl
  maybeTimeout
  (SynthesisServer _ taskGroup nextVarId _)
  config
  task = do
    startTimeTMVar <- newEmptyTMVarIO
    endTimeTMVar <- newEmptyTMVarIO
    taskHandleTMVar <- newEmptyTMVarIO
    cancelledResultTMVar <- newEmptyTMVarIO
    handle <-
      Pool.async taskGroup $ mask $ \restore -> do
        selfHandle <- atomically $ readTMVar taskHandleTMVar
        case maybeTimeout of
          Just timeout -> do
            async $ threadDelay timeout >> cancelTaskWith selfHandle TaskTimeout
            return ()
          Nothing -> return ()
        getCurrentTime >>= atomically . tryPutTMVar startTimeTMVar
        restore (runSynthesisTask config task)
          `finally` (getCurrentTime >>= atomically . tryPutTMVar endTimeTMVar)
    taskId <- atomically $ do
      taskId <- readTVar nextVarId
      writeTVar nextVarId (taskId + 1)
      return taskId
    let taskHandle =
          TaskHandle
            handle
            taskId
            startTimeTMVar
            endTimeTMVar
            cancelledResultTMVar
    atomically $ putTMVar taskHandleTMVar taskHandle
    return taskHandle

-- | Add a task to the synthesis server.
submitTask ::
  (ConfigurableSolver config h) =>
  SynthesisServer ->
  config ->
  SynthesisTask conProg ->
  IO (TaskHandle conProg)
submitTask = submitTaskImpl Nothing

-- | Add a task to the synthesis server with a timeout.
--
-- This function may result in zombie processes with sbv-10.9 or earlier.
-- See https://github.com/LeventErkok/sbv/pull/691.
--
-- This function should not be called with a very short timeout, e.g., fewer
-- than 10ms due to a bug in async-pool.
-- See https://github.com/jwiegley/async-pool/issues/31.
submitTaskWithTimeout ::
  (ConfigurableSolver config h) =>
  SynthesisServer ->
  config ->
  Int ->
  SynthesisTask conProg ->
  IO (TaskHandle conProg)
submitTaskWithTimeout server config timeout =
  submitTaskImpl (Just timeout) server config

pollTask ::
  TaskHandle conProg ->
  IO (Maybe (Either C.SomeException (SynthesisResult conProg)))
pollTask (TaskHandle handle _ _ _ _) = Pool.poll handle

waitCatchTask ::
  TaskHandle conProg -> IO (Either C.SomeException (SynthesisResult conProg))
waitCatchTask (TaskHandle handle _ _ _ cancelledResult) =
  atomically $ Pool.waitCatchSTM handle `orElse` readTMVar cancelledResult

pollTasks ::
  TaskSet conProg ->
  IO
    ( TaskSet conProg,
      HM.HashMap
        (TaskHandle conProg)
        (Either C.SomeException (SynthesisResult conProg))
    )
pollTasks tasks = go (HS.toList tasks) HS.empty HM.empty
  where
    go [] nonFinished finished = return (nonFinished, finished)
    go (task : tasks) nonFinished finished = do
      r <- pollTask task
      case r of
        Nothing -> go tasks (HS.insert task nonFinished) finished
        Just r -> go tasks nonFinished (HM.insert task r finished)

-- | Cancel a task.
--
-- This function may result in zombie processes with sbv-10.9 or earlier.
-- See https://github.com/LeventErkok/sbv/pull/691.
--
-- This function should not be called immediately after submitting a task due to
-- a bug in async-pool. See https://github.com/jwiegley/async-pool/issues/31.
cancelTask ::
  TaskHandle conProg -> IO ()
cancelTask handle = cancelTaskWith handle TaskCancelled

cancelTaskWith ::
  (C.Exception e) =>
  TaskHandle conProg ->
  e ->
  IO ()
cancelTaskWith (TaskHandle handle _ startTime endTime cancelledResult) e = do
  Pool.cancelWith handle e
  currentTime <- getCurrentTime
  atomically $ tryPutTMVar startTime currentTime
  atomically $ tryPutTMVar endTime currentTime
  atomically $
    tryPutTMVar cancelledResult $
      Left $
        C.SomeException TaskCancelled
  return ()
