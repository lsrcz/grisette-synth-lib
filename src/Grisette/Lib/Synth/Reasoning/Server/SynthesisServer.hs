{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Reasoning.Server.SynthesisServer
  ( SynthesisServer,
    SynthesisTaskException (..),
    newSynthesisServer,
    endSynthesisServer,
    withSynthesisServer,
    SynthesisTaskHandle (taskId),
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
import Control.Concurrent.Async (async)
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
import Data.Typeable (Typeable)
import Grisette (ConfigurableSolver)
import Grisette.Lib.Synth.Reasoning.Server.ThreadPool
  ( Pool,
    TaskHandle,
    addNewTask,
    cancelAllTasksWith,
    newPool,
  )
import qualified Grisette.Lib.Synth.Reasoning.Server.ThreadPool as Pool
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( SynthesisResult,
    SynthesisTask,
    runSynthesisTask,
  )

data SynthesisTaskException = SynthesisTaskCancelled | SynthesisTaskTimeout
  deriving (Eq, Show)

instance C.Exception SynthesisTaskException

type TaskId = Int

data SynthesisServer = SynthesisServer
  { _threadPool :: Pool,
    _nextTaskId :: TVar TaskId
  }

-- | Create a new synthesis server.
newSynthesisServer ::
  Int -> IO SynthesisServer
newSynthesisServer numOfTasks = do
  pool <- newPool numOfTasks
  nextTaskId <- newTVarIO 0
  return $ SynthesisServer pool nextTaskId

endSynthesisServer :: SynthesisServer -> IO ()
endSynthesisServer (SynthesisServer pool _) = do
  cancelAllTasksWith pool SynthesisTaskCancelled

withSynthesisServer :: Int -> (SynthesisServer -> IO a) -> IO a
withSynthesisServer numOfTasks =
  C.bracket (newSynthesisServer numOfTasks) endSynthesisServer

data SynthesisTaskHandle conProg where
  SynthesisTaskHandle ::
    (Typeable conProg) =>
    { _underlyingHandle :: TaskHandle (SynthesisResult conProg),
      taskId :: Int,
      _taskStartTime :: TMVar UTCTime,
      _taskEndTime :: TMVar UTCTime,
      _cancelledResult ::
        TMVar (Either C.SomeException (SynthesisResult conProg))
    } ->
    SynthesisTaskHandle conProg

-- | Get the start time of a task. This function return Nothing if the task
-- haven't really started.
maybeTaskStartTime :: SynthesisTaskHandle conProg -> IO (Maybe UTCTime)
maybeTaskStartTime (SynthesisTaskHandle _ _ startTime _ _) =
  atomically $ (Just <$> readTMVar startTime) `orElse` return Nothing

-- | Get the start time of a task. This function blocks until the task is
-- really started.
taskStartTime :: SynthesisTaskHandle conProg -> IO UTCTime
taskStartTime (SynthesisTaskHandle _ _ startTime _ _) =
  atomically $ readTMVar startTime

-- | Get the end time of a task. This function return Nothing if the task
-- haven't finished.
maybeTaskEndTime :: SynthesisTaskHandle conProg -> IO (Maybe UTCTime)
maybeTaskEndTime (SynthesisTaskHandle _ _ _ endTime _) =
  atomically $ (Just <$> readTMVar endTime) `orElse` return Nothing

-- | Get the end time of a task. This function blocks until the task is
-- finished.
taskEndTime :: SynthesisTaskHandle conProg -> IO UTCTime
taskEndTime (SynthesisTaskHandle _ _ _ endTime _) =
  atomically $ readTMVar endTime

-- | Get the elapsed time of a task. This function returns Nothing if the task
-- haven't finished.
maybeTaskElapsedTime ::
  SynthesisTaskHandle conProg -> IO (Maybe NominalDiffTime)
maybeTaskElapsedTime task = do
  startTime <- maybeTaskStartTime task
  endTime <- maybeTaskEndTime task
  return $ diffUTCTime <$> endTime <*> startTime

-- | Get the elapsed time of a task. This function blocks until the task is
-- finished.
taskElapsedTime :: SynthesisTaskHandle conProg -> IO NominalDiffTime
taskElapsedTime task = do
  startTime <- taskStartTime task
  endTime <- taskEndTime task
  return $ endTime `diffUTCTime` startTime

instance Eq (SynthesisTaskHandle conProg) where
  SynthesisTaskHandle _ taskId1 _ _ _ == SynthesisTaskHandle _ taskId2 _ _ _ =
    taskId1 == taskId2

instance Hashable (SynthesisTaskHandle conProg) where
  hashWithSalt salt (SynthesisTaskHandle _ taskId _ _ _) =
    hashWithSalt salt taskId

type TaskSet conProg = HS.HashSet (SynthesisTaskHandle conProg)

submitTaskImpl ::
  (ConfigurableSolver config h, Typeable conProg) =>
  Maybe Int ->
  SynthesisServer ->
  config ->
  SynthesisTask conProg ->
  IO (SynthesisTaskHandle conProg)
submitTaskImpl
  maybeTimeout
  (SynthesisServer pool nextVarId)
  config
  task = do
    startTimeTMVar <- newEmptyTMVarIO
    endTimeTMVar <- newEmptyTMVarIO
    taskHandleTMVar <- newEmptyTMVarIO
    cancelledResultTMVar <- newEmptyTMVarIO
    handle <-
      addNewTask pool $ mask $ \restore -> do
        selfHandle <- atomically $ readTMVar taskHandleTMVar
        case maybeTimeout of
          Just timeout -> do
            async $
              threadDelay timeout
                >> cancelTaskWith selfHandle SynthesisTaskTimeout
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
          SynthesisTaskHandle
            handle
            taskId
            startTimeTMVar
            endTimeTMVar
            cancelledResultTMVar
    atomically $ putTMVar taskHandleTMVar taskHandle
    return taskHandle

-- | Add a task to the synthesis server.
submitTask ::
  (ConfigurableSolver config h, Typeable conProg) =>
  SynthesisServer ->
  config ->
  SynthesisTask conProg ->
  IO (SynthesisTaskHandle conProg)
submitTask = submitTaskImpl Nothing

-- | Add a task to the synthesis server with a timeout.
--
-- This function may result in zombie processes with sbv-10.9 or earlier.
-- See https://github.com/LeventErkok/sbv/pull/691.
submitTaskWithTimeout ::
  (ConfigurableSolver config h, Typeable conProg) =>
  SynthesisServer ->
  config ->
  Int ->
  SynthesisTask conProg ->
  IO (SynthesisTaskHandle conProg)
submitTaskWithTimeout server config timeout =
  submitTaskImpl (Just timeout) server config

pollTask ::
  SynthesisTaskHandle conProg ->
  IO (Maybe (Either C.SomeException (SynthesisResult conProg)))
pollTask (SynthesisTaskHandle handle _ _ _ _) = Pool.pollTask handle

waitCatchTask ::
  SynthesisTaskHandle conProg ->
  IO (Either C.SomeException (SynthesisResult conProg))
waitCatchTask (SynthesisTaskHandle handle _ _ _ cancelledResult) =
  atomically $ Pool.waitCatchTaskSTM handle `orElse` readTMVar cancelledResult

pollTasks ::
  TaskSet conProg ->
  IO
    ( TaskSet conProg,
      HM.HashMap
        (SynthesisTaskHandle conProg)
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
  SynthesisTaskHandle conProg -> IO ()
cancelTask handle = cancelTaskWith handle SynthesisTaskCancelled

cancelTaskWith ::
  (C.Exception e) =>
  SynthesisTaskHandle conProg ->
  e ->
  IO ()
cancelTaskWith
  (SynthesisTaskHandle handle _ startTime endTime cancelledResult)
  e = do
    Pool.cancelTaskWith e handle
    currentTime <- getCurrentTime
    atomically $ tryPutTMVar startTime currentTime
    atomically $ tryPutTMVar endTime currentTime
    atomically $
      tryPutTMVar cancelledResult $
        Left $
          C.SomeException SynthesisTaskCancelled
    return ()
