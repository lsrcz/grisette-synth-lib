{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Lib.Synth.Reasoning.SynthesisServer
  ( SynthesisServer,
    TaskException (..),
    newSynthesisServer,
    TaskHandle (taskId, taskStartTime),
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
import Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.Async.Pool as Pool
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    newEmptyTMVarIO,
    newTVarIO,
    putTMVar,
    readTMVar,
    readTVar,
    writeTVar,
  )
import Control.Exception (finally)
import qualified Control.Exception as C
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable (hashWithSalt))
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Grisette.Lib.Synth.Reasoning.IOPair (IOPair)
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( SynthesisResult,
    ToSynthesisTask (ConProgType, ConValType, ExceptionType, MatcherType),
    synthesizeProgWithVerifier,
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

data TaskHandle conVal conProg matcher exception = TaskHandle
  { _taskAsync ::
      Pool.Async
        ([(IOPair conVal, matcher)], SynthesisResult conProg exception),
    taskId :: Int,
    taskStartTime :: UTCTime,
    _taskEndTime :: STM UTCTime
  }

-- | Get the end time of a task. This function blocks until the task is
-- finished.
taskEndTime :: TaskHandle conVal conProg matcher exception -> IO UTCTime
taskEndTime (TaskHandle _ _ _ endTime) = atomically endTime

-- | Get the elapsed time of a task. This function blocks until the task is
-- finished.
taskElapsedTime ::
  TaskHandle conVal conProg matcher exception -> IO NominalDiffTime
taskElapsedTime task = do
  endTime <- taskEndTime task
  return $ endTime `diffUTCTime` taskStartTime task

instance Eq (TaskHandle conVal conProg matcher exception) where
  TaskHandle _ taskId1 _ _ == TaskHandle _ taskId2 _ _ = taskId1 == taskId2

instance Hashable (TaskHandle conVal conProg matcher exception) where
  hashWithSalt salt (TaskHandle _ taskId _ _) = hashWithSalt salt taskId

type TaskSet conVal conProg matcher exception =
  HS.HashSet (TaskHandle conVal conProg matcher exception)

-- | Add a task to the synthesis server.
submitTask ::
  ( ToSynthesisTask task,
    matcher ~ MatcherType task,
    conVal ~ ConValType task,
    conProg ~ ConProgType task,
    exception ~ ExceptionType task
  ) =>
  SynthesisServer ->
  task ->
  IO (TaskHandle conVal conProg matcher exception)
submitTask (SynthesisServer _ taskGroup nextVarId _) task = do
  endTimeTMVar <- newEmptyTMVarIO
  handle <-
    Pool.async taskGroup $
      synthesizeProgWithVerifier task
        `finally` ( getCurrentTime >>= \currentTime ->
                      atomically (putTMVar endTimeTMVar currentTime)
                  )
  taskId <- atomically $ do
    taskId <- readTVar nextVarId
    writeTVar nextVarId (taskId + 1)
    return taskId
  startTime <- getCurrentTime
  return $ TaskHandle handle taskId startTime (readTMVar endTimeTMVar)

-- | Add a task to the synthesis server with a timeout.
--
-- This function may result in zombie processes with sbv-10.9 or earlier.
-- See https://github.com/LeventErkok/sbv/pull/691.
--
-- This function should not be called with a very short timeout, e.g., fewer
-- than 10ms due to a bug in async-pool.
-- See https://github.com/jwiegley/async-pool/issues/31.
submitTaskWithTimeout ::
  ( ToSynthesisTask task,
    matcher ~ MatcherType task,
    conVal ~ ConValType task,
    conProg ~ ConProgType task,
    exception ~ ExceptionType task
  ) =>
  SynthesisServer ->
  Int ->
  task ->
  IO (TaskHandle conVal conProg matcher exception)
submitTaskWithTimeout server timeout task = do
  handle <- submitTask server task
  _ <- async $ threadDelay timeout >> cancelTaskWith handle TaskTimeout
  return handle

pollTask ::
  TaskHandle conVal conProg matcher exception ->
  IO
    ( Maybe
        ( Either
            C.SomeException
            ([(IOPair conVal, matcher)], SynthesisResult conProg exception)
        )
    )
pollTask (TaskHandle handle _ _ _) = Pool.poll handle

waitCatchTask ::
  TaskHandle conVal conProg matcher exception ->
  IO
    ( Either
        C.SomeException
        ([(IOPair conVal, matcher)], SynthesisResult conProg exception)
    )
waitCatchTask (TaskHandle handle _ _ _) = Pool.waitCatch handle

pollTasks ::
  TaskSet conVal conProg matcher exception ->
  IO
    ( TaskSet conVal conProg matcher exception,
      HM.HashMap
        (TaskHandle conVal conProg matcher exception)
        ( Either
            C.SomeException
            ([(IOPair conVal, matcher)], SynthesisResult conProg exception)
        )
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
  TaskHandle conVal conProg matcher exception -> IO ()
cancelTask handle = cancelTaskWith handle TaskCancelled

cancelTaskWith ::
  (C.Exception e) =>
  TaskHandle conVal conProg matcher exception ->
  e ->
  IO ()
cancelTaskWith (TaskHandle handle _ _ _) = Pool.cancelWith handle
