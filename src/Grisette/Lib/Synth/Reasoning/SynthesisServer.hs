{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Lib.Synth.Reasoning.SynthesisServer
  ( SynthesisServer,
    TaskException (..),
    newSynthesisServer,
    TaskHandle,
    TaskSet,
    taskId,
    submitTask,
    submitTaskWithTimeout,
    pollTask,
    pollTasks,
    cancelTask,
    cancelTaskWith,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.Async.Pool as Pool
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import qualified Control.Exception as C
import qualified Data.HashMap.Internal.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable (hashWithSalt))
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

data TaskHandle conVal conProg matcher exception
  = TaskHandle
      ( Pool.Async
          ([(IOPair conVal, matcher)], SynthesisResult conProg exception)
      )
      Int
  deriving (Eq)

taskId :: TaskHandle conVal conProg matcher exception -> Int
taskId (TaskHandle _ tid) = tid

instance Hashable (TaskHandle conVal conProg matcher exception) where
  hashWithSalt salt (TaskHandle _ taskId) = hashWithSalt salt taskId

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
  handle <- Pool.async taskGroup $ synthesizeProgWithVerifier task
  taskId <- atomically $ do
    taskId <- readTVar nextVarId
    writeTVar nextVarId (taskId + 1)
    return taskId
  return $ TaskHandle handle taskId

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
pollTask (TaskHandle handle _) = Pool.poll handle

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

cancelTask ::
  TaskHandle conVal conProg matcher exception -> IO ()
cancelTask handle = cancelTaskWith handle TaskCancelled

cancelTaskWith ::
  (C.Exception e) =>
  TaskHandle conVal conProg matcher exception ->
  e ->
  IO ()
cancelTaskWith (TaskHandle handle _) = Pool.cancelWith handle
