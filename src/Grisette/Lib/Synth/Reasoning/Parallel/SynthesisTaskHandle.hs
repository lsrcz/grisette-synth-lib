{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Grisette.Lib.Synth.Reasoning.Parallel.SynthesisTaskHandle
  ( SynthesisTaskHandle,
    enqueueMinimalCostTask,
    enqueueMinimalCostTaskWithTimeout,
    alterTaskIfPending,
    alterTaskIfPendingWithTimeout,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM
  ( TMVar,
    atomically,
    newEmptyTMVarIO,
    putTMVar,
    readTMVar,
  )
import qualified Control.Exception as C
import Data.Hashable (Hashable)
import Data.Typeable (Typeable)
import Grisette (ConfigurableSolver, Solver (solverAssert), withSolver)
import Grisette.Lib.Synth.Reasoning.Parallel.BaseTaskHandle
  ( BaseTaskHandle
      ( cancelWith,
        endTimeSTM,
        enqueueTaskPrecondMaybeTimeout,
        pollSTM,
        startTimeSTM,
        waitCatchSTM
      ),
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Exception
  ( SynthesisTaskException (SynthesisTaskTimeout),
  )
import Grisette.Lib.Synth.Reasoning.Parallel.ThreadPool (ThreadHandle)
import qualified Grisette.Lib.Synth.Reasoning.Parallel.ThreadPool as Pool
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( SynthesisMinimalCostTask,
    SynthesisResult,
    SynthesisTask,
    Example,
    runSynthesisMinimalCostTaskExtractCex,
    runSynthesisTaskExtractCex,
    solverRunRefinableSynthesisTaskExtractCex,
  )

newtype SynthesisTaskHandle symProg conProg = SynthesisTaskHandle
  { _underlyingHandle ::
      ThreadHandle ([Example symProg], SynthesisResult conProg)
  }
  deriving newtype (Eq, Hashable)

instance
  (Typeable symProg, Typeable conProg) =>
  BaseTaskHandle (SynthesisTaskHandle symProg conProg) symProg conProg
  where
  enqueueTaskPrecondMaybeTimeout timeout pool config priority task precond =
    enqueueActionImpl
      timeout
      pool
      priority
      ( withSolver config $ \solver -> do
          precondition <- precond
          solverAssert solver precondition
          solverRunRefinableSynthesisTaskExtractCex solver task
      )
  startTimeSTM = Pool.startTimeSTM . _underlyingHandle
  endTimeSTM = Pool.endTimeSTM . _underlyingHandle
  pollSTM = Pool.pollSTM . _underlyingHandle
  waitCatchSTM = Pool.waitCatchSTM . _underlyingHandle
  cancelWith (SynthesisTaskHandle handle) e = Pool.cancelWith e handle

actionWithTimeout ::
  (Typeable symProg, Typeable conProg) =>
  Maybe Int ->
  TMVar (SynthesisTaskHandle symProg conProg) ->
  IO ([Example symProg], SynthesisResult conProg) ->
  IO ([Example symProg], SynthesisResult conProg)
actionWithTimeout maybeTimeout taskHandleTMVar action =
  C.mask $ \restore -> do
    selfHandle <- atomically $ readTMVar taskHandleTMVar
    case maybeTimeout of
      Just timeout -> do
        async $
          threadDelay timeout
            >> cancelWith selfHandle SynthesisTaskTimeout
        return ()
      Nothing -> return ()
    restore action

enqueueActionImpl ::
  (Typeable symProg, Typeable conProg) =>
  Maybe Int ->
  Pool.ThreadPool ->
  Double ->
  IO ([Example symProg], SynthesisResult conProg) ->
  IO (SynthesisTaskHandle symProg conProg)
enqueueActionImpl
  maybeTimeout
  pool
  priority
  action = do
    taskHandleTMVar <- newEmptyTMVarIO
    handle <-
      Pool.newThread pool priority $
        actionWithTimeout maybeTimeout taskHandleTMVar action
    let taskHandle = SynthesisTaskHandle handle
    atomically $ putTMVar taskHandleTMVar taskHandle
    return taskHandle

-- | Add a task to the synthesis server.
enqueueAction ::
  (Typeable symProg, Typeable conProg) =>
  Pool.ThreadPool ->
  Double ->
  IO ([Example symProg], SynthesisResult conProg) ->
  IO (SynthesisTaskHandle symProg conProg)
enqueueAction = enqueueActionImpl Nothing

-- | Add a task to the synthesis server.
enqueueActionWithTimeout ::
  (Typeable symProg, Typeable conProg) =>
  Int ->
  Pool.ThreadPool ->
  Double ->
  IO ([Example symProg], SynthesisResult conProg) ->
  IO (SynthesisTaskHandle symProg conProg)
enqueueActionWithTimeout timeout = enqueueActionImpl (Just timeout)

enqueueMinimalCostTask ::
  (ConfigurableSolver config solver, Typeable symProg, Typeable conProg) =>
  Pool.ThreadPool ->
  config ->
  Double ->
  SynthesisMinimalCostTask symProg conProg ->
  IO (SynthesisTaskHandle symProg conProg)
enqueueMinimalCostTask pool config priority task =
  enqueueAction
    pool
    priority
    (runSynthesisMinimalCostTaskExtractCex config task)

enqueueMinimalCostTaskWithTimeout ::
  (ConfigurableSolver config solver, Typeable symProg, Typeable conProg) =>
  Int ->
  Pool.ThreadPool ->
  config ->
  Double ->
  SynthesisMinimalCostTask symProg conProg ->
  IO (SynthesisTaskHandle symProg conProg)
enqueueMinimalCostTaskWithTimeout timeout pool config priority task =
  enqueueActionWithTimeout
    timeout
    pool
    priority
    (runSynthesisMinimalCostTaskExtractCex config task)

alterTaskIfPendingImpl ::
  (ConfigurableSolver config h, Typeable symProg, Typeable conProg) =>
  Maybe Int ->
  SynthesisTaskHandle symProg conProg ->
  config ->
  SynthesisTask symProg conProg ->
  IO ()
alterTaskIfPendingImpl
  maybeTimeout
  taskHandle@SynthesisTaskHandle {..}
  config
  task = do
    taskHandleTMVar <- newEmptyTMVarIO
    Pool.alterIfPending _underlyingHandle $
      actionWithTimeout
        maybeTimeout
        taskHandleTMVar
        (runSynthesisTaskExtractCex config task)
    atomically $ putTMVar taskHandleTMVar taskHandle

alterTaskIfPending ::
  (ConfigurableSolver config h, Typeable symProg, Typeable conProg) =>
  SynthesisTaskHandle symProg conProg ->
  config ->
  SynthesisTask symProg conProg ->
  IO ()
alterTaskIfPending = alterTaskIfPendingImpl Nothing

alterTaskIfPendingWithTimeout ::
  (ConfigurableSolver config h, Typeable symProg, Typeable conProg) =>
  SynthesisTaskHandle symProg conProg ->
  config ->
  SynthesisTask symProg conProg ->
  Int ->
  IO ()
alterTaskIfPendingWithTimeout handle config task timeout =
  alterTaskIfPendingImpl (Just timeout) handle config task
