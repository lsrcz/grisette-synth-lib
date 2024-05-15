{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Grisette.Lib.Synth.Reasoning.Server.SynthesisTaskHandle
  ( SynthesisTaskHandle,
    enqueueTask,
    enqueueTaskWithTimeout,
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
import Grisette (ConfigurableSolver)
import Grisette.Lib.Synth.Reasoning.Server.BaseTaskHandle
  ( BaseTaskHandle
      ( cancelWith,
        endTimeSTM,
        pollSTM,
        startTimeSTM,
        waitCatchSTM
      ),
    SynthesisTaskException (SynthesisTaskTimeout),
  )
import Grisette.Lib.Synth.Reasoning.Server.ThreadPool (TaskHandle)
import qualified Grisette.Lib.Synth.Reasoning.Server.ThreadPool as Pool
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( SynthesisResult,
    SynthesisTask,
    runSynthesisTask,
  )

newtype SynthesisTaskHandle conProg = SynthesisTaskHandle
  { _underlyingHandle :: TaskHandle (SynthesisResult conProg)
  }
  deriving newtype (Eq, Hashable)

instance
  (Typeable conProg) =>
  BaseTaskHandle (SynthesisTaskHandle conProg) conProg
  where
  startTimeSTM = Pool.startTimeSTM . _underlyingHandle
  endTimeSTM = Pool.endTimeSTM . _underlyingHandle
  pollSTM = Pool.pollTaskSTM . _underlyingHandle
  waitCatchSTM = Pool.waitCatchTaskSTM . _underlyingHandle
  cancelWith (SynthesisTaskHandle handle) e = do Pool.cancelTaskWith e handle

taskFun ::
  (ConfigurableSolver config h, Typeable conProg) =>
  Maybe Int ->
  TMVar (SynthesisTaskHandle conProg) ->
  config ->
  SynthesisTask conProg ->
  IO (SynthesisResult conProg)
taskFun maybeTimeout taskHandleTMVar config task =
  C.mask $ \restore -> do
    selfHandle <- atomically $ readTMVar taskHandleTMVar
    case maybeTimeout of
      Just timeout -> do
        async $
          threadDelay timeout
            >> cancelWith selfHandle SynthesisTaskTimeout
        return ()
      Nothing -> return ()
    restore (runSynthesisTask config task)

enqueueTaskImpl ::
  (ConfigurableSolver config h, Typeable conProg) =>
  Maybe Int ->
  Pool.Pool ->
  config ->
  SynthesisTask conProg ->
  IO (SynthesisTaskHandle conProg)
enqueueTaskImpl
  maybeTimeout
  pool
  config
  task = do
    taskHandleTMVar <- newEmptyTMVarIO
    handle <-
      Pool.addNewTask pool $
        taskFun maybeTimeout taskHandleTMVar config task
    let taskHandle = SynthesisTaskHandle handle
    atomically $ putTMVar taskHandleTMVar taskHandle
    return taskHandle

-- | Add a task to the synthesis server.
enqueueTask ::
  (ConfigurableSolver config h, Typeable conProg) =>
  Pool.Pool ->
  config ->
  SynthesisTask conProg ->
  IO (SynthesisTaskHandle conProg)
enqueueTask = enqueueTaskImpl Nothing

-- | Add a task to the synthesis server with a timeout.
--
-- This function may result in zombie processes with sbv-10.9 or earlier.
-- See https://github.com/LeventErkok/sbv/pull/691.
enqueueTaskWithTimeout ::
  (ConfigurableSolver config h, Typeable conProg) =>
  Pool.Pool ->
  config ->
  Int ->
  SynthesisTask conProg ->
  IO (SynthesisTaskHandle conProg)
enqueueTaskWithTimeout server config timeout =
  enqueueTaskImpl (Just timeout) server config

alterTaskIfPendingImpl ::
  (ConfigurableSolver config h, Typeable conProg) =>
  Maybe Int ->
  SynthesisTaskHandle conProg ->
  config ->
  SynthesisTask conProg ->
  IO ()
alterTaskIfPendingImpl
  maybeTimeout
  taskHandle@SynthesisTaskHandle {..}
  config
  task = do
    taskHandleTMVar <- newEmptyTMVarIO
    Pool.alterTaskIfPending _underlyingHandle $
      taskFun
        maybeTimeout
        taskHandleTMVar
        config
        task
    atomically $ putTMVar taskHandleTMVar taskHandle

alterTaskIfPending ::
  (ConfigurableSolver config h, Typeable conProg) =>
  SynthesisTaskHandle conProg ->
  config ->
  SynthesisTask conProg ->
  IO ()
alterTaskIfPending = alterTaskIfPendingImpl Nothing

alterTaskIfPendingWithTimeout ::
  (ConfigurableSolver config h, Typeable conProg) =>
  SynthesisTaskHandle conProg ->
  config ->
  SynthesisTask conProg ->
  Int ->
  IO ()
alterTaskIfPendingWithTimeout handle config task timeout =
  alterTaskIfPendingImpl (Just timeout) handle config task
