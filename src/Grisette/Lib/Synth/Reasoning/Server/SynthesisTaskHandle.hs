{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Grisette.Lib.Synth.Reasoning.Server.SynthesisTaskHandle
  ( SynthesisTaskHandle,
    enqueueAction,
    enqueueActionWithTimeout,
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
import Grisette.Lib.Synth.Reasoning.Server.ThreadPool (ThreadHandle)
import qualified Grisette.Lib.Synth.Reasoning.Server.ThreadPool as Pool
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( SynthesisResult,
    SynthesisTask,
    runSynthesisTask,
  )

newtype SynthesisTaskHandle conProg = SynthesisTaskHandle
  { _underlyingHandle :: ThreadHandle (SynthesisResult conProg)
  }
  deriving newtype (Eq, Hashable)

instance
  (Typeable conProg) =>
  BaseTaskHandle (SynthesisTaskHandle conProg) conProg
  where
  startTimeSTM = Pool.startTimeSTM . _underlyingHandle
  endTimeSTM = Pool.endTimeSTM . _underlyingHandle
  pollSTM = Pool.pollSTM . _underlyingHandle
  waitCatchSTM = Pool.waitCatchSTM . _underlyingHandle
  cancelWith (SynthesisTaskHandle handle) e = do Pool.cancelWith e handle

actionWithTimeout ::
  (Typeable conProg) =>
  Maybe Int ->
  TMVar (SynthesisTaskHandle conProg) ->
  IO (SynthesisResult conProg) ->
  IO (SynthesisResult conProg)
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
  (Typeable conProg) =>
  Maybe Int ->
  Pool.ThreadPool ->
  IO (SynthesisResult conProg) ->
  IO (SynthesisTaskHandle conProg)
enqueueActionImpl
  maybeTimeout
  pool
  action = do
    taskHandleTMVar <- newEmptyTMVarIO
    handle <-
      Pool.newThread pool $
        actionWithTimeout maybeTimeout taskHandleTMVar action
    let taskHandle = SynthesisTaskHandle handle
    atomically $ putTMVar taskHandleTMVar taskHandle
    return taskHandle

-- | Add a task to the synthesis server.
enqueueAction ::
  (Typeable conProg) =>
  Pool.ThreadPool ->
  IO (SynthesisResult conProg) ->
  IO (SynthesisTaskHandle conProg)
enqueueAction = enqueueActionImpl Nothing

-- | Add a task to the synthesis server.
enqueueActionWithTimeout ::
  (Typeable conProg) =>
  Int ->
  Pool.ThreadPool ->
  IO (SynthesisResult conProg) ->
  IO (SynthesisTaskHandle conProg)
enqueueActionWithTimeout timeout = enqueueActionImpl (Just timeout)

-- | Add a task to the synthesis server.
enqueueTask ::
  (ConfigurableSolver config h, Typeable conProg) =>
  Pool.ThreadPool ->
  config ->
  SynthesisTask conProg ->
  IO (SynthesisTaskHandle conProg)
enqueueTask pool config task =
  enqueueAction pool (runSynthesisTask config task)

-- | Add a task to the synthesis server with a timeout.
--
-- This function may result in zombie processes with sbv-10.9 or earlier.
-- See https://github.com/LeventErkok/sbv/pull/691.
enqueueTaskWithTimeout ::
  (ConfigurableSolver config h, Typeable conProg) =>
  Int ->
  Pool.ThreadPool ->
  config ->
  SynthesisTask conProg ->
  IO (SynthesisTaskHandle conProg)
enqueueTaskWithTimeout timeout pool config task =
  enqueueActionWithTimeout timeout pool (runSynthesisTask config task)

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
    Pool.alterIfPending _underlyingHandle $
      actionWithTimeout
        maybeTimeout
        taskHandleTMVar
        (runSynthesisTask config task)
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
