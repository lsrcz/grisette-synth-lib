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
        enqueueTaskMaybeTimeout,
        pollSTM,
        startTimeSTM,
        waitCatchSTM
      ),
  )
import Grisette.Lib.Synth.Reasoning.Server.Exception
  ( SynthesisTaskException (SynthesisTaskTimeout),
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
  enqueueTaskMaybeTimeout timeout pool config task =
    enqueueActionImpl timeout pool (runSynthesisTask config task)
  startTimeSTM = Pool.startTimeSTM . _underlyingHandle
  endTimeSTM = Pool.endTimeSTM . _underlyingHandle
  pollSTM = Pool.pollSTM . _underlyingHandle
  waitCatchSTM = Pool.waitCatchSTM . _underlyingHandle
  cancelWith (SynthesisTaskHandle handle) e = Pool.cancelWith e handle

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
