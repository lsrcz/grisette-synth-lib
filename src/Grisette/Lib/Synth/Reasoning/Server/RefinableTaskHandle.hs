{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Grisette.Lib.Synth.Reasoning.Server.RefinableTaskHandle
  ( RefinableTaskHandle,
    enqueueRefineableAction,
    enqueueRefineableActionWithTimeout,
    enqueueRefineAction,
    enqueueRefineActionWithTimeout,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM
  ( TMVar,
    TVar,
    atomically,
    newEmptyTMVarIO,
    newTMVarIO,
    newTVarIO,
    putTMVar,
    readTMVar,
    readTVar,
    readTVarIO,
    tryReadTMVar,
    tryTakeTMVar,
    writeTVar,
  )
import Control.Exception (mask, throwIO)
import qualified Control.Exception as C
import Data.Foldable (traverse_)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Typeable (Typeable)
import Grisette (ConfigurableSolver (newSolver), Solver (solverForceTerminate))
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
  ( SynthesisTaskException (SynthesisTaskCancelled, SynthesisTaskTimeout),
  )
import Grisette.Lib.Synth.Reasoning.Server.ThreadPool
  ( ThreadHandle,
    threadId,
    threadPool,
  )
import qualified Grisette.Lib.Synth.Reasoning.Server.ThreadPool as Pool
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( SynthesisResult,
    solverRunSynthesisTask,
  )

data RefinableTaskHandle conProg where
  RefinableTaskHandle ::
    (Solver solver) =>
    { _initialThreadHandleId :: Int,
      _underlyingHandle :: TVar (ThreadHandle (SynthesisResult conProg)),
      _solverHandle :: TMVar solver
    } ->
    RefinableTaskHandle conProg

instance Eq (RefinableTaskHandle conProg) where
  RefinableTaskHandle id1 _ _ == RefinableTaskHandle id2 _ _ =
    id1 == id2

instance Hashable (RefinableTaskHandle conProg) where
  hashWithSalt salt (RefinableTaskHandle id _ _) = hashWithSalt salt id

instance
  (Typeable conProg) =>
  BaseTaskHandle (RefinableTaskHandle conProg) conProg
  where
  enqueueTaskMaybeTimeout timeout pool config task =
    enqueueRefinableActionMaybeTimeout
      timeout
      pool
      config
      (`solverRunSynthesisTask` task)
  startTimeSTM RefinableTaskHandle {..} =
    readTVar _underlyingHandle >>= Pool.startTimeSTM
  endTimeSTM RefinableTaskHandle {..} =
    readTVar _underlyingHandle >>= Pool.endTimeSTM
  pollSTM RefinableTaskHandle {..} =
    readTVar _underlyingHandle >>= Pool.pollSTM
  waitCatchSTM RefinableTaskHandle {..} =
    readTVar _underlyingHandle >>= Pool.waitCatchSTM
  cancelWith RefinableTaskHandle {..} e = mask $ \_ -> do
    handle <- readTVarIO _underlyingHandle
    solverHandle <- atomically $ tryTakeTMVar _solverHandle
    Pool.cancelWith e handle
    traverse_ solverForceTerminate solverHandle

actionWithTimeout ::
  (Solver solver, Typeable conProg) =>
  Maybe Int ->
  TMVar (RefinableTaskHandle conProg) ->
  solver ->
  (solver -> IO (SynthesisResult conProg)) ->
  IO (SynthesisResult conProg)
actionWithTimeout maybeTimeout taskHandleTMVar solver action =
  C.mask $ \restore -> do
    selfHandle <- atomically $ readTMVar taskHandleTMVar
    case maybeTimeout of
      Just timeout -> do
        async $
          threadDelay timeout
            >> cancelWith selfHandle SynthesisTaskTimeout
        return ()
      Nothing -> return ()
    restore (action solver)

enqueueRefinableActionMaybeTimeout ::
  (ConfigurableSolver config solver, Typeable conProg) =>
  Maybe Int ->
  Pool.ThreadPool ->
  config ->
  (solver -> IO (SynthesisResult conProg)) ->
  IO (RefinableTaskHandle conProg)
enqueueRefinableActionMaybeTimeout maybeTimeout pool config action = do
  solverHandle <- newSolver config
  _solverHandle <- newTMVarIO solverHandle
  taskHandleTMVar <- newEmptyTMVarIO
  handle <-
    Pool.newThread pool $
      actionWithTimeout maybeTimeout taskHandleTMVar solverHandle action
  _handle <- newTVarIO handle
  let taskHandle = RefinableTaskHandle (threadId handle) _handle _solverHandle
  atomically $ putTMVar taskHandleTMVar taskHandle
  return taskHandle

enqueueRefineableAction ::
  (ConfigurableSolver config solver, Typeable conProg) =>
  Pool.ThreadPool ->
  config ->
  (solver -> IO (SynthesisResult conProg)) ->
  IO (RefinableTaskHandle conProg)
enqueueRefineableAction = enqueueRefinableActionMaybeTimeout Nothing

enqueueRefineableActionWithTimeout ::
  (ConfigurableSolver config solver, Typeable conProg) =>
  Int ->
  Pool.ThreadPool ->
  config ->
  (solver -> IO (SynthesisResult conProg)) ->
  IO (RefinableTaskHandle conProg)
enqueueRefineableActionWithTimeout timeout =
  enqueueRefinableActionMaybeTimeout (Just timeout)

enqueueRefineActionMaybeTimeout ::
  (Typeable conProg) =>
  Maybe Int ->
  RefinableTaskHandle conProg ->
  (forall solver. (Solver solver) => solver -> IO (SynthesisResult conProg)) ->
  IO ()
enqueueRefineActionMaybeTimeout
  maybeTimeout
  taskHandle@RefinableTaskHandle {..}
  action = do
    oldHandle <- readTVarIO _underlyingHandle
    taskHandleTMVar <- newEmptyTMVarIO
    handle <-
      Pool.newThread (threadPool oldHandle) $ do
        solverHandle <- atomically $ tryReadTMVar _solverHandle
        case solverHandle of
          Just solver ->
            actionWithTimeout maybeTimeout taskHandleTMVar solver action
          Nothing -> throwIO SynthesisTaskCancelled
    atomically $ writeTVar _underlyingHandle handle
    atomically $ putTMVar taskHandleTMVar taskHandle

enqueueRefineAction ::
  (Typeable conProg) =>
  RefinableTaskHandle conProg ->
  (forall solver. (Solver solver) => solver -> IO (SynthesisResult conProg)) ->
  IO ()
enqueueRefineAction = enqueueRefineActionMaybeTimeout Nothing

enqueueRefineActionWithTimeout ::
  (Typeable conProg) =>
  Int ->
  RefinableTaskHandle conProg ->
  (forall solver. (Solver solver) => solver -> IO (SynthesisResult conProg)) ->
  IO ()
enqueueRefineActionWithTimeout timeout =
  enqueueRefineActionMaybeTimeout (Just timeout)
