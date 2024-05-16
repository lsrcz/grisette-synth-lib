{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Grisette.Lib.Synth.Reasoning.Server.RefinableTaskHandle
  ( RefinableTaskHandle,
    pollAtIndexSTM,
    pollAtIndex,
    waitCatchAtIndexSTM,
    waitCatchAtIndex,
    checkRefinableSolverAlive,
    enqueueRefineableAction,
    enqueueRefineableActionWithTimeout,
    enqueueRefineAction,
    enqueueRefineActionWithTimeout,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM
  ( STM,
    TMVar,
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
import Control.Exception (Exception (toException), mask, throwIO)
import qualified Control.Exception as C
import Data.Foldable (traverse_)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Typeable (Typeable)
import Grisette
  ( ConfigurableSolver (newSolver),
    Solvable (con),
    Solver (solverForceTerminate, solverSolve),
  )
import Grisette.Lib.Synth.Reasoning.Server.BaseTaskHandle
  ( BaseTaskHandle
      ( cancelWith,
        elapsedTimeSTM,
        endTimeSTM,
        enqueueTaskMaybeTimeout,
        pollSTM,
        startTimeSTM,
        waitCatchSTM
      ),
  )
import Grisette.Lib.Synth.Reasoning.Server.Exception
  ( SynthesisTaskException
      ( SynthesisTaskIndexOutOfBounds,
        SynthesisTaskSolverDead,
        SynthesisTaskTimeout
      ),
  )
import Grisette.Lib.Synth.Reasoning.Server.ThreadPool
  ( ThreadHandle,
    threadId,
    threadPool,
  )
import qualified Grisette.Lib.Synth.Reasoning.Server.ThreadPool as Pool
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( SynthesisResult (SynthesisSuccess),
    solverRunRefinableSynthesisTask,
  )

data RefinableTaskHandle conProg where
  RefinableTaskHandle ::
    (Solver solver) =>
    { _initialThreadHandleId :: Int,
      _underlyingHandles :: TVar [ThreadHandle (SynthesisResult conProg)],
      _maxSucceedIndex :: TVar Int,
      _solverHandle :: TMVar solver
    } ->
    RefinableTaskHandle conProg

instance Eq (RefinableTaskHandle conProg) where
  RefinableTaskHandle id1 _ _ _ == RefinableTaskHandle id2 _ _ _ =
    id1 == id2

instance Hashable (RefinableTaskHandle conProg) where
  hashWithSalt salt (RefinableTaskHandle id _ _ _) = hashWithSalt salt id

instance
  (Typeable conProg) =>
  BaseTaskHandle (RefinableTaskHandle conProg) conProg
  where
  enqueueTaskMaybeTimeout timeout pool config task =
    enqueueRefinableActionMaybeTimeout
      timeout
      pool
      config
      (`solverRunRefinableSynthesisTask` task)
  startTimeSTM RefinableTaskHandle {..} =
    readTVar _underlyingHandles >>= Pool.startTimeSTM . head
  endTimeSTM RefinableTaskHandle {..} =
    readTVar _underlyingHandles >>= Pool.endTimeSTM . last
  elapsedTimeSTM RefinableTaskHandle {..} =
    sum <$> (readTVar _underlyingHandles >>= mapM Pool.elapsedTimeSTM)
  pollSTM RefinableTaskHandle {..} = do
    lastFinished <- Pool.pollSTM . last =<< readTVar _underlyingHandles
    case lastFinished of
      Nothing -> return Nothing
      _ -> do
        bestIndex <- max 0 <$> readTVar _maxSucceedIndex
        Pool.pollSTM . (!! bestIndex) =<< readTVar _underlyingHandles
  waitCatchSTM RefinableTaskHandle {..} = do
    Pool.waitCatchSTM . last =<< readTVar _underlyingHandles
    bestIndex <- max 0 <$> readTVar _maxSucceedIndex
    Pool.waitCatchSTM . (!! bestIndex) =<< readTVar _underlyingHandles
  cancelWith RefinableTaskHandle {..} e = mask $ \_ -> do
    handles <- readTVarIO _underlyingHandles
    solverHandle <- atomically $ tryTakeTMVar _solverHandle
    mapM_ (Pool.cancelWith e) handles
    traverse_ solverForceTerminate solverHandle

pollAtIndexSTM ::
  (Typeable conProg) =>
  RefinableTaskHandle conProg ->
  Int ->
  STM (Maybe (Either C.SomeException (SynthesisResult conProg)))
pollAtIndexSTM RefinableTaskHandle {..} index = do
  handles <- readTVar _underlyingHandles
  if index >= length handles || index < 0
    then return $ Just $ Left $ toException SynthesisTaskIndexOutOfBounds
    else Pool.pollSTM (handles !! index)

pollAtIndex ::
  (Typeable conProg) =>
  RefinableTaskHandle conProg ->
  Int ->
  IO (Maybe (Either C.SomeException (SynthesisResult conProg)))
pollAtIndex handle index = atomically $ pollAtIndexSTM handle index

waitCatchAtIndexSTM ::
  (Typeable conProg) =>
  RefinableTaskHandle conProg ->
  Int ->
  STM (Either C.SomeException (SynthesisResult conProg))
waitCatchAtIndexSTM RefinableTaskHandle {..} index = do
  handles <- readTVar _underlyingHandles
  if index >= length handles || index < 0
    then return $ Left $ toException SynthesisTaskIndexOutOfBounds
    else Pool.waitCatchSTM (handles !! index)

waitCatchAtIndex ::
  (Typeable conProg) =>
  RefinableTaskHandle conProg ->
  Int ->
  IO (Either C.SomeException (SynthesisResult conProg))
waitCatchAtIndex handle index = atomically $ waitCatchAtIndexSTM handle index

checkRefinableSolverAlive ::
  RefinableTaskHandle conProg ->
  IO Bool
checkRefinableSolverAlive RefinableTaskHandle {..} = do
  solverHandle <- atomically $ tryReadTMVar _solverHandle
  case solverHandle of
    Just solver -> do
      r <- solverSolve solver (con True)
      case r of
        Right _ -> return True
        _ -> do
          solverForceTerminate solver
          atomically $ tryTakeTMVar _solverHandle
          return False
    Nothing -> return False

withAliveSolver ::
  RefinableTaskHandle conProg ->
  (forall solver. (Solver solver) => solver -> IO a) ->
  IO a
withAliveSolver handle@RefinableTaskHandle {..} action = do
  checkRefinableSolverAlive handle
  atomically (tryReadTMVar _solverHandle) >>= \case
    Just solver -> do
      res <- action solver
      checkRefinableSolverAlive handle
      return res
    Nothing -> throwIO SynthesisTaskSolverDead

actionWithTimeout ::
  (Solver solver, Typeable conProg) =>
  Maybe Int ->
  TMVar (RefinableTaskHandle conProg) ->
  Int ->
  TVar Int ->
  solver ->
  (solver -> IO (SynthesisResult conProg)) ->
  IO (SynthesisResult conProg)
actionWithTimeout
  maybeTimeout
  taskHandleTMVar
  currentIndex
  maxFinishedIndex
  solver
  action =
    C.mask $ \restore -> do
      selfHandle <- atomically $ readTMVar taskHandleTMVar
      case maybeTimeout of
        Just timeout -> do
          async $
            threadDelay timeout
              >> cancelWith selfHandle SynthesisTaskTimeout
          return ()
        Nothing -> return ()
      r <- restore (action solver)
      case r of
        SynthesisSuccess _ ->
          atomically $ writeTVar maxFinishedIndex currentIndex
        _ -> return ()
      return r

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
  _maxSucceedIndex <- newTVarIO (-1)
  handle <-
    Pool.newThread pool $
      actionWithTimeout
        maybeTimeout
        taskHandleTMVar
        0
        _maxSucceedIndex
        solverHandle
        action
  _underlyingHandles <- newTVarIO [handle]
  let taskHandle =
        RefinableTaskHandle
          { _initialThreadHandleId = threadId handle,
            ..
          }
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
    oldHandles <- readTVarIO _underlyingHandles
    let lastHandle = last oldHandles
    taskHandleTMVar <- newEmptyTMVarIO
    handle <-
      Pool.newThread (threadPool lastHandle) $ do
        Pool.waitCatch lastHandle
        withAliveSolver taskHandle $ \solver ->
          actionWithTimeout
            maybeTimeout
            taskHandleTMVar
            (length oldHandles)
            _maxSucceedIndex
            solver
            action
    atomically $ writeTVar _underlyingHandles $ oldHandles ++ [handle]
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
