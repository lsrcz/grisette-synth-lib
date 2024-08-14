{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Grisette.Lib.Synth.Reasoning.Parallel.RefinableTaskHandle
  ( RefinableTaskHandle,
    pollAtIndexSTM,
    pollAtIndex,
    waitCatchAtIndexSTM,
    waitCatchAtIndex,
    checkRefinableSolverAlive,
    enqueueRefineCond,
    enqueueRefineCondWithTimeout,
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
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Typeable (Typeable)
import Grisette
  ( ConfigurableSolver (newSolver),
    Solvable (con),
    Solver (solverAssert, solverForceTerminate),
    solverSolve,
  )
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Lib.Synth.Reasoning.Parallel.BaseTaskHandle
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
import Grisette.Lib.Synth.Reasoning.Parallel.Exception
  ( SynthesisTaskException
      ( SynthesisTaskIndexOutOfBounds,
        SynthesisTaskSolverDead,
        SynthesisTaskTimeout
      ),
  )
import Grisette.Lib.Synth.Reasoning.Parallel.ThreadPool
  ( ThreadHandle,
    threadId,
    threadPool,
  )
import qualified Grisette.Lib.Synth.Reasoning.Parallel.ThreadPool as Pool
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( Example,
    RunSynthesisTask (solverRunSynthesisTaskExtractCex, taskRefinable),
    SynthesisResult (SynthesisSuccess),
  )

data RefinableTaskHandle symProg conProg where
  RefinableTaskHandle ::
    (Solver solver, RunSynthesisTask task symProg conProg) =>
    { _initialThreadHandleId :: Int,
      _initialSynthesisTask :: TMVar task,
      _underlyingHandles ::
        TVar [ThreadHandle ([Example symProg], SynthesisResult conProg)],
      _maxSucceedIndex :: TVar Int,
      _solverHandle :: TMVar solver
    } ->
    RefinableTaskHandle symProg conProg

instance Eq (RefinableTaskHandle symProg conProg) where
  RefinableTaskHandle id1 _ _ _ _ == RefinableTaskHandle id2 _ _ _ _ =
    id1 == id2

instance Hashable (RefinableTaskHandle symProg conProg) where
  hashWithSalt salt (RefinableTaskHandle id _ _ _ _) = hashWithSalt salt id

instance
  (Typeable symProg, Typeable conProg) =>
  BaseTaskHandle (RefinableTaskHandle symProg conProg) symProg conProg
  where
  enqueueTaskMaybeTimeout
    maybeTimeout
    pool
    config
    priority
    ioTask = do
      solverHandle <- newSolver config
      _solverHandle <- newTMVarIO solverHandle
      taskHandleTMVar <- newEmptyTMVarIO
      _maxSucceedIndex <- newTVarIO (-1)
      _initialSynthesisTask <- newEmptyTMVarIO
      handle <-
        Pool.newThread pool priority $
          actionWithTimeout
            maybeTimeout
            taskHandleTMVar
            0
            _maxSucceedIndex
            solverHandle
            ( \solver -> do
                task <- ioTask
                unless (taskRefinable task) $
                  fail "Task is not refinable"
                atomically $ putTMVar _initialSynthesisTask task
                solverRunSynthesisTaskExtractCex solver task
            )
      _underlyingHandles <- newTVarIO [handle]
      let taskHandle =
            RefinableTaskHandle
              { _initialThreadHandleId = threadId handle,
                ..
              }
      atomically $ putTMVar taskHandleTMVar taskHandle
      return taskHandle
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
  (Typeable symProg, Typeable conProg) =>
  RefinableTaskHandle symProg conProg ->
  Int ->
  STM
    ( Maybe
        (Either C.SomeException ([Example symProg], SynthesisResult conProg))
    )
pollAtIndexSTM RefinableTaskHandle {..} index = do
  handles <- readTVar _underlyingHandles
  if index >= length handles || index < 0
    then return $ Just $ Left $ toException SynthesisTaskIndexOutOfBounds
    else Pool.pollSTM (handles !! index)

pollAtIndex ::
  (Typeable symProg, Typeable conProg) =>
  RefinableTaskHandle symProg conProg ->
  Int ->
  IO
    ( Maybe
        (Either C.SomeException ([Example symProg], SynthesisResult conProg))
    )
pollAtIndex handle index = atomically $ pollAtIndexSTM handle index

waitCatchAtIndexSTM ::
  (Typeable symProg, Typeable conProg) =>
  RefinableTaskHandle symProg conProg ->
  Int ->
  STM (Either C.SomeException ([Example symProg], SynthesisResult conProg))
waitCatchAtIndexSTM RefinableTaskHandle {..} index = do
  handles <- readTVar _underlyingHandles
  if index >= length handles || index < 0
    then return $ Left $ toException SynthesisTaskIndexOutOfBounds
    else Pool.waitCatchSTM (handles !! index)

waitCatchAtIndex ::
  (Typeable symProg, Typeable conProg) =>
  RefinableTaskHandle symProg conProg ->
  Int ->
  IO (Either C.SomeException ([Example symProg], SynthesisResult conProg))
waitCatchAtIndex handle index = atomically $ waitCatchAtIndexSTM handle index

checkRefinableSolverAlive :: RefinableTaskHandle symProg conProg -> IO Bool
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
  RefinableTaskHandle symProg conProg ->
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
  (Solver solver, Typeable symProg, Typeable conProg) =>
  Maybe Int ->
  TMVar (RefinableTaskHandle symProg conProg) ->
  Int ->
  TVar Int ->
  solver ->
  (solver -> IO ([Example symProg], SynthesisResult conProg)) ->
  IO ([Example symProg], SynthesisResult conProg)
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
        (_, SynthesisSuccess _) ->
          atomically $ writeTVar maxFinishedIndex currentIndex
        _ -> return ()
      return r

enqueueRefineCondMaybeTimeout ::
  (Typeable symProg, Typeable conProg) =>
  Maybe Int ->
  Double ->
  RefinableTaskHandle symProg conProg ->
  IO SymBool ->
  IO ()
enqueueRefineCondMaybeTimeout
  maybeTimeout
  priority
  taskHandle@RefinableTaskHandle {..}
  cond = do
    oldHandles <- readTVarIO _underlyingHandles
    let lastHandle = last oldHandles
    taskHandleTMVar <- newEmptyTMVarIO
    handle <-
      Pool.newChildThread
        (threadPool lastHandle)
        [Pool.threadId lastHandle]
        priority
        $ do
          withAliveSolver taskHandle $ \solver ->
            actionWithTimeout
              maybeTimeout
              taskHandleTMVar
              (length oldHandles)
              _maxSucceedIndex
              solver
              ( \solver -> do
                  solverAssert solver =<< cond
                  task <- atomically $ readTMVar _initialSynthesisTask
                  solverRunSynthesisTaskExtractCex
                    solver
                    task
              )
    atomically $ writeTVar _underlyingHandles $ oldHandles ++ [handle]
    atomically $ putTMVar taskHandleTMVar taskHandle

enqueueRefineCond ::
  (Typeable symProg, Typeable conProg) =>
  Double ->
  RefinableTaskHandle symProg conProg ->
  IO SymBool ->
  IO ()
enqueueRefineCond = enqueueRefineCondMaybeTimeout Nothing

enqueueRefineCondWithTimeout ::
  (Typeable symProg, Typeable conProg) =>
  Int ->
  Double ->
  RefinableTaskHandle symProg conProg ->
  IO SymBool ->
  IO ()
enqueueRefineCondWithTimeout timeout =
  enqueueRefineCondMaybeTimeout (Just timeout)
