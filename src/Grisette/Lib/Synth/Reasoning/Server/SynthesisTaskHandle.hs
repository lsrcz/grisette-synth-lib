{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
    tryPutTMVar,
  )
import qualified Control.Exception as C
import Data.Hashable (Hashable (hashWithSalt))
import Data.Time (UTCTime, getCurrentTime)
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

data SynthesisTaskHandle conProg where
  SynthesisTaskHandle ::
    { _underlyingHandle :: TaskHandle (SynthesisResult conProg),
      _taskStartTime :: TMVar UTCTime,
      _taskEndTime :: TMVar UTCTime
    } ->
    SynthesisTaskHandle conProg

instance Eq (SynthesisTaskHandle conProg) where
  SynthesisTaskHandle handle1 _ _ == SynthesisTaskHandle handle2 _ _ =
    handle1 == handle2

instance Hashable (SynthesisTaskHandle conProg) where
  hashWithSalt salt (SynthesisTaskHandle handle _ _) =
    hashWithSalt salt handle

instance
  (Typeable conProg) =>
  BaseTaskHandle (SynthesisTaskHandle conProg) conProg
  where
  startTimeSTM = readTMVar . _taskStartTime
  endTimeSTM = readTMVar . _taskEndTime
  pollSTM = Pool.pollTaskSTM . _underlyingHandle
  waitCatchSTM = Pool.waitCatchTaskSTM . _underlyingHandle
  cancelWith
    (SynthesisTaskHandle handle startTime endTime)
    e = do
      Pool.cancelTaskWith e handle
      currentTime <- getCurrentTime
      atomically $ do
        tryPutTMVar startTime currentTime
        tryPutTMVar endTime currentTime
      return ()

taskFun ::
  (ConfigurableSolver config h, Typeable conProg) =>
  Maybe Int ->
  TMVar UTCTime ->
  TMVar UTCTime ->
  TMVar (SynthesisTaskHandle conProg) ->
  config ->
  SynthesisTask conProg ->
  IO (SynthesisResult conProg)
taskFun maybeTimeout startTimeTMVar endTimeTMVar taskHandleTMVar config task =
  C.mask $ \restore -> do
    selfHandle <- atomically $ readTMVar taskHandleTMVar
    case maybeTimeout of
      Just timeout -> do
        async $
          threadDelay timeout
            >> cancelWith selfHandle SynthesisTaskTimeout
        return ()
      Nothing -> return ()
    getCurrentTime >>= atomically . tryPutTMVar startTimeTMVar
    restore (runSynthesisTask config task)
      `C.finally` (getCurrentTime >>= atomically . tryPutTMVar endTimeTMVar)

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
    startTime <- newEmptyTMVarIO
    endTime <- newEmptyTMVarIO
    taskHandleTMVar <- newEmptyTMVarIO
    handle <-
      Pool.addNewTask pool $
        taskFun maybeTimeout startTime endTime taskHandleTMVar config task
    let taskHandle = SynthesisTaskHandle handle startTime endTime
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
        _taskStartTime
        _taskEndTime
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
