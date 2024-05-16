{-# LANGUAGE FunctionalDependencies #-}

module Grisette.Lib.Synth.Reasoning.Server.BaseTaskHandle
  ( BaseTaskHandle (..),
    startTime,
    endTime,
    elapsedTime,
    maybeStartTime,
    maybeEndTime,
    maybeElapsedTime,
    poll,
    waitCatch,
    cancel,
    pollAnySTM,
    pollAny,
    enqueueTask,
    enqueueTaskWithTimeout,
  )
where

import Control.Concurrent.STM (STM, atomically, orElse)
import qualified Control.Exception as C
import Data.Hashable (Hashable)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import Data.Typeable (Typeable)
import Grisette (ConfigurableSolver)
import Grisette.Lib.Synth.Reasoning.Server.Exception
  ( SynthesisTaskException (SynthesisTaskCancelled),
  )
import qualified Grisette.Lib.Synth.Reasoning.Server.ThreadPool as Pool
import Grisette.Lib.Synth.Reasoning.Synthesis (SynthesisResult, SynthesisTask)

class
  (Eq handle, Hashable handle, Typeable conProg) =>
  BaseTaskHandle handle conProg
    | handle -> conProg
  where
  enqueueTaskMaybeTimeout ::
    (ConfigurableSolver config solver) =>
    Maybe Int ->
    Pool.ThreadPool ->
    config ->
    SynthesisTask conProg ->
    IO handle
  startTimeSTM :: handle -> STM UTCTime
  endTimeSTM :: handle -> STM UTCTime
  elapsedTimeSTM :: handle -> STM NominalDiffTime
  elapsedTimeSTM task = do
    startTime <- startTimeSTM task
    endTime <- endTimeSTM task
    return $ endTime `diffUTCTime` startTime
  pollSTM ::
    handle -> STM (Maybe (Either C.SomeException (SynthesisResult conProg)))
  waitCatchSTM ::
    handle -> STM (Either C.SomeException (SynthesisResult conProg))
  cancelWith :: (C.Exception e) => handle -> e -> IO ()

startTime :: (BaseTaskHandle task conProg) => task -> IO UTCTime
startTime = atomically . startTimeSTM

endTime :: (BaseTaskHandle task conProg) => task -> IO UTCTime
endTime = atomically . endTimeSTM

elapsedTime :: (BaseTaskHandle task conProg) => task -> IO NominalDiffTime
elapsedTime = atomically . elapsedTimeSTM

maybeStartTimeSTM :: (BaseTaskHandle task conProg) => task -> STM (Maybe UTCTime)
maybeStartTimeSTM task =
  (Just <$> startTimeSTM task) `orElse` return Nothing

maybeEndTimeSTM :: (BaseTaskHandle task conProg) => task -> STM (Maybe UTCTime)
maybeEndTimeSTM task =
  (Just <$> endTimeSTM task) `orElse` return Nothing

maybeElapsedTimeSTM ::
  (BaseTaskHandle task conProg) => task -> STM (Maybe NominalDiffTime)
maybeElapsedTimeSTM task =
  (Just <$> elapsedTimeSTM task) `orElse` return Nothing

maybeStartTime :: (BaseTaskHandle task conProg) => task -> IO (Maybe UTCTime)
maybeStartTime = atomically . maybeStartTimeSTM

maybeEndTime :: (BaseTaskHandle task conProg) => task -> IO (Maybe UTCTime)
maybeEndTime = atomically . maybeEndTimeSTM

maybeElapsedTime ::
  (BaseTaskHandle task conProg) => task -> IO (Maybe NominalDiffTime)
maybeElapsedTime = atomically . maybeElapsedTimeSTM

poll ::
  (BaseTaskHandle task conProg) =>
  task ->
  IO (Maybe (Either C.SomeException (SynthesisResult conProg)))
poll = atomically . pollSTM

waitCatch ::
  (BaseTaskHandle task conProg) =>
  task ->
  IO (Either C.SomeException (SynthesisResult conProg))
waitCatch = atomically . waitCatchSTM

cancel :: (BaseTaskHandle task conProg) => task -> IO ()
cancel task = cancelWith task SynthesisTaskCancelled

pollAnySTM ::
  (BaseTaskHandle task conProg) =>
  [task] ->
  STM ([task], [(task, Either C.SomeException (SynthesisResult conProg))])
pollAnySTM tasks = do
  results <- mapM pollSTM tasks
  let zipped = zip tasks results
  let unfinished = [task | (task, Nothing) <- zipped]
  let finished = [(task, result) | (task, Just result) <- zipped]
  return (unfinished, finished)

pollAny ::
  (BaseTaskHandle task conProg) =>
  [task] ->
  IO ([task], [(task, Either C.SomeException (SynthesisResult conProg))])
pollAny = atomically . pollAnySTM

enqueueTask ::
  (ConfigurableSolver config solver, BaseTaskHandle handle conProg) =>
  Pool.ThreadPool ->
  config ->
  SynthesisTask conProg ->
  IO handle
enqueueTask = enqueueTaskMaybeTimeout Nothing

enqueueTaskWithTimeout ::
  (ConfigurableSolver config solver, BaseTaskHandle handle conProg) =>
  Int ->
  Pool.ThreadPool ->
  config ->
  SynthesisTask conProg ->
  IO handle
enqueueTaskWithTimeout timeout = enqueueTaskMaybeTimeout (Just timeout)
