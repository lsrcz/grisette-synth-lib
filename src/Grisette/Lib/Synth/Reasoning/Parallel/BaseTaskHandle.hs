{-# LANGUAGE FunctionalDependencies #-}

module Grisette.Lib.Synth.Reasoning.Parallel.BaseTaskHandle
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
import Grisette (ConfigurableSolver)
import Grisette.Lib.Synth.Reasoning.Parallel.Exception
  ( SynthesisTaskException (SynthesisTaskCancelled),
  )
import qualified Grisette.Lib.Synth.Reasoning.Parallel.ThreadPool as Pool
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( RunSynthesisTask,
    SomeExample,
    SynthesisResult,
  )

class
  (Eq handle, Hashable handle) =>
  BaseTaskHandle handle symProg conProg
    | handle -> symProg conProg
  where
  enqueueTaskMaybeTimeout ::
    (ConfigurableSolver config solver, RunSynthesisTask task symProg conProg) =>
    Maybe Int ->
    Pool.ThreadPool ->
    config ->
    Double ->
    IO task ->
    IO handle
  startTimeSTM :: handle -> STM UTCTime
  endTimeSTM :: handle -> STM UTCTime
  elapsedTimeSTM :: handle -> STM NominalDiffTime
  elapsedTimeSTM task = do
    startTime <- startTimeSTM task
    endTime <- endTimeSTM task
    return $ endTime `diffUTCTime` startTime
  pollSTM ::
    handle ->
    STM
      ( Maybe
          (Either C.SomeException ([SomeExample symProg conProg], SynthesisResult conProg))
      )
  waitCatchSTM ::
    handle ->
    STM (Either C.SomeException ([SomeExample symProg conProg], SynthesisResult conProg))
  cancelWith :: (C.Exception e) => handle -> e -> IO ()

startTime :: (BaseTaskHandle task symProg conProg) => task -> IO UTCTime
startTime = atomically . startTimeSTM

endTime :: (BaseTaskHandle task symProg conProg) => task -> IO UTCTime
endTime = atomically . endTimeSTM

elapsedTime ::
  (BaseTaskHandle task symProg conProg) => task -> IO NominalDiffTime
elapsedTime = atomically . elapsedTimeSTM

maybeStartTimeSTM ::
  (BaseTaskHandle task symProg conProg) => task -> STM (Maybe UTCTime)
maybeStartTimeSTM task =
  (Just <$> startTimeSTM task) `orElse` return Nothing

maybeEndTimeSTM ::
  (BaseTaskHandle task symProg conProg) => task -> STM (Maybe UTCTime)
maybeEndTimeSTM task =
  (Just <$> endTimeSTM task) `orElse` return Nothing

maybeElapsedTimeSTM ::
  (BaseTaskHandle task symProg conProg) => task -> STM (Maybe NominalDiffTime)
maybeElapsedTimeSTM task =
  (Just <$> elapsedTimeSTM task) `orElse` return Nothing

maybeStartTime ::
  (BaseTaskHandle task symProg conProg) => task -> IO (Maybe UTCTime)
maybeStartTime = atomically . maybeStartTimeSTM

maybeEndTime ::
  (BaseTaskHandle task symProg conProg) => task -> IO (Maybe UTCTime)
maybeEndTime = atomically . maybeEndTimeSTM

maybeElapsedTime ::
  (BaseTaskHandle task symProg conProg) => task -> IO (Maybe NominalDiffTime)
maybeElapsedTime = atomically . maybeElapsedTimeSTM

poll ::
  (BaseTaskHandle task symProg conProg) =>
  task ->
  IO
    ( Maybe
        ( Either
            C.SomeException
            ([SomeExample symProg conProg], SynthesisResult conProg)
        )
    )
poll = atomically . pollSTM

waitCatch ::
  (BaseTaskHandle task symProg conProg) =>
  task ->
  IO (Either C.SomeException ([SomeExample symProg conProg], SynthesisResult conProg))
waitCatch = atomically . waitCatchSTM

cancel :: (BaseTaskHandle task symProg conProg) => task -> IO ()
cancel task = cancelWith task SynthesisTaskCancelled

pollAnySTM ::
  (BaseTaskHandle task symProg conProg) =>
  [task] ->
  STM
    ( [task],
      [ ( task,
          Either
            C.SomeException
            ([SomeExample symProg conProg], SynthesisResult conProg)
        )
      ]
    )
pollAnySTM tasks = do
  results <- mapM pollSTM tasks
  let zipped = zip tasks results
  let unfinished = [task | (task, Nothing) <- zipped]
  let finished = [(task, result) | (task, Just result) <- zipped]
  return (unfinished, finished)

pollAny ::
  (BaseTaskHandle task symProg conProg) =>
  [task] ->
  IO
    ( [task],
      [ ( task,
          Either
            C.SomeException
            ([SomeExample symProg conProg], SynthesisResult conProg)
        )
      ]
    )
pollAny = atomically . pollAnySTM

enqueueTask ::
  ( ConfigurableSolver config solver,
    BaseTaskHandle handle symProg conProg,
    RunSynthesisTask task symProg conProg
  ) =>
  Pool.ThreadPool ->
  config ->
  Double ->
  IO task ->
  IO handle
enqueueTask = enqueueTaskMaybeTimeout Nothing

enqueueTaskWithTimeout ::
  ( ConfigurableSolver config solver,
    BaseTaskHandle handle symProg conProg,
    RunSynthesisTask task symProg conProg
  ) =>
  Int ->
  Pool.ThreadPool ->
  config ->
  Double ->
  IO task ->
  IO handle
enqueueTaskWithTimeout timeout = enqueueTaskMaybeTimeout (Just timeout)
