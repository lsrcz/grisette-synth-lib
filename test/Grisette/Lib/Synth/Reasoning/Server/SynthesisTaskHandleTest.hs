module Grisette.Lib.Synth.Reasoning.Server.SynthesisTaskHandleTest
  ( synthesisTaskHandleTest,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception (fromException), SomeException)
import Data.Either (fromRight)
import qualified Data.HashMap.Lazy as HM
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Typeable (Typeable)
import Grisette (z3)
import Grisette.Backend (precise)
import Grisette.Lib.Synth.Reasoning.Server.BaseTaskHandle
  ( SynthesisTaskException (SynthesisTaskCancelled, SynthesisTaskTimeout),
    cancel,
    elapsedTime,
    endTime,
    poll,
    pollAny,
    startTime,
    waitCatch,
  )
import Grisette.Lib.Synth.Reasoning.Server.SynthesisTaskHandle
  ( SynthesisTaskHandle,
    enqueueTask,
    enqueueTaskWithTimeout,
  )
import Grisette.Lib.Synth.Reasoning.Server.ThreadPool (newPool)
import Grisette.Lib.Synth.Reasoning.Synthesis (SynthesisResult)
import Grisette.Lib.Synth.Reasoning.Synthesis.ComponentSketchTest
  ( fuzzResult,
    sharedSketch,
    task,
  )
import Grisette.Lib.Synth.Reasoning.Synthesis.Problem
  ( addThenDoubleGen,
    addThenDoubleReverseSpec,
    addThenDoubleSpec,
    divModTwiceGen,
    divModTwiceSpec,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool, (@?=))

pollUntilFinished ::
  (Typeable conProg) =>
  SynthesisTaskHandle conProg ->
  IO (Either SomeException (SynthesisResult conProg))
pollUntilFinished handle = do
  r <- poll handle
  case r of
    Just v -> return v
    _ -> threadDelay 100000 >> pollUntilFinished handle

pollTasksUntilFinished ::
  (Typeable conProg) =>
  [SynthesisTaskHandle conProg] ->
  IO
    [ ( SynthesisTaskHandle conProg,
        Either SomeException (SynthesisResult conProg)
      )
    ]
pollTasksUntilFinished taskSet = do
  (remaining, r) <- pollAny taskSet
  if null remaining
    then return r
    else threadDelay 100000 >> (r ++) <$> pollTasksUntilFinished remaining

synthesisTaskHandleTest :: Test
synthesisTaskHandleTest =
  testGroup
    "SynthesisTaskHandle"
    [ testCase "Concurrently synthesize several programs" $ do
        pool <- newPool 2
        handle0 <-
          enqueueTask pool (precise z3) $
            task addThenDoubleSpec addThenDoubleGen sharedSketch
        handle1 <-
          enqueueTask pool (precise z3) $
            task divModTwiceSpec divModTwiceGen sharedSketch
        handle2 <-
          enqueueTask pool (precise z3) $
            task addThenDoubleReverseSpec addThenDoubleGen sharedSketch
        Right r0 <- pollUntilFinished handle0
        Right r1 <- pollUntilFinished handle1
        Right r2 <- pollUntilFinished handle2
        fuzzResult r0 addThenDoubleGen addThenDoubleSpec
        fuzzResult r1 divModTwiceGen divModTwiceSpec
        fuzzResult r2 addThenDoubleGen addThenDoubleReverseSpec,
      testCase "pollTasks" $ do
        pool <- newPool 2
        handle0 <-
          enqueueTask pool (precise z3) $
            task addThenDoubleSpec addThenDoubleGen sharedSketch
        handle1 <-
          enqueueTask pool (precise z3) $
            task divModTwiceSpec divModTwiceGen sharedSketch
        map <- HM.fromList <$> pollTasksUntilFinished [handle0, handle1]
        fuzzResult
          (fromRight undefined $ map HM.! handle0)
          addThenDoubleGen
          addThenDoubleSpec
        fuzzResult
          (fromRight undefined $ map HM.! handle1)
          divModTwiceGen
          divModTwiceSpec,
      testCase "enqueueTaskWithTimeout" $ do
        pool <- newPool 2
        -- The timeout cannot be too short due to
        -- https://github.com/jwiegley/async-pool/issues/31
        -- It cannot be too long, either, otherwise the task may finish before
        -- we can cancel the task.
        handle1 <-
          enqueueTaskWithTimeout pool (precise z3) 10000 $
            task divModTwiceSpec divModTwiceGen sharedSketch
        r0 <- pollUntilFinished handle1
        case r0 of
          Left e -> fromException e @?= Just SynthesisTaskTimeout
          _ -> fail "Expected TaskTimeout exception.",
      testCase "cancelTask" $ do
        pool <- newPool 2
        handle1 <-
          enqueueTask pool (precise z3) $
            task divModTwiceSpec divModTwiceGen sharedSketch
        -- The delay is necessary due to
        -- https://github.com/jwiegley/async-pool/issues/31
        -- It cannot be too long, either, otherwise the task may finish before
        -- we can cancel the task.
        threadDelay 10000
        cancel handle1
        r0 <- pollUntilFinished handle1
        case r0 of
          Left e -> fromException e @?= Just SynthesisTaskCancelled
          _ -> fail "Expected TaskCancelled exception.",
      testCase "time measurement" $ do
        pool <- newPool 2
        handle <-
          enqueueTask pool (precise z3) $
            task divModTwiceSpec divModTwiceGen sharedSketch
        _ <- waitCatch handle
        startTime <- startTime handle
        expectedEndTime <- getCurrentTime
        let expectedElapsedTime = diffUTCTime expectedEndTime startTime
        elapsedTime <- elapsedTime handle
        endTime <- endTime handle
        assertBool "Diff should be less than 0.3 second" $
          abs (expectedElapsedTime - elapsedTime) < 0.3
        assertBool "End time diff should be less than 0.3 second" $
          abs (diffUTCTime endTime expectedEndTime) < 0.3,
      testCase "time measurement for cancelled tasks" $ do
        pool <- newPool 2
        handle <-
          enqueueTask pool (precise z3) $
            task divModTwiceSpec divModTwiceGen sharedSketch
        threadDelay 100000
        cancel handle
        startTime <- startTime handle
        expectedEndTime <- getCurrentTime
        let expectedElapsedTime = diffUTCTime expectedEndTime startTime
        elapsedTime <- elapsedTime handle
        endTime <- endTime handle
        assertBool "Diff should be less than 0.3 second" $
          abs (expectedElapsedTime - elapsedTime) < 0.3
        assertBool "End time diff should be less than 0.3 second" $
          abs (diffUTCTime endTime expectedEndTime) < 0.3
    ]
