{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Reasoning.Server.BaseTaskHandleTestCommon
  ( baseTaskHandleTestCommon,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception (fromException), SomeException)
import Data.Either (fromRight)
import qualified Data.HashMap.Lazy as HM
import Data.Time (diffUTCTime, getCurrentTime)
import Grisette (precise, z3)
import Grisette.Lib.Synth.Reasoning.Server.BaseTaskHandle
  ( BaseTaskHandle,
    cancel,
    elapsedTime,
    endTime,
    enqueueTask,
    enqueueTaskWithTimeout,
    poll,
    pollAny,
    startTime,
    waitCatch,
  )
import Grisette.Lib.Synth.Reasoning.Server.Exception
  ( SynthesisTaskException (SynthesisTaskCancelled, SynthesisTaskTimeout),
  )
import Grisette.Lib.Synth.Reasoning.Server.ThreadPool (newThreadPool)
import Grisette.Lib.Synth.Reasoning.Synthesis (SynthesisResult)
import Grisette.Lib.Synth.Reasoning.Synthesis.ComponentSketchTest
  ( ConProg,
    fuzzResult,
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
  (BaseTaskHandle handle ConProg) =>
  handle ->
  IO (Either SomeException (SynthesisResult ConProg))
pollUntilFinished handle = do
  r <- poll handle
  case r of
    Just v -> return v
    _ -> threadDelay 100000 >> pollUntilFinished handle

pollTasksUntilFinished ::
  (BaseTaskHandle handle ConProg) =>
  [handle] ->
  IO
    [ ( handle,
        Either SomeException (SynthesisResult ConProg)
      )
    ]
pollTasksUntilFinished taskSet = do
  (remaining, r) <- pollAny taskSet
  if null remaining
    then return r
    else threadDelay 100000 >> (r ++) <$> pollTasksUntilFinished remaining

baseTaskHandleTestCommon ::
  forall handle proxy.
  (BaseTaskHandle handle ConProg) =>
  String ->
  proxy handle ->
  Test
baseTaskHandleTestCommon name _ =
  testGroup
    name
    [ testCase "Concurrently synthesize several programs" $ do
        pool <- newThreadPool 2
        handle0 :: handle <-
          enqueueTask pool (precise z3) $
            task addThenDoubleSpec addThenDoubleGen sharedSketch
        handle1 :: handle <-
          enqueueTask pool (precise z3) $
            task divModTwiceSpec divModTwiceGen sharedSketch
        handle2 :: handle <-
          enqueueTask pool (precise z3) $
            task addThenDoubleReverseSpec addThenDoubleGen sharedSketch
        Right r0 <- pollUntilFinished handle0
        Right r1 <- pollUntilFinished handle1
        Right r2 <- pollUntilFinished handle2
        fuzzResult r0 addThenDoubleGen addThenDoubleSpec
        fuzzResult r1 divModTwiceGen divModTwiceSpec
        fuzzResult r2 addThenDoubleGen addThenDoubleReverseSpec,
      testCase "pollTasks" $ do
        pool <- newThreadPool 2
        handle0 :: handle <-
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
        pool <- newThreadPool 2
        -- The timeout cannot be too short due to
        -- https://github.com/jwiegley/async-pool/issues/31
        -- It cannot be too long, either, otherwise the task may finish before
        -- we can cancel the task.
        handle1 :: handle <-
          enqueueTaskWithTimeout 10000 pool (precise z3) $
            task divModTwiceSpec divModTwiceGen sharedSketch
        r0 <- pollUntilFinished handle1
        case r0 of
          Left e -> fromException e @?= Just SynthesisTaskTimeout
          _ -> fail "Expected TaskTimeout exception.",
      testCase "cancelTask" $ do
        pool <- newThreadPool 2
        handle1 :: handle <-
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
        pool <- newThreadPool 2
        handle :: handle <-
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
        pool <- newThreadPool 2
        handle :: handle <-
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
