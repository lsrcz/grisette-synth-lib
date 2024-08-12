{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Reasoning.Parallel.BaseTaskHandleTestCommon
  ( baseTaskHandleTestCommon,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
  ( atomically,
    newEmptyTMVarIO,
    putTMVar,
    readTMVar,
  )
import Control.Exception (Exception (fromException), SomeException)
import Data.Either (fromRight)
import qualified Data.HashMap.Lazy as HM
import Data.Time (diffUTCTime, getCurrentTime)
import Grisette (SymEq ((.==)), z3)
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Grisette.Lib.Synth.Context (SymbolicContext)
import Grisette.Lib.Synth.Program.CostModel.PerStmtCostModel
  ( PerStmtCostObj (PerStmtCostObj),
  )
import Grisette.Lib.Synth.Program.ProgCost (ProgCost (progCost))
import Grisette.Lib.Synth.Reasoning.Parallel.BaseTaskHandle
  ( BaseTaskHandle,
    cancel,
    elapsedTime,
    endTime,
    enqueueTask,
    enqueueTaskPrecond,
    enqueueTaskWithTimeout,
    poll,
    pollAny,
    startTime,
    waitCatch,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Exception
  ( SynthesisTaskException (SynthesisTaskCancelled, SynthesisTaskTimeout),
  )
import Grisette.Lib.Synth.Reasoning.Parallel.ThreadPool (newThreadPool)
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( Example,
    SynthesisResult (SynthesisSuccess),
  )
import Grisette.Lib.Synth.Reasoning.Synthesis.ComponentSketchTest
  ( ConProg,
    SymProg,
    fuzzResult,
    sharedSketch,
    task,
    times4Sketch,
  )
import Grisette.Lib.Synth.Reasoning.Synthesis.Problem
  ( addThenDoubleGen,
    addThenDoubleReverseSpec,
    addThenDoubleSpec,
    divModTwiceGen,
    divModTwiceSpec,
    times4Gen,
    times4Spec,
  )
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsCost (TestSemanticsCost),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool, (@?=))

pollUntilFinished ::
  (BaseTaskHandle handle SymProg ConProg) =>
  handle ->
  IO (Either SomeException ([Example SymProg], SynthesisResult ConProg))
pollUntilFinished handle = do
  r <- poll handle
  case r of
    Just v -> return v
    _ -> threadDelay 100000 >> pollUntilFinished handle

pollTasksUntilFinished ::
  (BaseTaskHandle handle SymProg ConProg) =>
  [handle] ->
  IO
    [ ( handle,
        Either SomeException ([Example SymProg], SynthesisResult ConProg)
      )
    ]
pollTasksUntilFinished taskSet = do
  (remaining, r) <- pollAny taskSet
  if null remaining
    then return r
    else threadDelay 100000 >> (r ++) <$> pollTasksUntilFinished remaining

baseTaskHandleTestCommon ::
  forall handle proxy.
  (BaseTaskHandle handle SymProg ConProg) =>
  String ->
  proxy handle ->
  Test
baseTaskHandleTestCommon name _ =
  testGroup
    name
    [ testCase "Concurrently synthesize several programs" $ do
        pool <- newThreadPool 2
        handle0 :: handle <-
          enqueueTask pool z3 0 $
            task addThenDoubleSpec addThenDoubleGen [] sharedSketch
        handle1 :: handle <-
          enqueueTask pool z3 0 $
            task divModTwiceSpec divModTwiceGen [] sharedSketch
        handle2 :: handle <-
          enqueueTask pool z3 0 $
            task addThenDoubleReverseSpec addThenDoubleGen [] sharedSketch
        Right (_, r0) <- pollUntilFinished handle0
        Right (_, r1) <- pollUntilFinished handle1
        Right (_, r2) <- pollUntilFinished handle2
        fuzzResult r0 addThenDoubleGen addThenDoubleSpec
        fuzzResult r1 divModTwiceGen divModTwiceSpec
        fuzzResult r2 addThenDoubleGen addThenDoubleReverseSpec,
      testCase "pollTasks" $ do
        pool <- newThreadPool 2
        handle0 :: handle <-
          enqueueTask pool z3 0 $
            task addThenDoubleSpec addThenDoubleGen [] sharedSketch
        handle1 <-
          enqueueTask pool z3 0 $
            task divModTwiceSpec divModTwiceGen [] sharedSketch
        map <- HM.fromList <$> pollTasksUntilFinished [handle0, handle1]
        fuzzResult
          (snd $ fromRight undefined $ map HM.! handle0)
          addThenDoubleGen
          addThenDoubleSpec
        fuzzResult
          (snd $ fromRight undefined $ map HM.! handle1)
          divModTwiceGen
          divModTwiceSpec,
      testCase "enqueueTaskWithTimeout" $ do
        pool <- newThreadPool 2
        -- The timeout cannot be too short due to
        -- https://github.com/jwiegley/async-pool/issues/31
        -- It cannot be too long, either, otherwise the task may finish before
        -- we can cancel the task.
        handle1 :: handle <-
          enqueueTaskWithTimeout 10000 pool z3 0 $
            task divModTwiceSpec divModTwiceGen [] sharedSketch
        r0 <- pollUntilFinished handle1
        case r0 of
          Left e -> fromException e @?= Just SynthesisTaskTimeout
          _ -> fail "Expected TaskTimeout exception.",
      testCase "cancelTask" $ do
        pool <- newThreadPool 2
        handle1 :: handle <-
          enqueueTask pool z3 0 $
            task divModTwiceSpec divModTwiceGen [] sharedSketch
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
          enqueueTask pool z3 0 $
            task divModTwiceSpec divModTwiceGen [] sharedSketch
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
          enqueueTask pool z3 0 $
            task divModTwiceSpec divModTwiceGen [] sharedSketch
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
          abs (diffUTCTime endTime expectedEndTime) < 0.3,
      testCase "with precondition that can be provided later" $ do
        expectedCost <- newEmptyTMVarIO
        pool <- newThreadPool 1
        let sketchCost =
              progCost (PerStmtCostObj TestSemanticsCost) times4Sketch ::
                SymbolicContext SymInteger
        handle :: handle <-
          enqueueTaskPrecond
            pool
            z3
            0
            (task times4Spec times4Gen [] times4Sketch)
            $ atomically
            $ do
              cost <- readTMVar expectedCost
              return $ sketchCost .== return cost
        atomically $ putTMVar expectedCost 4
        Right (_, SynthesisSuccess prog) <- waitCatch handle
        progCost (PerStmtCostObj TestSemanticsCost) prog
          @?= Right (4 :: SymInteger)
    ]
