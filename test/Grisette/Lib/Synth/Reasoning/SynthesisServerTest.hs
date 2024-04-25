module Grisette.Lib.Synth.Reasoning.SynthesisServerTest
  ( synthesisServerTest,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception (fromException), SomeException)
import Data.Either (fromRight)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Time (diffUTCTime, getCurrentTime)
import Grisette.Lib.Synth.Reasoning.IOPair (IOPair)
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
import Grisette.Lib.Synth.Reasoning.SynthesisServer
  ( TaskException (TaskCancelled, TaskTimeout),
    TaskHandle (taskStartTime),
    TaskSet,
    cancelTask,
    newSynthesisServer,
    pollTask,
    pollTasks,
    submitTask,
    submitTaskWithTimeout,
    taskElapsedTime,
    taskEndTime,
    waitCatchTask,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool, (@?=))

pollUntilFinished ::
  TaskHandle conVal conProg matcher exception ->
  IO
    ( Either
        SomeException
        ([(IOPair conVal, matcher)], SynthesisResult conProg exception)
    )
pollUntilFinished handle = do
  r <- pollTask handle
  case r of
    Just v -> return v
    _ -> threadDelay 100000 >> pollUntilFinished handle

pollTasksUntilFinished ::
  TaskSet conVal conProg matcher exception ->
  IO
    ( HM.HashMap
        (TaskHandle conVal conProg matcher exception)
        ( Either
            SomeException
            ([(IOPair conVal, matcher)], SynthesisResult conProg exception)
        )
    )
pollTasksUntilFinished taskSet = do
  (remaining, r) <- pollTasks taskSet
  if HS.null remaining
    then return r
    else threadDelay 100000 >> HM.union r <$> pollTasksUntilFinished remaining

synthesisServerTest :: Test
synthesisServerTest =
  testGroup
    "SynthesisServer"
    [ testCase "Concurrently synthesize several programs" $ do
        server <- newSynthesisServer 2
        handle0 <-
          submitTask server $
            task addThenDoubleSpec addThenDoubleGen sharedSketch
        handle1 <-
          submitTask server $
            task divModTwiceSpec divModTwiceGen sharedSketch
        handle2 <-
          submitTask server $
            task addThenDoubleReverseSpec addThenDoubleGen sharedSketch
        Right r0 <- pollUntilFinished handle0
        Right r1 <- pollUntilFinished handle1
        Right r2 <- pollUntilFinished handle2
        fuzzResult r0 addThenDoubleGen addThenDoubleSpec
        fuzzResult r1 divModTwiceGen divModTwiceSpec
        fuzzResult r2 addThenDoubleGen addThenDoubleReverseSpec,
      testCase "pollTasks" $ do
        server <- newSynthesisServer 2
        handle0 <-
          submitTask server $
            task addThenDoubleSpec addThenDoubleGen sharedSketch
        handle1 <-
          submitTask server $
            task divModTwiceSpec divModTwiceGen sharedSketch
        map <-
          pollTasksUntilFinished $ HS.fromList [handle0, handle1]
        fuzzResult
          (fromRight undefined $ map HM.! handle0)
          addThenDoubleGen
          addThenDoubleSpec
        fuzzResult
          (fromRight undefined $ map HM.! handle1)
          divModTwiceGen
          divModTwiceSpec,
      testCase "submitTaskWithTimeout" $ do
        server <- newSynthesisServer 2
        -- The timeout cannot be too short due to
        -- https://github.com/jwiegley/async-pool/issues/31
        -- It cannot be too long, either, otherwise the task may finish before
        -- we can cancel the task.
        handle1 <-
          submitTaskWithTimeout server 10000 $
            task divModTwiceSpec divModTwiceGen sharedSketch
        r0 <- pollUntilFinished handle1
        case r0 of
          Left e -> fromException e @?= Just TaskTimeout
          _ -> fail "Expected TaskTimeout exception.",
      testCase "cancelTask" $ do
        server <- newSynthesisServer 2
        handle1 <-
          submitTask server $
            task divModTwiceSpec divModTwiceGen sharedSketch
        -- The delay is necessary due to
        -- https://github.com/jwiegley/async-pool/issues/31
        -- It cannot be too long, either, otherwise the task may finish before
        -- we can cancel the task.
        threadDelay 10000
        cancelTask handle1
        r0 <- pollUntilFinished handle1
        case r0 of
          Left e -> fromException e @?= Just TaskCancelled
          _ -> fail "Expected TaskCancelled exception.",
      testCase "time measurement" $ do
        server <- newSynthesisServer 2
        handle <-
          submitTask server $
            task divModTwiceSpec divModTwiceGen sharedSketch
        _ <- waitCatchTask handle
        expectedEndTime <- getCurrentTime
        let expectedElapsedTime =
              diffUTCTime expectedEndTime $ taskStartTime handle
        elapsedTime <- taskElapsedTime handle
        endTime <- taskEndTime handle
        assertBool "Diff should be less than 0.1 second" $
          abs (expectedElapsedTime - elapsedTime) < 0.1
        assertBool "End time diff should be less than 0.1 second" $
          abs (diffUTCTime endTime expectedEndTime) < 0.1,
      testCase "time measurement for cancelled tasks" $ do
        server <- newSynthesisServer 2
        handle <-
          submitTask server $
            task divModTwiceSpec divModTwiceGen sharedSketch
        threadDelay 100000
        cancelTask handle
        expectedEndTime <- getCurrentTime
        let expectedElapsedTime =
              diffUTCTime expectedEndTime $ taskStartTime handle
        elapsedTime <- taskElapsedTime handle
        endTime <- taskEndTime handle
        assertBool "Diff should be less than 0.1 second" $
          abs (expectedElapsedTime - elapsedTime) < 0.1
        assertBool "End time diff should be less than 0.01 second" $
          abs (diffUTCTime endTime expectedEndTime) < 0.1
    ]
