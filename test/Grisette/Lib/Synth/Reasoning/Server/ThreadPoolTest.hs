module Grisette.Lib.Synth.Reasoning.Server.ThreadPoolTest (threadPoolTest) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (replicateM, replicateM_, unless, when)
import Data.Foldable (traverse_)
import Data.Maybe (isNothing)
import Data.Time (diffUTCTime, getCurrentTime)
import Grisette.Lib.Synth.Reasoning.Server.Exception
  ( SynthesisTaskException (SynthesisTaskCancelled),
  )
import Grisette.Lib.Synth.Reasoning.Server.ThreadPool
  ( alterIfPending,
    cancelAllWith,
    cancelWith,
    elapsedTime,
    endTime,
    maybeElapsedTime,
    maybeEndTime,
    maybeStartTime,
    newThread,
    newThreadPool,
    numOfRunningThreads,
    poll,
    startTime,
    waitCatch,
  )
import Test.Framework
  ( Test,
    TestOptions' (topt_timeout),
    plusTestOptions,
    testGroup,
  )
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool, (@?=))

threadPoolTest :: Test
threadPoolTest =
  plusTestOptions (mempty {topt_timeout = Just $ Just 5000000}) $
    testGroup
      "Grisette.Lib.Synth.Reasoning.Server.ThreadPool"
      [ testCase "poll" $ do
          mvar <- newEmptyMVar
          pool <- newThreadPool 2
          handle <- newThread pool (takeMVar mvar >> return (42 :: Int))
          r0 <- poll handle
          assertBool "Poll before finishing" $ isNothing r0
          let wait = do
                r <- numOfRunningThreads pool
                threadDelay 100000
                unless (r == 0) wait
          putMVar mvar ()
          wait
          r1 <- poll handle
          case r1 of
            Just (Right v) -> v @?= 42
            _ -> fail "Expected Right 42",
        testCase "waitCatch" $ do
          pool <- newThreadPool 2
          handle <- newThread pool (return (42 :: Int))
          r1 <- waitCatch handle
          case r1 of
            Right v -> v @?= 42
            _ -> fail "Expected Right 42",
        testCase "more tasks than parallelism" $ do
          mvar <- newEmptyMVar
          pool <- newThreadPool 2
          handle <-
            traverse
              (\n -> newThread pool $ takeMVar mvar >> return n)
              [0 .. 20 :: Int]
          let wait = do
                r <- numOfRunningThreads pool
                threadDelay 100000
                unless (r == 2) wait
          wait
          replicateM_ 2 (putMVar mvar ())
          wait
          replicateM_ 19 (putMVar mvar ())
          Right 15 <- waitCatch (handle !! 15)
          results <- traverse waitCatch handle
          traverse_ (\(i, Right v) -> i @?= v) $ zip [0 .. 20 :: Int] results,
        testCase "cancelWith running" $ do
          mvar <- newEmptyMVar
          pool <- newThreadPool 2
          handle <-
            traverse
              (\n -> newThread pool $ takeMVar mvar >> return n)
              [0 .. 1 :: Int]
          let wait = do
                r <- numOfRunningThreads pool
                threadDelay 100000
                unless (r == 2) wait
          wait
          cancelWith SynthesisTaskCancelled (head handle)
          replicateM_ 1 (putMVar mvar ())
          results <- traverse waitCatch handle
          assertBool "Should be cancelled task" $ case head results of
            Left _ -> True
            _ -> False
          assertBool "Should be finished task" $ case results !! 1 of
            Right _ -> True
            _ -> False,
        testCase "cancelWith pending" $ do
          mvar <- newEmptyMVar
          pool <- newThreadPool 2
          handle <-
            traverse
              (\n -> newThread pool $ takeMVar mvar >> return n)
              [0 .. 3 :: Int]
          cancelWith SynthesisTaskCancelled (last handle)
          replicateM_ 3 (putMVar mvar ())
          results <- traverse waitCatch handle
          assertBool "Should be cancelled task" $ case last results of
            Left _ -> True
            _ -> False
          traverse_ (\(i, Right v) -> i @?= v) $ zip [0 .. 2 :: Int] results,
        testCase "cancelAllWith" $ do
          mvar <- newEmptyMVar
          pool <- newThreadPool 2
          handle <-
            traverse
              (\n -> newThread pool $ takeMVar mvar >> return n)
              [0 .. 3 :: Int]
          cancelAllWith pool SynthesisTaskCancelled
          results <- traverse waitCatch handle
          traverse_ (\(Left _) -> return ()) results,
        testCase "alterIfPending" $ do
          mvars <- replicateM 4 newEmptyMVar
          mvar1 <- newEmptyMVar
          pool <- newThreadPool 2
          handles <-
            traverse
              ( \n ->
                  newThread pool $
                    takeMVar (mvars !! n)
                      >> when (n == 0) (putMVar mvar1 ())
                      >> return n
              )
              [0 .. 3 :: Int]
          putMVar (head mvars) ()
          takeMVar mvar1
          traverse_ (`alterIfPending` return 42) handles
          putMVar (mvars !! 1) ()
          putMVar (mvars !! 2) ()
          results <- traverse waitCatch handles
          traverse_ (\(i, Right v) -> i @?= v) $ zip [0, 1, 2, 42] results,
        testCase "timing test" $ do
          mvars <- replicateM 3 newEmptyMVar
          pool <- newThreadPool 2
          handle <-
            traverse
              (\n -> newThread pool $ takeMVar (mvars !! n) >> return n)
              [0 .. 2 :: Int]
          currentTime <- getCurrentTime
          startTime2 <- maybeStartTime (handle !! 2)
          startTime2 @?= Nothing
          endTime2 <- maybeEndTime (handle !! 2)
          endTime2 @?= Nothing
          elapsedTime2 <- maybeElapsedTime (handle !! 2)
          elapsedTime2 @?= Nothing
          mapM_ (`putMVar` 0) mvars
          startTime2' <- startTime (handle !! 2)
          assertBool "The time must be after the current time" $
            currentTime <= startTime2'
          endTime2' <- endTime (handle !! 2)
          assertBool "The time must be after the current time" $
            startTime2' <= endTime2'
          elapsedTime2' <- elapsedTime (handle !! 2)
          assertBool "The time must be after the current time" $
            elapsedTime2' == endTime2' `diffUTCTime` startTime2'
      ]
