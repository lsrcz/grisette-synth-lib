module Grisette.Lib.Synth.Reasoning.Server.ThreadPoolTest (threadPoolTest) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (replicateM, replicateM_, unless, when)
import Data.Foldable (traverse_)
import Data.Maybe (isNothing)
import Grisette.Lib.Synth.Reasoning.Server.BaseTaskHandle
  ( SynthesisTaskException (SynthesisTaskCancelled),
  )
import Grisette.Lib.Synth.Reasoning.Server.ThreadPool
  ( addNewTask,
    alterTaskIfPending,
    cancelAllTasksWith,
    cancelTaskWith,
    newPool,
    numOfRunningTasks,
    pollTask,
    waitCatchTask,
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
      [ testCase "pollTask" $ do
          mvar <- newEmptyMVar
          pool <- newPool 2
          handle <- addNewTask pool (takeMVar mvar >> return (42 :: Int))
          r0 <- pollTask handle
          assertBool "Poll before finishing" $ isNothing r0
          let wait = do
                r <- numOfRunningTasks pool
                threadDelay 100000
                unless (r == 0) wait
          putMVar mvar ()
          wait
          r1 <- pollTask handle
          case r1 of
            Just (Right v) -> v @?= 42
            _ -> fail "Expected Right 42",
        testCase "waitCatchTask" $ do
          pool <- newPool 2
          handle <- addNewTask pool (return (42 :: Int))
          r1 <- waitCatchTask handle
          case r1 of
            Right v -> v @?= 42
            _ -> fail "Expected Right 42",
        testCase "more tasks than parallelism" $ do
          mvar <- newEmptyMVar
          pool <- newPool 2
          handle <-
            traverse
              (\n -> addNewTask pool $ takeMVar mvar >> return n)
              [0 .. 20 :: Int]
          let wait = do
                r <- numOfRunningTasks pool
                threadDelay 100000
                unless (r == 2) wait
          wait
          replicateM_ 2 (putMVar mvar ())
          wait
          replicateM_ 19 (putMVar mvar ())
          Right 15 <- waitCatchTask (handle !! 15)
          results <- traverse waitCatchTask handle
          traverse_ (\(i, Right v) -> i @?= v) $ zip [0 .. 20 :: Int] results,
        testCase "cancelTaskWith running" $ do
          mvar <- newEmptyMVar
          pool <- newPool 2
          handle <-
            traverse
              (\n -> addNewTask pool $ takeMVar mvar >> return n)
              [0 .. 1 :: Int]
          cancelTaskWith SynthesisTaskCancelled (head handle)
          let wait = do
                r <- numOfRunningTasks pool
                threadDelay 100000
                unless (r == 1) wait
          wait
          replicateM_ 1 (putMVar mvar ())
          results <- traverse waitCatchTask handle
          assertBool "Should be cancelled task" $ case head results of
            Left _ -> True
            _ -> False
          assertBool "Should be finished task" $ case results !! 1 of
            Right _ -> True
            _ -> False,
        testCase "cancelTaskWith pending" $ do
          mvar <- newEmptyMVar
          pool <- newPool 2
          handle <-
            traverse
              (\n -> addNewTask pool $ takeMVar mvar >> return n)
              [0 .. 3 :: Int]
          cancelTaskWith SynthesisTaskCancelled (last handle)
          replicateM_ 3 (putMVar mvar ())
          results <- traverse waitCatchTask handle
          assertBool "Should be cancelled task" $ case last results of
            Left _ -> True
            _ -> False
          traverse_ (\(i, Right v) -> i @?= v) $ zip [0 .. 2 :: Int] results,
        testCase "cancelAllTasksWith" $ do
          mvar <- newEmptyMVar
          pool <- newPool 2
          handle <-
            traverse
              (\n -> addNewTask pool $ takeMVar mvar >> return n)
              [0 .. 3 :: Int]
          cancelAllTasksWith pool SynthesisTaskCancelled
          results <- traverse waitCatchTask handle
          traverse_ (\(Left _) -> return ()) results,
        testCase "alterTaskIfPending" $ do
          mvars <- replicateM 4 newEmptyMVar
          mvar1 <- newEmptyMVar
          pool <- newPool 2
          handles <-
            traverse
              ( \n ->
                  addNewTask pool $
                    takeMVar (mvars !! n)
                      >> when (n == 0) (putMVar mvar1 ())
                      >> return n
              )
              [0 .. 3 :: Int]
          putMVar (head mvars) ()
          takeMVar mvar1
          traverse_ (`alterTaskIfPending` return 42) handles
          putMVar (mvars !! 1) ()
          putMVar (mvars !! 2) ()
          results <- traverse waitCatchTask handles
          traverse_ (\(i, Right v) -> i @?= v) $ zip [0, 1, 2, 42] results
      ]
