module Grisette.Lib.Synth.Reasoning.Server.ThreadPoolTest (threadPoolTest) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (replicateM_, unless)
import Data.Foldable (traverse_)
import Data.Maybe (isNothing)
import Grisette.Lib.Synth.Reasoning.Server.ThreadPool
  ( addNewTask,
    cancelAllTasksWith,
    cancelTaskWith,
    newPool,
    numOfRunningTasks,
    pollTask,
    waitCatchTask,
  )
import Grisette.Lib.Synth.Reasoning.SynthesisServer
  ( TaskException (TaskCancelled),
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
          tid <- addNewTask pool (takeMVar mvar >> return 42)
          r0 <- pollTask pool tid
          assertBool "Poll before finishing" $ isNothing r0
          let wait = do
                r <- numOfRunningTasks pool
                threadDelay 100000
                unless (r == 0) wait
          putMVar mvar ()
          wait
          r1 <- pollTask pool tid
          case r1 of
            Just (Right v) -> v @?= 42
            _ -> fail "Expected Right 42",
        testCase "waitCatchTask" $ do
          pool <- newPool 2
          tid <- addNewTask pool (return 42)
          r1 <- waitCatchTask pool tid
          case r1 of
            Right v -> v @?= 42
            _ -> fail "Expected Right 42",
        testCase "more tasks than parallelism" $ do
          mvar <- newEmptyMVar
          pool <- newPool 2
          tid <-
            traverse
              (\n -> addNewTask pool $ takeMVar mvar >> return n)
              [0 .. 20]
          let wait = do
                r <- numOfRunningTasks pool
                threadDelay 100000
                unless (r == 2) wait
          wait
          replicateM_ 2 (putMVar mvar ())
          wait
          replicateM_ 19 (putMVar mvar ())
          Right 15 <- waitCatchTask pool (tid !! 15)
          results <- traverse (waitCatchTask pool) tid
          traverse_ (\(i, Right v) -> i @?= v) $ zip [0 .. 20] results,
        testCase "cancelTaskWith running" $ do
          mvar <- newEmptyMVar
          pool <- newPool 2
          tid <-
            traverse
              (\n -> addNewTask pool $ takeMVar mvar >> return n)
              [0 .. 1]
          cancelTaskWith pool TaskCancelled (head tid)
          let wait = do
                r <- numOfRunningTasks pool
                threadDelay 100000
                unless (r == 1) wait
          wait
          replicateM_ 1 (putMVar mvar ())
          results <- traverse (waitCatchTask pool) tid
          assertBool "Should be cancelled task" $ case head results of
            Left _ -> True
            _ -> False
          assertBool "Should be finished task" $ case results !! 1 of
            Right _ -> True
            _ -> False,
        testCase "cancelTaskWith pending" $ do
          mvar <- newEmptyMVar
          pool <- newPool 2
          tid <-
            traverse
              (\n -> addNewTask pool $ takeMVar mvar >> return n)
              [0 .. 3]
          cancelTaskWith pool TaskCancelled (last tid)
          replicateM_ 3 (putMVar mvar ())
          results <- traverse (waitCatchTask pool) tid
          assertBool "Should be cancelled task" $ case last results of
            Left _ -> True
            _ -> False
          traverse_ (\(i, Right v) -> i @?= v) $ zip [0 .. 2] results,
        testCase "cancelAllTasksWith" $ do
          mvar <- newEmptyMVar
          pool <- newPool 2
          tid <-
            traverse
              (\n -> addNewTask pool $ takeMVar mvar >> return n)
              [0 .. 3]
          cancelAllTasksWith pool TaskCancelled
          results <- traverse (waitCatchTask pool) tid
          traverse_ (\(Left _) -> return ()) results
      ]
