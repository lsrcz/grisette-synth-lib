module Grisette.Lib.Synth.Reasoning.SynthesisServer.TaskQueueTest
  ( taskQueueTest,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (atomically)
import Grisette.Lib.Synth.Reasoning.SynthesisServer.TaskQueue
  ( newTaskQueue,
    nextTask,
    removeTask,
    submitTask,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

taskQueueTest :: Test
taskQueueTest =
  testGroup
    "TaskQueue"
    [ testCase "submit then retrieve" $ do
        queue <- atomically newTaskQueue
        taskId1 <- atomically $ submitTask queue "task1"
        taskId2 <- atomically $ submitTask queue "task2"
        (taskId1', task1') <- atomically $ nextTask queue
        (taskId2', task2') <- atomically $ nextTask queue
        taskId1 @?= 0
        taskId2 @?= 1
        taskId1' @?= taskId1
        taskId2' @?= taskId2
        task1' @?= "task1"
        task2' @?= "task2",
      testGroup
        "remove"
        [ testCase "submit, remove" $ do
            queue <- atomically newTaskQueue
            taskId1 <- atomically $ submitTask queue "task1"
            taskId2 <- atomically $ submitTask queue "task2"
            removed <- atomically $ removeTask queue taskId1
            (taskId2', task2') <- atomically $ nextTask queue
            removed @?= True
            taskId2 @?= 1
            taskId2' @?= taskId2
            task2' @?= "task2",
          testCase "submit, remove non-existent" $ do
            queue <- atomically newTaskQueue
            taskId1 <- atomically $ submitTask queue "task1"
            taskId2 <- atomically $ submitTask queue "task2"
            removed <- atomically $ removeTask queue 3
            (taskId1', task1') <- atomically $ nextTask queue
            (taskId2', task2') <- atomically $ nextTask queue
            removed @?= False
            taskId1 @?= 0
            taskId2 @?= 1
            taskId1' @?= taskId1
            taskId2' @?= taskId2
            task1' @?= "task1"
            task2' @?= "task2",
          testCase "remove on empty queue" $ do
            queue <- atomically newTaskQueue
            removed <- atomically $ removeTask queue 0
            removed @?= False
        ],
      testCase "async submit" $ do
        queue <- atomically newTaskQueue
        retrived <- async $ atomically $ nextTask queue
        threadDelay 100000
        taskId1 <- atomically $ submitTask queue "task1"
        (taskId1', task1') <- wait retrived
        taskId1 @?= 0
        taskId1' @?= taskId1
        task1' @?= "task1"
    ]
