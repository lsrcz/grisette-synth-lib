module Grisette.Lib.Synth.Reasoning.SynthesisServer.TaskQueue
  ( TaskQueue,
    newTaskQueue,
    submitTask,
    nextTask,
    removeTask,
  )
where

import Control.Concurrent.STM (STM, readTVar, writeTVar)
import Control.Concurrent.STM.TSem (TSem, newTSem, signalTSem, waitTSem)
import Control.Concurrent.STM.TVar (TVar, newTVar)
import qualified Data.HashMap.Internal.Strict as HM
import qualified Data.Sequence as Sequence

type TaskId = Int

data TaskQueue task = TaskQueue
  { _tasks ::
      TVar (HM.HashMap TaskId task),
    _queue :: TVar (Sequence.Seq TaskId),
    _nextTaskId :: TVar TaskId,
    _numAvailableTask :: TSem
  }

newTaskQueue :: STM (TaskQueue task)
newTaskQueue = do
  tasksTVar <- newTVar HM.empty
  queueTVar <- newTVar Sequence.empty
  nextTaskIdTVar <- newTVar 0
  numTSem <- newTSem 0
  return $ TaskQueue tasksTVar queueTVar nextTaskIdTVar numTSem

submitTask :: TaskQueue task -> task -> STM TaskId
submitTask (TaskQueue tasksTVar queueTVar nextTaskIdTVar numTSem) task = do
  tasks <- readTVar tasksTVar
  queue <- readTVar queueTVar
  nextTaskId <- readTVar nextTaskIdTVar
  writeTVar tasksTVar $ HM.insert nextTaskId task tasks
  writeTVar queueTVar $ queue Sequence.|> nextTaskId
  writeTVar nextTaskIdTVar $ nextTaskId + 1
  signalTSem numTSem
  return nextTaskId

nextTask :: TaskQueue task -> STM (TaskId, task)
nextTask (TaskQueue tasksTVar queueTVar _ numTSem) = do
  waitTSem numTSem
  queue <- readTVar queueTVar
  tasks <- readTVar tasksTVar
  go queue tasks
  where
    go (nextTaskId Sequence.:<| rest) tasks = do
      let task = HM.lookup nextTaskId tasks
      case task of
        Just t -> do
          writeTVar queueTVar rest
          return (nextTaskId, t)
        Nothing -> go rest tasks
    go _ _ = error "nextTask: impossible"

removeTask :: TaskQueue task -> TaskId -> STM Bool
removeTask (TaskQueue tasksTVar _ _ numTSem) taskId = do
  tasks <- readTVar tasksTVar
  if HM.size tasks > 0
    then do
      waitTSem numTSem
      case HM.lookup taskId tasks of
        Nothing -> do
          signalTSem numTSem
          return False
        Just _ -> do
          writeTVar tasksTVar $ HM.delete taskId tasks
          return True
    else return False
