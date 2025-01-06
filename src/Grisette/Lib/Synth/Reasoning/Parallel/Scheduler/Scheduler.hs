{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Scheduler
  ( ProcessScheduler (..),
    NodeInfo (..),
    newProcessScheduler,
    getCPid,
    getProcessByCPid,
    getProcess,
    getCurrentMinimalCost,
    getCurrentTimeout,
    getCurrentElapsedTime,
    getStatus,
    getNodeInfo,
    getDepth,
    getIsSplitted,
    getSketchTable,
    getPriority,
    getNumRunningProcess,
    getNumQueuedProcess,
    setIsSplitted,
    setPriority,
    setTimeout,
    removeProcessByCPid,
    removeProcess,
    updateCurrentMinimalCost,
  )
where

import Control.Concurrent (MVar, newMVar)
import Control.Exception (throwIO)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Int (Int32)
import Data.Time
  ( NominalDiffTime,
    UTCTime,
    getCurrentTime,
  )
import Foreign.C (eBADF)
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable)
import qualified Grisette.Lib.Synth.Reasoning.Parallel.BiasedQueue as Q
import Grisette.Lib.Synth.Reasoning.Parallel.DCTree
  ( DCTree,
    NodeId,
    emptyDCTree,
    nodeDepth,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.NodeState
  ( NodeState (nodeStatus),
    nodeStateCurrentElapsedTime,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.NodeStatus (NodeStatus)
import Grisette.Lib.Synth.Reasoning.Parallel.Process
  ( Process (pipeRd, pipeWr),
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Config
  ( ProcessSchedulerConfig,
    biasedDrawProbability,
    initialMinimalCost,
    schedulerRandomSeed,
  )
import Grisette.Lib.Synth.Util.Exception (catchErrno)
import System.Posix
  ( closeFd,
  )
import System.Random.Stateful
  ( AtomicGenM,
    StdGen,
    mkStdGen,
    newAtomicGenM,
  )

data NodeInfo sketchSpec = NodeInfo
  { nodeSplitted :: Bool,
    nodeSketchTable :: SymbolTable sketchSpec,
    nodePriority :: Q.Priority,
    nodeTimeoutSeconds :: Int
  }

data
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher
  where
  ProcessScheduler ::
    { config ::
        ProcessSchedulerConfig
          sketchSpec
          sketch
          conProg
          costObj
          cost
          symSemObj
          symVal
          conSemObj
          conVal
          matcher,
      randGen :: AtomicGenM StdGen,
      nodeInfo :: IORef (HM.HashMap NodeId (NodeInfo sketchSpec)),
      nodeStates ::
        IORef
          ( HM.HashMap
              NodeId
              (NodeState conProg symSemObj symVal conSemObj conVal matcher)
          ),
      nodeQueue :: IORef Q.BiasedQueue,
      processes :: IORef (HM.HashMap Int32 Process),
      processToNode :: IORef (HM.HashMap Int32 NodeId),
      nodeToProcess :: IORef (HM.HashMap NodeId Int32),
      dcTree :: IORef (DCTree (SymbolTable sketchSpec)),
      currentMinimalCost :: IORef (Maybe Int),
      queueLock :: MVar (),
      stopped :: IORef Bool,
      schedulerStartTime :: UTCTime
    } ->
    ProcessScheduler
      sketchSpec
      sketch
      conProg
      costObj
      cost
      symSemObj
      symVal
      conSemObj
      conVal
      matcher

newProcessScheduler ::
  ProcessSchedulerConfig
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  IO
    ( ProcessScheduler
        sketchSpec
        sketch
        conProg
        costObj
        cost
        symSemObj
        symVal
        conSemObj
        conVal
        matcher
    )
newProcessScheduler config = do
  randGen <- newAtomicGenM (mkStdGen (schedulerRandomSeed config))
  nodeInfo <- newIORef HM.empty
  nodeStates <- newIORef HM.empty
  nodeQueue <- newIORef $ Q.empty (biasedDrawProbability config)
  processes <- newIORef HM.empty
  processToNode <- newIORef HM.empty
  nodeToProcess <- newIORef HM.empty
  dcTree <- newIORef emptyDCTree
  currentMinimalCost <- newIORef $ initialMinimalCost config
  queueLock <- newMVar ()
  stopped <- newIORef False
  schedulerStartTime <- getCurrentTime
  return $ ProcessScheduler {..}

getCPid ::
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO Int32
getCPid ProcessScheduler {..} nid =
  readIORef nodeToProcess >>= \m -> return $ m HM.! nid

getProcessByCPid ::
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  Int32 ->
  IO Process
getProcessByCPid ProcessScheduler {..} cpid =
  readIORef processes >>= \m -> return $ m HM.! cpid

getProcess ::
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO Process
getProcess scheduler nid = do
  cpid <- getCPid scheduler nid
  getProcessByCPid scheduler cpid

getCurrentMinimalCost ::
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  IO (Maybe Int)
getCurrentMinimalCost ProcessScheduler {..} = readIORef currentMinimalCost

getCurrentTimeout ::
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO Int
getCurrentTimeout ProcessScheduler {..} nid = do
  nodeInfo <- readIORef nodeInfo
  case HM.lookup nid nodeInfo of
    Just NodeInfo {nodeTimeoutSeconds} -> return nodeTimeoutSeconds
    Nothing -> error "Should not happen: node info not found"

getCurrentElapsedTime ::
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO NominalDiffTime
getCurrentElapsedTime ProcessScheduler {..} nid = do
  curTime <- getCurrentTime
  nodeStates <- readIORef nodeStates
  return $ nodeStateCurrentElapsedTime curTime $ nodeStates HM.! nid

getStatus ::
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO (NodeStatus conProg)
getStatus ProcessScheduler {..} nid = do
  nodeStates <- readIORef nodeStates
  return $ nodeStatus $ nodeStates HM.! nid

getNodeInfo ::
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO (NodeInfo sketchSpec)
getNodeInfo ProcessScheduler {..} nid = do
  nodeInfo <- readIORef nodeInfo
  return $ nodeInfo HM.! nid

getDepth ::
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO Int
getDepth ProcessScheduler {..} nid = do
  dcTree <- readIORef dcTree
  return $ nodeDepth dcTree nid

getIsSplitted ::
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO Bool
getIsSplitted ProcessScheduler {..} nid = do
  nodeInfo <- readIORef nodeInfo
  return $ nodeSplitted $ nodeInfo HM.! nid

getSketchTable ::
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO (SymbolTable sketchSpec)
getSketchTable ProcessScheduler {..} nid = do
  nodeInfo <- readIORef nodeInfo
  return $ nodeSketchTable $ nodeInfo HM.! nid

getPriority ::
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO Q.Priority
getPriority ProcessScheduler {..} nid = do
  nodeInfo <- readIORef nodeInfo
  return $ nodePriority $ nodeInfo HM.! nid

getNumRunningProcess ::
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  IO Int
getNumRunningProcess ProcessScheduler {..} = length <$> readIORef processes

getNumQueuedProcess ::
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  IO Int
getNumQueuedProcess ProcessScheduler {..} = Q.size <$> readIORef nodeQueue

setIsSplitted ::
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  Bool ->
  IO ()
setIsSplitted ProcessScheduler {..} nid isSplitted = do
  modifyIORef' nodeInfo $ HM.adjust (\ni -> ni {nodeSplitted = isSplitted}) nid

setPriority ::
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  Q.Priority ->
  IO ()
setPriority ProcessScheduler {..} nid priority = do
  modifyIORef' nodeInfo $ HM.adjust (\ni -> ni {nodePriority = priority}) nid
  modifyIORef' nodeQueue $ Q.setPriority nid priority

setTimeout ::
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  Int ->
  IO ()
setTimeout ProcessScheduler {..} nid timeout = do
  modifyIORef' nodeInfo $
    HM.adjust
      (\ni -> ni {nodeTimeoutSeconds = timeout})
      nid

removeProcessByCPid ::
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  Int32 ->
  IO ()
removeProcessByCPid ProcessScheduler {..} cpid = do
  process <- readIORef processes >>= \m -> return $ m HM.! cpid
  let ec e v = if v == eBADF then return () else throwIO e
  catchErrno (closeFd $ pipeRd process) ec
  catchErrno (closeFd $ pipeWr process) ec
  modifyIORef' processes $ HM.delete cpid
  nid <- readIORef processToNode >>= \m -> return $ m HM.! cpid
  modifyIORef' processToNode $ HM.delete cpid
  modifyIORef' nodeToProcess $ HM.delete nid

removeProcess ::
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NodeId ->
  IO ()
removeProcess scheduler nid = do
  cpid <- getCPid scheduler nid
  removeProcessByCPid scheduler cpid

updateCurrentMinimalCost ::
  ProcessScheduler
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  Maybe Int ->
  IO ()
updateCurrentMinimalCost ProcessScheduler {..} newCost = do
  modifyIORef' currentMinimalCost $ \case
    Nothing -> newCost
    Just oldCost -> case newCost of
      Nothing -> return oldCost
      Just newCost -> Just $ min oldCost newCost
