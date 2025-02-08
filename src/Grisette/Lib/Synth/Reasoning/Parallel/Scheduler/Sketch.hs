{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Sketch
  ( addRootSketch,
    splitNode,
  )
where

import Control.Monad (void)
import Data.Foldable (Foldable (toList))
import qualified Data.HashMap.Strict as HM
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Grisette
  ( PPrint (pformat),
    nest,
    vsep,
  )
import Grisette.Lib.Synth.Program.Choice.Counting
  ( countNumChoicesWithEvidence,
    countNumProgsWithEvidence,
  )
import Grisette.Lib.Synth.Program.Choice.Split
  ( LowestSeqNum (lowestSeqNum),
    PartitionSpec (partitionSpec),
  )
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable)
import qualified Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.BiasedQueue as Q
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Config
  ( SchedulerConfig
      ( SchedulerConfig,
        biasedDrawProbability,
        cmdline,
        costObj,
        countNumProgsEvidence,
        doDeadCodeElimination,
        easySketchFromFastResult,
        exactCost,
        fastTrackTimeoutSeconds,
        initialMinimalCost,
        initialSplitRatio,
        initialTimeoutSeconds,
        logConfig,
        logger,
        parallelism,
        pollIntervalSeconds,
        restartRunningTimeThresholdSeconds,
        rootPriority,
        schedulerRandomSeed,
        schedulerTimeoutSeconds,
        solverConfig,
        subNodePriorityMultiplier,
        subNodeRandomMultiplierRange,
        successNodeNewTimeoutSeconds,
        synthesisSketchSymbol,
        targetCost,
        transcriptSMT,
        verifiers
      ),
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.DCTree
  ( NodeId,
    insertRootSketches,
    insertSplittedSketches,
    nodeFailed,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.NodeState
  ( NodeState
      ( NodeState
      ),
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.NodeStatus
  ( NodeStatus
      ( NodeNotYetStarted
      ),
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Scheduler
  ( NodeInfo
      ( NodeInfo
      ),
    Scheduler
      ( Scheduler,
        config,
        currentMinimalCost,
        dcTree,
        nodeInfo,
        nodeQueue,
        nodeStates,
        nodeToProcess,
        processToNode,
        processes,
        queueLock,
        randGen,
        schedulerStartTime,
        stopped
      ),
    getIsSplitted,
    getPriority,
    getSketchTable,
    setIsSplitted,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Transition (nodeInferFailureTransition)
import Grisette.Lib.Synth.Util.Logging (logMultiLineDoc)
import System.Log.Logger (Priority (DEBUG, NOTICE))
import System.Random.Stateful (UniformRange (uniformRM))

_addSubSketches ::
  Scheduler
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
  Maybe NodeId ->
  HM.HashMap (SymbolTable sketchSpec) Double ->
  IO [NodeId]
_addSubSketches
  scheduler@Scheduler {config = SchedulerConfig {..}, ..}
  parentId
  sketchPriorities = do
    oldDcTree <- readIORef dcTree
    let sketches = HM.keys sketchPriorities
    let (nodeIds, newDcTree) = case parentId of
          Nothing -> insertRootSketches oldDcTree sketches
          Just parentId ->
            insertSplittedSketches oldDcTree parentId sketches
    let sketchesToNodeId = HM.fromList $ zip sketches nodeIds

    let nodeIdToSketches =
          HM.fromList $ (\(a, b) -> (b, a)) <$> HM.toList sketchesToNodeId
    let nodeIdPriorities =
          HM.map (sketchPriorities HM.!) nodeIdToSketches

    parentBasePriority <-
      case parentId of
        Just parentId -> getPriority scheduler parentId
        Nothing -> return $ Q.Priority rootPriority 1 False Nothing False
    let childrenBasePriority =
          subNodePriorityMultiplier * Q.basePriority parentBasePriority
    writeIORef dcTree newDcTree
    let initialState = NodeState NodeNotYetStarted Nothing Nothing [] [] []
    modifyIORef' nodeStates $ \nodeStates' ->
      foldr (`HM.insert` initialState) nodeStates' sketchesToNodeId
    let taskPriority nid =
          parentBasePriority
            { Q.basePriority = childrenBasePriority,
              Q.randomPriority = nodeIdPriorities HM.! nid,
              Q.knownWorking = False,
              Q.knownWorkingAncestorDistance =
                (+ 1) <$> Q.knownWorkingAncestorDistance parentBasePriority
            }
    let taskNodeInfo nid =
          NodeInfo
            False
            (nodeIdToSketches HM.! nid)
            (taskPriority nid)
            initialTimeoutSeconds

    modifyIORef' nodeInfo $ \nodeInfo' ->
      foldr (\nid acc -> HM.insert nid (taskNodeInfo nid) acc) nodeInfo' $
        HM.keysSet nodeIdToSketches
    modifyIORef' nodeQueue $ \q ->
      foldr (\nid -> Q.insert nid (taskPriority nid)) q $
        HM.keysSet nodeIdToSketches
    let nodeIdToTaskPriority =
          HM.fromList $
            (\nid -> (nid, taskPriority nid))
              <$> HM.keys nodeIdToSketches

    finalNodeInfo <- readIORef nodeInfo
    inferredFailures <-
      concat
        <$> traverse
          ( \nid ->
              if nodeFailed newDcTree nid
                then do
                  logMultiLineDoc logger NOTICE $
                    "Newly added node "
                      <> pformat nid
                      <> " is inferred to fail, not inserting to the queue."
                  modifyIORef' nodeQueue $ Q.delete nid
                  nodeInferFailureTransition scheduler nid
                  return [nid]
                else return []
          )
          nodeIds

    numsInfo <- case countNumProgsEvidence of
      Just countNumProgsEvidence -> do
        let nodeIdToNumChoices =
              HM.fromList $
                ( \(sketch, nid) ->
                    (nid, countNumChoicesWithEvidence countNumProgsEvidence sketch)
                )
                  <$> HM.toList sketchesToNodeId
        let nodeIdToNumWellTyped =
              HM.fromList $
                ( \(sketch, nid) ->
                    (nid, countNumProgsWithEvidence countNumProgsEvidence sketch)
                )
                  <$> HM.toList sketchesToNodeId
        return
          [ "Num of choices in sketches: ",
            pformat nodeIdToNumChoices,
            "Num of well typed programs in sketches: ",
            pformat nodeIdToNumWellTyped
          ]
      Nothing -> return []

    logMultiLineDoc logger NOTICE $
      nest 2 $
        vsep $
          concat
            [ [ "Added sub sketches to node " <> pformat parentId <> ":",
                "Priorities and new node IDs: ",
                pformat nodeIdToTaskPriority
              ],
              numsInfo,
              [ "Num of inferred failures: "
                  <> pformat (length inferredFailures),
                "Num of nodes: " <> pformat (HM.size finalNodeInfo)
              ]
            ]
    return $ toList sketchesToNodeId

addRootSketch ::
  Scheduler
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
  SymbolTable sketchSpec ->
  IO () -- NodeId
addRootSketch
  scheduler@Scheduler {config = SchedulerConfig {..}}
  sketch = do
    logMultiLineDoc logger NOTICE $
      nest 2 $
        vsep ["Adding root sketch: ", pformat sketch]
    void $ _addSubSketches scheduler Nothing (HM.fromList [(sketch, 1)])

splitNode ::
  Scheduler
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
  Bool ->
  NodeId ->
  IO [NodeId]
splitNode
  scheduler@Scheduler {config = SchedulerConfig {..}}
  success
  nodeId = do
    nodeSplitted <- getIsSplitted scheduler nodeId
    if nodeSplitted
      then do
        logMultiLineDoc logger NOTICE $
          nest 2 $
            vsep
              [ "Node " <> pformat nodeId <> " is already splitted.",
                "Skipping."
              ]
        return []
      else do
        sketch <- getSketchTable scheduler nodeId
        let seqNum = lowestSeqNum success sketch
        let splittedSketches =
              case seqNum of
                Nothing -> []
                Just seqNum ->
                  filter (/= sketch) $ partitionSpec seqNum sketch
        logMultiLineDoc logger NOTICE $
          nest 2 $
            vsep
              [ "Splitting node " <> pformat nodeId <> ":",
                "Splitted seq num: " <> pformat seqNum,
                "Splitted under successful node: " <> pformat success
              ]
        logMultiLineDoc logger DEBUG $
          vsep
            [ nest 2 $ vsep ["Original sketch: ", pformat sketch],
              nest 2 $ vsep ["Splitted sketches: ", pformat splittedSketches]
            ]
        splittedSketchesWithPriority <-
          HM.fromList
            <$> traverse
              ( \s -> do
                  randomMultiplier <-
                    uniformRM
                      subNodeRandomMultiplierRange
                      (randGen scheduler)
                  return (s, randomMultiplier)
              )
              splittedSketches
        if null splittedSketches
          then do
            logMultiLineDoc logger NOTICE $
              nest 2 $
                vsep
                  [ "Node " <> pformat nodeId <> " have no sub-sketches.",
                    "Skipping."
                  ]
            setIsSplitted scheduler nodeId True
            return []
          else do
            splittedNodeIds <-
              _addSubSketches
                scheduler
                (Just nodeId)
                splittedSketchesWithPriority
            setIsSplitted scheduler nodeId True
            return splittedNodeIds
