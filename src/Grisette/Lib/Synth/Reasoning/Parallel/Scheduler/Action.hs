{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Action
  ( killNode,
    killIfTimeout,
    checkResponse,
    resetIfJustStarted,
    startQueued,
    refineNode,
    markFailure,
    markSuccess,
    markAllChildrenSuccess,
    markAllSiblingChildrenSuccess,
    killIfDividedChildrenAllStarted,
  )
where

import Control.Exception (throwIO)
import Control.Monad (unless, void, when)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.Maybe (fromJust, isNothing)
import Data.Time
  ( nominalDiffTimeToSeconds,
  )
import Foreign.C (eSRCH)
import Grisette
  ( PPrint (pformat),
    viaShow,
  )
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
        schedulerRandomSeed,
        schedulerTimeoutSeconds,
        solverConfig,
        successNodeNewTimeoutSeconds,
        synthesisSketchSymbol,
        targetCost,
        transcriptSMT,
        verifiers
      ),
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.DCTree
  ( NodeId,
    allChildrenNodes,
    allSiblingNodes,
    markNodeFailed,
    nodeDividedChildren,
    nodeFailed,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.NodeStatus
  ( NodeAction,
    NodeStatus
      ( NodeFailed,
        NodeInferredFailure,
        NodeNotYetStarted,
        NodeRefining,
        NodeStarted,
        NodeSucceeded,
        NodeTerminated,
        NodeViable
      ),
    StatusType (StatusRefining),
    statusType,
    statusTypeIsNotYetStarted,
    statusTypeIsRunning,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Process
  ( Message (Failure),
    Process (pgid, pid),
    ProcessConfig
      ( ProcessConfig,
        costObj,
        countNumProgsEvidence,
        doDeadCodeElimination,
        easySketchFromFastResult,
        easySynthTimeout,
        exactCost,
        initialCost,
        logConfig,
        nodeId,
        sketchSpec,
        sketchSymbol,
        transcriptSMT,
        verifiers
      ),
    ProcessResponse,
    getProcessResponse,
    runRequestInSubProcess,
    sendNewMinimalCost,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Scheduler
  ( NodeInfo (nodeSplitted),
    Scheduler
      ( Scheduler,
        config,
        currentMinimalCost,
        dcTree,
        nodeInfo,
        nodeQueue,
        nodeSplitQueue,
        nodeStates,
        nodeToProcess,
        processToNode,
        processes,
        queueLock,
        randGen,
        schedulerStartTime,
        stopped
      ),
    getCurrentElapsedTime,
    getCurrentMinimalCost,
    getCurrentTimeout,
    getDepth,
    getFirstNotFullySplitDepth,
    getNodeInfo,
    getNumRunningProcess,
    getPriority,
    getProcess,
    getProcessByCPid,
    getSketchTable,
    getStatus,
    removeProcess,
    setPriority,
    setTimeout,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Sketch (splitNode)
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Transition
  ( nodeInferFailureTransition,
    nodeResetTransition,
    nodeStartTransition,
    nodeTransition,
  )
import Grisette.Lib.Synth.Util.Exception (catchErrno)
import Grisette.Lib.Synth.Util.Logging (logMultiLineDoc)
import System.Log.Logger (Priority (NOTICE))
import System.Posix
  ( CPid (CPid),
    sigKILL,
    signalProcessGroup,
  )
import System.Random.Stateful (UniformRange (uniformRM))

_getNodeResponse ::
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
  IO
    ( Maybe
        (ProcessResponse conProg symSemObj symVal conSemObj conVal matcher)
    )
_getNodeResponse
  scheduler@Scheduler {config = SchedulerConfig {..}}
  blk
  nid = do
    process <- getProcess scheduler nid
    response <- getProcessResponse blk process
    case response of
      Just (Left {}) -> removeProcess scheduler nid
      Just (Right (Failure {})) -> removeProcess scheduler nid
      _ -> return ()
    return response

killNode ::
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
  NodeId ->
  IO (ProcessResponse conProg symSemObj symVal conSemObj conVal matcher)
killNode scheduler nid = do
  process <- getProcess scheduler nid
  signalProcessGroup sigKILL (pgid process) `catchErrno` \err errno ->
    if errno == eSRCH then return () else throwIO err
  Just response <- _getNodeResponse scheduler True nid
  return response

checkResponse ::
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
  NodeId ->
  IO (Maybe NodeAction)
checkResponse scheduler@Scheduler {..} nid = do
  response <- _getNodeResponse scheduler False nid
  case response of
    Nothing -> return Nothing
    Just response -> Just <$> nodeTransition scheduler nid response

killIfTimeout ::
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
  NodeId ->
  IO (Maybe NodeAction)
killIfTimeout scheduler@Scheduler {..} nid = do
  timeout <- getCurrentTimeout scheduler nid
  elapsedTime <- getCurrentElapsedTime scheduler nid
  if nominalDiffTimeToSeconds elapsedTime > fromIntegral timeout
    then do
      logMultiLineDoc (logger config) NOTICE $
        "Node " <> pformat nid <> " timed out, killing it."
      response <- killNode scheduler nid
      Just <$> nodeTransition scheduler nid response
    else return Nothing

killIfDividedChildrenAllStarted ::
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
  NodeId ->
  IO (Maybe NodeAction)
killIfDividedChildrenAllStarted scheduler@Scheduler {..} nid = do
  status <- getStatus scheduler nid
  if statusTypeIsRunning (statusType status) && statusType status /= StatusRefining
    then do
      dcTree' <- readIORef dcTree
      let children = nodeDividedChildren dcTree' nid
      case children of
        Just children | not (HS.null children) -> do
          let childrenList = HS.toList children
          childrenStatuses <- traverse (getStatus scheduler) childrenList
          let numRunning = length $ filter (statusTypeIsRunning . statusType) childrenStatuses
          let notYetStartedNodes =
                fmap fst $
                  filter (statusTypeIsNotYetStarted . statusType . snd) $
                    zip childrenList childrenStatuses
          case notYetStartedNodes of
            [] | numRunning <= 2 -> do
              logMultiLineDoc (logger config) NOTICE $
                "Node "
                  <> pformat nid
                  <> " has all its divided children started, and have no "
                  <> "more than 2 running children, killing it."
              response <- killNode scheduler nid
              Just <$> nodeTransition scheduler nid response
            [child] | numRunning <= 1 -> do
              logMultiLineDoc (logger config) NOTICE $
                "Node "
                  <> pformat nid
                  <> " has only one pending divided child, and have no "
                  <> "more than 1 running children, killing it and start the "
                  <> "child."
              _startNode scheduler child
              modifyIORef' nodeQueue $ Q.delete child
              response <- killNode scheduler nid
              Just <$> nodeTransition scheduler nid response
            _ -> return Nothing
        _ -> return Nothing
    else return Nothing

_killInferredFailure ::
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
  NodeId ->
  IO ()
_killInferredFailure scheduler@Scheduler {..} nid = do
  cpid <- HM.lookup nid <$> readIORef nodeToProcess
  case cpid of
    Nothing -> modifyIORef' nodeQueue $ Q.delete nid
    Just cpid -> do
      process <- getProcessByCPid scheduler cpid
      signalProcessGroup sigKILL (pgid process) `catchErrno` \err errno ->
        if errno == eSRCH then return () else throwIO err
      nodeInferFailureTransition scheduler nid
      _ <- _getNodeResponse scheduler True nid
      return ()

_setInferredFailure ::
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
  NodeId ->
  IO ()
_setInferredFailure scheduler@Scheduler {..} nid = do
  status <- getStatus scheduler nid
  case status of
    NodeInferredFailure -> error "Should not happen"
    NodeTerminated {} -> nodeInferFailureTransition scheduler nid
    NodeNotYetStarted -> do
      logMultiLineDoc (logger config) NOTICE $
        "Node " <> pformat nid <> " inferred failure, remove from the queue."
      modifyIORef' nodeQueue $ Q.delete nid
      nodeInferFailureTransition scheduler nid
    NodeFailed {} -> return ()
    NodeSucceeded {} -> return ()
    NodeStarted {} -> _killInferredFailure scheduler nid
    NodeViable {} -> _killInferredFailure scheduler nid
    NodeRefining {} -> _killInferredFailure scheduler nid
  return ()

_startNode ::
  forall sketchSpec sketch conProg costObj cost symSemObj symVal conSemObj conVal matcher.
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
  NodeId ->
  IO ()
_startNode
  scheduler@Scheduler {config = SchedulerConfig {..}, ..}
  nid = do
    sketchSpec <- getSketchTable scheduler nid
    status <- getStatus scheduler nid
    unless (statusTypeIsNotYetStarted (statusType status)) $ error "Should not happen"
    knownMinimalCost <- readIORef currentMinimalCost
    originalProcesses <- readIORef processes

    process <-
      runRequestInSubProcess
        solverConfig
        ( ProcessConfig
            { costObj = costObj,
              sketchSpec,
              sketchSymbol = synthesisSketchSymbol,
              verifiers = verifiers,
              logConfig = logConfig,
              nodeId = nid,
              easySketchFromFastResult = easySketchFromFastResult,
              transcriptSMT = transcriptSMT,
              doDeadCodeElimination = doDeadCodeElimination,
              exactCost,
              initialCost = knownMinimalCost,
              easySynthTimeout = fastTrackTimeoutSeconds,
              countNumProgsEvidence
            } ::
            ProcessConfig
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
    let CPid cpid = pid process
    let newProcesses = HM.insert cpid process originalProcesses
    queue <- readIORef nodeQueue
    priority <- getPriority scheduler nid
    timeoutSeconds <- getCurrentTimeout scheduler nid
    logMultiLineDoc logger NOTICE $
      "Starting node "
        <> pformat nid
        <> ", priority: "
        <> pformat priority
        <> ", timeout: "
        <> pformat timeoutSeconds
        <> "s, queue size: "
        <> pformat (Q.size queue)
        <> ", cost: "
        <> pformat knownMinimalCost
    writeIORef processes $! newProcesses
    nodeStartTransition scheduler nid
    modifyIORef' processToNode $ HM.insert cpid nid
    modifyIORef' nodeToProcess $ HM.insert nid cpid

resetIfJustStarted ::
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
  NodeId ->
  IO ()
resetIfJustStarted scheduler@Scheduler {..} nid = do
  status <- getStatus scheduler nid
  case status of
    NodeStarted {} -> do
      elapsedTime <- getCurrentElapsedTime scheduler nid
      when
        (elapsedTime < fromIntegral (restartRunningTimeThresholdSeconds config))
        $ do
          logMultiLineDoc (logger config) NOTICE $
            "Resetting node "
              <> pformat nid
              <> ", which have run for "
              <> viaShow elapsedTime
              <> " with new cost."
          _ <- killNode scheduler nid
          nodeResetTransition scheduler nid
          _startNode scheduler nid
    _ -> return ()

_startQueuedImpl ::
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
  IO ()
_startQueuedImpl scheduler@Scheduler {..} = do
  nodeQueue' <- readIORef nodeQueue
  nodeSplitQueue' <- readIORef nodeSplitQueue
  runningNum <- getNumRunningProcess scheduler
  let minQueuedDepth = Q.minQueuedDepth nodeQueue'
  (needSplit, pickBiased) <- case minQueuedDepth of
    Nothing -> do
      rand <- uniformRM (0, 1) randGen
      if Q.null nodeSplitQueue'
        then return (False, False)
        else do
          logMultiLineDoc (logger config) NOTICE "Empty queue, split a node"
          return (True, rand < biasedDrawProbability config)
    Just minQueuedDepth -> do
      firstNotFullySplitDepth' <- getFirstNotFullySplitDepth scheduler
      if minQueuedDepth > firstNotFullySplitDepth' + 1
        then do
          unless (Q.null nodeSplitQueue') $
            logMultiLineDoc (logger config) NOTICE $
              "Queue is not empty, but the minimum queued depth ("
                <> pformat minQueuedDepth
                <> ") is greater than the first not fully split depth plus 1 ("
                <> pformat (firstNotFullySplitDepth' + 1)
                <> "), split a node at depth "
                <> pformat firstNotFullySplitDepth'
          return (True, False)
        else return (False, False)

  -- Handle empty queue or need for splitting
  when needSplit $ do
    -- First try to use the split queue if it's not empty
    (splitNodeId, newSplitQueue) <-
      if pickBiased
        then do
          (_, splitNodeId, newSplitQueue) <- Q.popBiasedMin nodeSplitQueue'
          return (splitNodeId, newSplitQueue)
        else do
          (_, splitNodeId, newSplitQueue) <- Q.popSimpleMin nodeSplitQueue'
          return (splitNodeId, newSplitQueue)
    writeIORef nodeSplitQueue $! newSplitQueue
    info <- getNodeInfo scheduler splitNodeId
    if nodeSplitted info
      then error "Should not happen"
      else do
        depth <- getDepth scheduler splitNodeId
        priority <- getPriority scheduler splitNodeId
        logMultiLineDoc (logger config) NOTICE $
          "Splitting node "
            <> pformat splitNodeId
            <> ", depth: "
            <> pformat depth
            <> ", split priority: "
            <> pformat priority
            <> ", pickBiased: "
            <> pformat pickBiased
        void $ splitNode scheduler False splitNodeId

  -- Start nodes if we have capacity
  when (runningNum < parallelism config && not (Q.null nodeQueue')) $ do
    (priority, nodeId, biased, nodeQueue') <- Q.popMin randGen nodeQueue'
    writeIORef nodeQueue $! nodeQueue'
    logMultiLineDoc (logger config) NOTICE $
      "Drew node "
        <> pformat nodeId
        <> ", biased: "
        <> pformat biased
        <> ", priority: "
        <> pformat priority

    curDcTree <- readIORef dcTree
    if nodeFailed curDcTree nodeId
      then do
        error "Should not happen"
        logMultiLineDoc (logger config) NOTICE $
          "Node " <> pformat nodeId <> " is already inferred to fail."
        nodeInferFailureTransition scheduler nodeId
      else _startNode scheduler nodeId
    _startQueuedImpl scheduler

startQueued ::
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
  IO ()
startQueued scheduler = do
  stopped <- readIORef (stopped scheduler)
  unless stopped $ _startQueuedImpl scheduler

refineNode ::
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
  NodeId ->
  Maybe Int ->
  IO ()
refineNode scheduler@Scheduler {..} nid bestCostKnowledge = do
  status <- getStatus scheduler nid
  if not (statusTypeIsRunning (statusType status))
    then
      logMultiLineDoc (logger config) NOTICE $
        "No need to refine node " <> pformat nid <> ", as it is not running now."
    else do
      timeout <- getCurrentTimeout scheduler nid
      currentCost <- getCurrentMinimalCost scheduler
      case (currentCost, bestCostKnowledge) of
        (Nothing, Nothing) -> return ()
        (Nothing, Just _) -> error "Should not happen"
        (Just currentCost, _) -> do
          when
            ( isNothing bestCostKnowledge
                || fromJust bestCostKnowledge > currentCost
            )
            $ logMultiLineDoc (logger config) NOTICE
            $ "Refine node "
              <> pformat nid
              <> " with cost "
              <> pformat currentCost
              <> " and timeout "
              <> pformat timeout
              <> "s"
      process <- getProcess scheduler nid
      sendNewMinimalCost currentCost process

markFailure ::
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
  NodeId ->
  IO ()
markFailure scheduler@Scheduler {..} nid = do
  dcTree' <- readIORef dcTree
  let (inferredFailure, newDcTree) = markNodeFailed dcTree' nid
  mapM_ (_setInferredFailure scheduler) inferredFailure
  writeIORef dcTree newDcTree

markSuccess ::
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
  NodeId ->
  IO ()
markSuccess scheduler@Scheduler {..} nid = do
  priority <- getPriority scheduler nid
  let newPriority = priority {Q.knownWorking = True}
  when (newPriority /= priority) $
    logMultiLineDoc (logger config) NOTICE $
      "Marked node " <> pformat nid <> " as success"
  setPriority scheduler nid newPriority
  elapsedTime <- getCurrentElapsedTime scheduler nid
  let newTimeout = successNodeNewTimeoutSeconds config + round elapsedTime
  setTimeout scheduler nid newTimeout

_markAncestorKnownWorking ::
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
  NodeId ->
  NodeId ->
  IO ()
_markAncestorKnownWorking scheduler@Scheduler {..} ancestorId nid = do
  ancestorDepth <- getDepth scheduler ancestorId
  depth <- getDepth scheduler nid
  priority <- getPriority scheduler nid
  let newPriority =
        priority
          { Q.knownWorkingAncestorDistance =
              case Q.knownWorkingAncestorDistance priority of
                Nothing -> Just $ depth - ancestorDepth
                Just d -> Just $ min d (depth - ancestorDepth)
          }
  when (newPriority /= priority) $
    logMultiLineDoc (logger config) NOTICE $
      "Marked node " <> pformat nid <> " as an ancestor succeeded"
  setPriority scheduler nid newPriority

_markAncestorSiblingKnownWorking ::
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
  NodeId ->
  IO ()
_markAncestorSiblingKnownWorking scheduler@Scheduler {..} nid = do
  priority <- getPriority scheduler nid
  let newPriority = priority {Q.ancestorSiblingKnownWorking = True}
  when (newPriority /= priority) $
    logMultiLineDoc (logger config) NOTICE $
      "Marked node " <> pformat nid <> " as an ancestor sibling succeeded"
  setPriority scheduler nid newPriority

markAllChildrenSuccess ::
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
  NodeId ->
  IO ()
markAllChildrenSuccess scheduler@Scheduler {..} nid = do
  markSuccess scheduler nid
  dcTree <- readIORef dcTree
  _markAncestorKnownWorking scheduler nid nid
  mapM_ (_markAncestorKnownWorking scheduler nid) $ allChildrenNodes dcTree nid

markAllSiblingChildrenSuccess ::
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
  NodeId ->
  IO ()
markAllSiblingChildrenSuccess scheduler@Scheduler {..} nid = do
  markSuccess scheduler nid
  dcTree <- readIORef dcTree
  mapM_ (_markAncestorSiblingKnownWorking scheduler) $ allSiblingNodes dcTree nid
