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
  )
where

import Control.Exception (throwIO)
import Control.Monad (unless, void, when)
import qualified Data.HashMap.Strict as HM
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.List (sortOn)
import Data.Maybe (fromJust, isNothing)
import Data.Time
  ( nominalDiffTimeToSeconds,
  )
import Foreign.C (eSRCH)
import Grisette
  ( PPrint (pformat),
    viaShow,
  )
import qualified Grisette.Lib.Synth.Reasoning.Parallel.BiasedQueue as Q
import Grisette.Lib.Synth.Reasoning.Parallel.DCTree
  ( NodeId,
    allChildrenNodes,
    allSiblingNodes,
    markNodeFailed,
    nodeFailed,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.NodeStatus
  ( NodeAction,
    NodeStatus
      ( NodeFailed,
        NodeFastTrackEasySynthFailure,
        NodeFastTrackRefining,
        NodeFastTrackViable,
        NodeInferredFailure,
        NodeNotYetStarted,
        NodeSlowTrackRefining,
        NodeStarted,
        NodeSucceeded,
        NodeTerminated
      ),
    nodeStatusIsNotYetStarted,
    nodeStatusIsRunning,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Process
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
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Config
  ( ProcessSchedulerConfig
      ( ProcessSchedulerConfig,
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
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Scheduler
  ( NodeInfo (nodeSplitted),
    ProcessScheduler
      ( ProcessScheduler,
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
    getCurrentElapsedTime,
    getCurrentMinimalCost,
    getCurrentTimeout,
    getDepth,
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

_getNodeResponse ::
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
  Bool ->
  NodeId ->
  IO
    ( Maybe
        (ProcessResponse conProg symSemObj symVal conSemObj conVal matcher)
    )
_getNodeResponse
  scheduler@ProcessScheduler {config = ProcessSchedulerConfig {..}}
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
  IO (ProcessResponse conProg symSemObj symVal conSemObj conVal matcher)
killNode scheduler nid = do
  process <- getProcess scheduler nid
  signalProcessGroup sigKILL (pgid process) `catchErrno` \err errno ->
    if errno == eSRCH then return () else throwIO err
  Just response <- _getNodeResponse scheduler True nid
  return response

checkResponse ::
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
  IO (Maybe NodeAction)
checkResponse scheduler@ProcessScheduler {..} nid = do
  response <- _getNodeResponse scheduler False nid
  case response of
    Nothing -> return Nothing
    Just response -> Just <$> nodeTransition scheduler nid response

killIfTimeout ::
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
  IO (Maybe NodeAction)
killIfTimeout scheduler@ProcessScheduler {..} nid = do
  timeout <- getCurrentTimeout scheduler nid
  elapsedTime <- getCurrentElapsedTime scheduler nid
  if nominalDiffTimeToSeconds elapsedTime > fromIntegral timeout
    then do
      response <- killNode scheduler nid
      Just <$> nodeTransition scheduler nid response
    else return Nothing

_killInferredFailure ::
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
_killInferredFailure scheduler@ProcessScheduler {..} nid = do
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
_setInferredFailure scheduler@ProcessScheduler {..} nid = do
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
    NodeFastTrackEasySynthFailure {} -> _killInferredFailure scheduler nid
    NodeFastTrackRefining {} -> _killInferredFailure scheduler nid
    NodeSlowTrackRefining {} -> _killInferredFailure scheduler nid
    NodeFastTrackViable {} -> _killInferredFailure scheduler nid
  return ()

_startNode ::
  forall sketchSpec sketch conProg costObj cost symSemObj symVal conSemObj conVal matcher.
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
_startNode
  scheduler@ProcessScheduler {config = ProcessSchedulerConfig {..}, ..}
  nid = do
    sketchSpec <- getSketchTable scheduler nid
    status <- getStatus scheduler nid
    unless (nodeStatusIsNotYetStarted status) $ error "Should not happen"
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
resetIfJustStarted scheduler@ProcessScheduler {..} nid = do
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
  IO ()
_startQueuedImpl scheduler@ProcessScheduler {..} = do
  nodeQueue' <- readIORef nodeQueue
  runningNum <- getNumRunningProcess scheduler
  when (Q.null nodeQueue') $ do
    runningNodes' <- HM.keys <$> readIORef nodeToProcess
    nodeDepths <- traverse (getDepth scheduler) runningNodes'
    let runningNodes = fmap fst $ sortOn snd $ zip runningNodes' nodeDepths
    let go [] = return ()
        go (nid : rest) = do
          info <- getNodeInfo scheduler nid
          if nodeSplitted info
            then go rest
            else do
              logMultiLineDoc (logger config) NOTICE $
                "Empty queue, splitting node " <> pformat nid <> "."
              void $ splitNode scheduler False nid
    go runningNodes
  when (runningNum < parallelism config && not (Q.null nodeQueue')) $ do
    (nodeId, biased, nodeQueue') <- Q.popMin randGen nodeQueue'
    writeIORef nodeQueue $! nodeQueue'
    logMultiLineDoc (logger config) NOTICE $
      "Drew node " <> pformat nodeId <> ", biased: " <> pformat biased

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
  IO ()
startQueued scheduler = do
  stopped <- readIORef (stopped scheduler)
  unless stopped $ _startQueuedImpl scheduler

refineNode ::
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
  Maybe Int ->
  IO ()
refineNode scheduler@ProcessScheduler {..} nid bestCostKnowledge = do
  status <- getStatus scheduler nid
  if not (nodeStatusIsRunning status)
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
markFailure scheduler@ProcessScheduler {..} nid = do
  dcTree' <- readIORef dcTree
  let (inferredFailure, newDcTree) = markNodeFailed dcTree' nid
  mapM_ (_setInferredFailure scheduler) inferredFailure
  writeIORef dcTree newDcTree

markSuccess ::
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
markSuccess scheduler@ProcessScheduler {..} nid = do
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
_markAncestorKnownWorking scheduler@ProcessScheduler {..} nid = do
  priority <- getPriority scheduler nid
  let newPriority = priority {Q.ancestorKnownWorking = True}
  when (newPriority /= priority) $
    logMultiLineDoc (logger config) NOTICE $
      "Marked node " <> pformat nid <> " as an ancestor succeeded"
  setPriority scheduler nid newPriority

_markAncestorSiblingKnownWorking ::
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
_markAncestorSiblingKnownWorking scheduler@ProcessScheduler {..} nid = do
  priority <- getPriority scheduler nid
  let newPriority = priority {Q.ancestorSiblingKnownWorking = True}
  when (newPriority /= priority) $
    logMultiLineDoc (logger config) NOTICE $
      "Marked node " <> pformat nid <> " as an ancestor sibling succeeded"
  setPriority scheduler nid newPriority

markAllChildrenSuccess ::
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
markAllChildrenSuccess scheduler@ProcessScheduler {..} nid = do
  markSuccess scheduler nid
  dcTree <- readIORef dcTree
  _markAncestorKnownWorking scheduler nid
  mapM_ (_markAncestorKnownWorking scheduler) $ allChildrenNodes dcTree nid

markAllSiblingChildrenSuccess ::
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
markAllSiblingChildrenSuccess scheduler@ProcessScheduler {..} nid = do
  markSuccess scheduler nid
  dcTree <- readIORef dcTree
  mapM_ (_markAncestorSiblingKnownWorking scheduler) $ allSiblingNodes dcTree nid
