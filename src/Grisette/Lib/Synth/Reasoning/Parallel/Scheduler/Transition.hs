{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Transition
  ( nodeTransition,
    nodeResetTransition,
    nodeStartTransition,
    nodeInferFailureTransition,
  )
where

import Control.Monad (unless)
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef)
import Data.String (IsString (fromString))
import Data.Time
  ( diffUTCTime,
    getCurrentTime,
  )
import GHC.Stack (HasCallStack)
import Grisette
  ( PPrint (pformat),
    nest,
    vsep,
  )
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
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.NodeState
  ( NodeState
      ( NodeState,
        nodeResponseReverseLog,
        nodeStartTime
      ),
    nodeStateInferFailureTransition,
    nodeStateResetTransition,
    nodeStateStartTransition,
    nodeStateTransition,
    pformatNodeStateSummary,
    pformatNodeStateSummaryWithElapsedTime,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.NodeStatus (NodeAction)
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Process
  ( ProcessResponse,
    processResponseIsGotExample,
    processResponseNewCost,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Scheduler
  ( Scheduler
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
    updateCurrentMinimalCost,
    updateNodeState,
  )
import Grisette.Lib.Synth.Util.Logging (logMultiLineDoc)
import Grisette.Lib.Synth.Util.Show (showDiffTime)
import System.Log.Logger (Priority (NOTICE))

nodeTransition ::
  (HasCallStack) =>
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
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher ->
  IO NodeAction
nodeTransition
  scheduler@Scheduler
    { config = SchedulerConfig {..},
      ..
    }
  nid
  response = do
    updateCurrentMinimalCost scheduler $ processResponseNewCost response
    nodeStates' <- readIORef nodeStates
    case HM.lookup nid nodeStates' of
      Just state@NodeState {nodeStartTime = Just startTime} -> do
        curTime <- getCurrentTime
        let elapsedTime = diffUTCTime curTime startTime

        (newState, nextStep) <- nodeStateTransition curTime state response
        unless (processResponseIsGotExample response) $ do
          case nodeResponseReverseLog state of
            [] ->
              logMultiLineDoc logger NOTICE $
                nest 2 $
                  vsep
                    [ "Node "
                        <> pformat nid
                        <> " accepted initial response (elapsed time: "
                        <> fromString (showDiffTime elapsedTime)
                        <> "): ",
                      pformat response
                    ]
            ((t, _) : _) -> do
              let diffLastResponseTime = diffUTCTime curTime t
              logMultiLineDoc logger NOTICE $
                nest 2 $
                  vsep
                    [ "Node "
                        <> pformat nid
                        <> " accepted response (elapsed time: "
                        <> fromString (showDiffTime elapsedTime)
                        <> ", time since last response: "
                        <> fromString (showDiffTime diffLastResponseTime)
                        <> ", time since scheduler started: "
                        <> fromString (showDiffTime $ diffUTCTime curTime schedulerStartTime)
                        <> "): ",
                      pformat response
                    ]
          logMultiLineDoc logger NOTICE $
            vsep
              [ nest 2 $
                  vsep
                    [ "Node " <> pformat nid <> " transitioned from: ",
                      pformatNodeStateSummary state
                    ],
                nest 2 $
                  vsep
                    [ "to: ",
                      pformatNodeStateSummaryWithElapsedTime curTime newState
                    ]
              ]
        updateNodeState scheduler nid newState
        return nextStep
      _ -> error "Should not happen: node not found"

nodeInferFailureTransition ::
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
nodeInferFailureTransition scheduler@Scheduler {..} nid = do
  logMultiLineDoc (logger config) NOTICE $
    "Node " <> pformat nid <> " inferred failure"
  nodeStates' <- readIORef nodeStates
  case HM.lookup nid nodeStates' of
    Just state -> do
      newState <- nodeStateInferFailureTransition state
      updateNodeState scheduler nid newState
    Nothing -> error "Should not happen"

nodeStartTransition ::
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
nodeStartTransition scheduler@Scheduler {..} nid = do
  logMultiLineDoc (logger config) NOTICE $
    "Node " <> pformat nid <> " started"
  nodeStates' <- readIORef nodeStates
  case HM.lookup nid nodeStates' of
    Just state -> do
      newState <- nodeStateStartTransition state
      updateNodeState scheduler nid newState
    Nothing -> error "Should not happen"

nodeResetTransition ::
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
nodeResetTransition scheduler@Scheduler {..} nid = do
  logMultiLineDoc (logger config) NOTICE $
    "Node " <> pformat nid <> " reset"
  nodeStates' <- readIORef nodeStates
  case HM.lookup nid nodeStates' of
    Just state -> do
      newState <- nodeStateResetTransition state
      updateNodeState scheduler nid newState
    Nothing -> error "Should not happen"
