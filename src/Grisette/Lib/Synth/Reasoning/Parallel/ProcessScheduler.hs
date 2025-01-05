{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Grisette.Lib.Synth.Reasoning.Parallel.ProcessScheduler
  ( ProcessSchedulerConfig (..),
    runWithScheduler,
  )
where

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (throwIO)
import Control.Monad (filterM, guard, unless, void, when)
import Control.Monad.Extra (mapMaybeM, whileM)
import Data.Bifunctor (second)
import Data.Dynamic (toDyn)
import Data.Foldable (Foldable (toList))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (intercalate, sort, sortOn)
import Data.List.Extra (minimumOn)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Ratio ((%))
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Time
  ( NominalDiffTime,
    UTCTime,
    addUTCTime,
    diffUTCTime,
    getCurrentTime,
    nominalDiffTimeToSeconds,
  )
import Foreign.C (eBADF, eSRCH)
import GHC.Conc.Signal (setHandler)
import Graphics.Rendering.Chart.Backend.Cairo
  ( FileFormat (SVG),
    FileOptions (FileOptions),
    toFile,
  )
import Graphics.Rendering.Chart.Easy
  ( PointShape
      ( PointShapeCircle,
        PointShapeCross,
        PointShapePlus,
        PointShapePolygon,
        PointShapeStar
      ),
    aqua,
    black,
    blue,
    deeppink,
    dodgerblue,
    font_size,
    goldenrod,
    gray,
    green,
    layout_title,
    liftEC,
    line,
    mediumpurple,
    opaque,
    orange,
    plot,
    plot_annotation_style,
    plot_annotation_values,
    points,
    red,
    setColors,
    setShapes,
    yellowgreen,
    (.=),
  )
import Grisette (Doc, GrisetteSMTConfig, PPrint (pformat), nest, viaShow, vsep, (<+>))
import Grisette.Lib.Synth.Program.Choice.Counting
  ( CountNumProgs (countNumProgs),
    CountNumProgsEvidence (CountNumProgsEvidence),
    countNumChoicesWithEvidence,
    countNumProgsWithEvidence,
  )
import Grisette.Lib.Synth.Program.Choice.Split
  ( LowestSeqNum (lowestSeqNum),
    PartitionSpec (partitionSpec),
  )
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable)
import qualified Grisette.Lib.Synth.Reasoning.Parallel.BiasedQueue as Q
import Grisette.Lib.Synth.Reasoning.Parallel.DCTree
  ( DCTree,
    NodeId,
    allChildrenNodes,
    allSiblingNodes,
    emptyDCTree,
    insertRootSketches,
    insertSplittedSketches,
    leafNodes,
    markNodeFailed,
    nodeDepth,
    nodeDividedChildren,
    nodeFailed,
    numNodes,
    rootNodes,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.LogConfig (LogConfig (LogConfig), logRootDir)
import Grisette.Lib.Synth.Reasoning.Parallel.LogMultiLine (logMultiLineDoc)
import Grisette.Lib.Synth.Reasoning.Parallel.NodeState
  ( NodeState
      ( NodeState,
        nodeEndTime,
        nodeMajorResponseReverseLog,
        nodeResponseReverseLog,
        nodeStartTime,
        nodeStatus
      ),
    nodeStateCurrentElapsedTime,
    nodeStateInferFailureTransition,
    nodeStateMajorRelativeTimeLog,
    nodeStateNumCollectedExamples,
    nodeStateNumInProgressExamples,
    nodeStateResetTransition,
    nodeStateStartTransition,
    nodeStateTransition,
    pformatNodeStateSummary,
    pformatNodeStateSummaryWithElapsedTime,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.NodeStatus
  ( NodeAction
      ( CleanUpAndSplitSketch,
        MarkFailure,
        Refine,
        RefineAndSplitSketch
      ),
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
    nodeStatusBestProgWithCost,
    nodeStatusIsDetermined,
    nodeStatusIsFastSuccess,
    nodeStatusIsFastTerminated,
    nodeStatusIsFastTrackEasySynthFailure,
    nodeStatusIsFastTrackRefining,
    nodeStatusIsFastTrackViable,
    nodeStatusIsFastUnknown,
    nodeStatusIsFastUnsat,
    nodeStatusIsInferredFailure,
    nodeStatusIsJustStarted,
    nodeStatusIsNotYetStarted,
    nodeStatusIsRunning,
    nodeStatusIsSlowSuccess,
    nodeStatusIsSlowTerminated,
    nodeStatusIsSlowTrackRefining,
    nodeStatusIsSlowUnknown,
    nodeStatusIsSlowUnsat,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Process
  ( ConProgConstraint,
    Message (Failure),
    Process (pgid, pid, pipeRd, pipeWr),
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
    ProcessConstraint,
    ProcessResponse,
    TwoTrackVerifiers,
    getProcessResponse,
    processResponseIsFastTrackEasySynthFailure,
    processResponseIsFastTrackSuccess,
    processResponseIsFastTrackViable,
    processResponseIsGotExample,
    processResponseIsSlowTrackSuccess,
    processResponseNewCost,
    runRequestInSubProcess,
    sendNewMinimalCost,
  )
import Grisette.Lib.Synth.Util.Exception (catchErrno)
import Numeric (showFFloat)
import System.Exit (ExitCode (ExitSuccess))
import System.Log.Logger (Logger, Priority (DEBUG, NOTICE, WARNING))
import System.Posix
  ( CPid (CPid),
    closeFd,
    exitImmediately,
    sigHUP,
    sigINT,
    sigKILL,
    sigTERM,
    signalProcessGroup,
  )
import System.Random.Stateful
  ( AtomicGenM,
    StdGen,
    UniformRange (uniformRM),
    mkStdGen,
    newAtomicGenM,
  )

data
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
    matcher
  where
  ProcessSchedulerConfig ::
    ( ProcessConstraint
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
      ConProgConstraint conProg conOp conVarId conType
    ) =>
    { initialTimeoutSeconds :: Int,
      schedulerTimeoutSeconds :: Maybe Int,
      initialSplitRatio :: Int,
      solverConfig :: GrisetteSMTConfig,
      rootPriority :: Double,
      subNodePriorityMultiplier :: Double,
      subNodeRandomMultiplierRange :: (Double, Double),
      verifiers :: Logger -> TwoTrackVerifiers sketch conProg,
      countNumProgsEvidence ::
        Maybe (CountNumProgsEvidence (SymbolTable sketchSpec)),
      logConfig :: LogConfig,
      logger :: Logger,
      costObj :: costObj,
      parallelism :: Int,
      initialMinimalCost :: Maybe Int,
      targetCost :: Int,
      exactCost :: Maybe Int,
      restartRunningTimeThresholdSeconds :: Int,
      fastTrackTimeoutSeconds :: Int,
      easySketchFromFastResult ::
        Maybe (SymbolTable conProg -> SymbolTable sketchSpec),
      synthesisSketchSymbol :: T.Text,
      successNodeNewTimeoutSeconds :: Int,
      cmdline :: Maybe String,
      transcriptSMT :: Bool,
      doDeadCodeElimination :: Bool,
      schedulerRandomSeed :: Int,
      pollIntervalSeconds :: Double,
      biasedDrawProbability :: Double
    } ->
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
      matcher

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

_updateCurrentMinimalCost ::
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
_updateCurrentMinimalCost ProcessScheduler {..} newCost = do
  modifyIORef' currentMinimalCost $ \case
    Nothing -> newCost
    Just oldCost -> case newCost of
      Nothing -> return oldCost
      Just newCost -> Just $ min oldCost newCost

_showFloat :: (RealFloat a) => a -> String
_showFloat x = showFFloat (Just 2) x []

_showDiffTime :: NominalDiffTime -> String
_showDiffTime dt = _showFloat (realToFrac dt) <> "s"

nodeTransition ::
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
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher ->
  IO NodeAction
nodeTransition
  scheduler@ProcessScheduler
    { config = ProcessSchedulerConfig {..},
      ..
    }
  nid
  response = do
    _updateCurrentMinimalCost scheduler $ processResponseNewCost response
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
                        <> fromString (_showDiffTime elapsedTime)
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
                        <> fromString (_showDiffTime elapsedTime)
                        <> ", time since last response: "
                        <> fromString (_showDiffTime diffLastResponseTime)
                        <> ", time since scheduler started: "
                        <> fromString (_showDiffTime $ diffUTCTime curTime schedulerStartTime)
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
        modifyIORef' nodeStates $ HM.insert nid newState
        return nextStep
      _ -> error "Should not happen: node not found"

nodeInferFailureTransition ::
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
nodeInferFailureTransition ProcessScheduler {..} nid = do
  logMultiLineDoc (logger config) NOTICE $
    "Node " <> pformat nid <> " inferred failure"
  nodeStates' <- readIORef nodeStates
  case HM.lookup nid nodeStates' of
    Just state -> do
      newState <- nodeStateInferFailureTransition state
      modifyIORef' nodeStates $ HM.insert nid newState
    Nothing -> error "Should not happen"

nodeStartTransition ::
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
nodeStartTransition ProcessScheduler {..} nid = do
  logMultiLineDoc (logger config) NOTICE $
    "Node " <> pformat nid <> " started"
  nodeStates' <- readIORef nodeStates
  case HM.lookup nid nodeStates' of
    Just state -> do
      newState <- nodeStateStartTransition state
      modifyIORef' nodeStates $ HM.insert nid newState
    Nothing -> error "Should not happen"

nodeResetTransition ::
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
nodeResetTransition ProcessScheduler {..} nid = do
  logMultiLineDoc (logger config) NOTICE $
    "Node " <> pformat nid <> " reset"
  nodeStates' <- readIORef nodeStates
  case HM.lookup nid nodeStates' of
    Just state -> do
      newState <- nodeStateResetTransition state
      modifyIORef' nodeStates $ HM.insert nid newState
    Nothing -> error "Should not happen"

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

getNodeResponse ::
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
getNodeResponse
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
  response <- getNodeResponse scheduler False nid
  case response of
    Nothing -> return Nothing
    Just response -> Just <$> nodeTransition scheduler nid response

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
  Just response <- getNodeResponse scheduler True nid
  return response

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

killInferredFailure ::
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
killInferredFailure scheduler@ProcessScheduler {..} nid = do
  cpid <- HM.lookup nid <$> readIORef nodeToProcess
  case cpid of
    Nothing -> modifyIORef' nodeQueue $ Q.delete nid
    Just cpid -> do
      process <- getProcessByCPid scheduler cpid
      signalProcessGroup sigKILL (pgid process) `catchErrno` \err errno ->
        if errno == eSRCH then return () else throwIO err
      nodeInferFailureTransition scheduler nid
      _ <- getNodeResponse scheduler True nid
      return ()

setInferredFailure ::
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
setInferredFailure scheduler@ProcessScheduler {..} nid = do
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
    NodeStarted {} -> killInferredFailure scheduler nid
    NodeFastTrackEasySynthFailure {} -> killInferredFailure scheduler nid
    NodeFastTrackRefining {} -> killInferredFailure scheduler nid
    NodeSlowTrackRefining {} -> killInferredFailure scheduler nid
    NodeFastTrackViable {} -> killInferredFailure scheduler nid
  return ()

-- Actions

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
  mapM_ (setInferredFailure scheduler) inferredFailure
  writeIORef dcTree newDcTree

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

markAncestorKnownWorking ::
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
markAncestorKnownWorking scheduler@ProcessScheduler {..} nid = do
  priority <- getPriority scheduler nid
  let newPriority = priority {Q.ancestorKnownWorking = True}
  when (newPriority /= priority) $
    logMultiLineDoc (logger config) NOTICE $
      "Marked node " <> pformat nid <> " as an ancestor succeeded"
  setPriority scheduler nid newPriority

markAncestorSiblingKnownWorking ::
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
markAncestorSiblingKnownWorking scheduler@ProcessScheduler {..} nid = do
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
  markAncestorKnownWorking scheduler nid
  mapM_ (markAncestorKnownWorking scheduler) $ allChildrenNodes dcTree nid

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
  mapM_ (markAncestorSiblingKnownWorking scheduler) $ allSiblingNodes dcTree nid

addSubSketches ::
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
  Maybe NodeId ->
  HM.HashMap (SymbolTable sketchSpec) Double ->
  IO [NodeId]
addSubSketches
  scheduler@ProcessScheduler {config = ProcessSchedulerConfig {..}, ..}
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
        Nothing -> return $ Q.Priority rootPriority 1 False False False
    let childrenBasePriority =
          subNodePriorityMultiplier * Q.basePriority parentBasePriority
    writeIORef dcTree newDcTree
    let initialState = NodeState NodeNotYetStarted Nothing Nothing [] [] []
    modifyIORef' nodeStates $ \nodeStates' ->
      foldr (`HM.insert` initialState) nodeStates' $ toList sketchesToNodeId
    let taskPriority nid =
          parentBasePriority
            { Q.basePriority = childrenBasePriority,
              Q.randomPriority = nodeIdPriorities HM.! nid,
              Q.knownWorking = False
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
                then
                  setInferredFailure scheduler nid >> return [nid]
                else return []
          )
          nodeIds

    numsInfo <- case countNumProgsEvidence of
      Just countNumProgsEvidence -> do
        let nodeIdToNumChoices =
              HM.fromList
                $ fmap
                  ( \(sketch, nid) ->
                      (nid, countNumChoicesWithEvidence countNumProgsEvidence sketch)
                  )
                $ HM.toList sketchesToNodeId
        let nodeIdToNumWellTyped =
              HM.fromList
                $ fmap
                  ( \(sketch, nid) ->
                      (nid, countNumProgsWithEvidence countNumProgsEvidence sketch)
                  )
                $ HM.toList sketchesToNodeId
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
  SymbolTable sketchSpec ->
  IO () -- NodeId
addRootSketch
  scheduler@ProcessScheduler {config = ProcessSchedulerConfig {..}}
  sketch = do
    logMultiLineDoc logger NOTICE $
      nest 2 $
        vsep ["Adding root sketch: ", pformat sketch]
    void $ addSubSketches scheduler Nothing (HM.fromList [(sketch, 1)])

splitNode ::
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
  IO [NodeId]
splitNode
  scheduler@ProcessScheduler {config = ProcessSchedulerConfig {..}}
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
              addSubSketches
                scheduler
                (Just nodeId)
                splittedSketchesWithPriority
            setIsSplitted scheduler nodeId True
            return splittedNodeIds

restartIfJustStarted ::
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
restartIfJustStarted scheduler@ProcessScheduler {..} nid = do
  status <- getStatus scheduler nid
  case status of
    NodeStarted {} -> do
      elapsedTime <- getCurrentElapsedTime scheduler nid
      when
        (elapsedTime < fromIntegral (restartRunningTimeThresholdSeconds config))
        $ do
          logMultiLineDoc (logger config) NOTICE $
            "Restarting node "
              <> pformat nid
              <> ", which have run for "
              <> viaShow elapsedTime
              <> " with new cost."
          process <- getProcess scheduler nid
          signalProcessGroup sigKILL (pgid process)
          _ <- getNodeResponse scheduler True nid
          nodeResetTransition scheduler nid
          startNode scheduler nid
    _ -> return ()

startNode ::
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
startNode
  scheduler@ProcessScheduler {config = ProcessSchedulerConfig {..}, ..}
  nid = do
    nodeInfo' <- readIORef nodeInfo
    let NodeInfo {..} = nodeInfo' HM.! nid
    status <- getStatus scheduler nid
    unless (nodeStatusIsNotYetStarted status) $ error "Should not happen"
    knownMinimalCost <- readIORef currentMinimalCost
    originalProcesses <- readIORef processes

    process <-
      runRequestInSubProcess
        solverConfig
        ( ProcessConfig
            { costObj = costObj,
              sketchSpec = nodeSketchTable,
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
    logMultiLineDoc logger NOTICE $
      "Starting node "
        <> pformat nid
        <> ", priority: "
        <> pformat nodePriority
        <> ", timeout: "
        <> pformat nodeTimeoutSeconds
        <> "s, queue size: "
        <> pformat (Q.size queue)
        <> ", cost: "
        <> pformat knownMinimalCost
    writeIORef processes $! newProcesses
    nodeStartTransition scheduler nid
    modifyIORef' processToNode $ HM.insert cpid nid
    modifyIORef' nodeToProcess $ HM.insert nid cpid

startQueuedImpl ::
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
startQueuedImpl scheduler@ProcessScheduler {..} = do
  nodeQueue' <- readIORef nodeQueue
  -- sketchQueue' <- readIORef sketchQueue
  -- sketchSimpleQueue' <- readIORef sketchSimpleQueue
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
      else startNode scheduler nodeId
    startQueuedImpl scheduler

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
  unless stopped $ startQueuedImpl scheduler

runAction ::
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
  NodeAction ->
  IO ()
runAction scheduler nid action =
  case action of
    MarkFailure -> markFailure scheduler nid
    CleanUpAndSplitSketch -> void $ splitNode scheduler False nid
    Refine succeed nodeBestCostKnowledge -> do
      when succeed $ markSuccess scheduler nid
      refineNode scheduler nid nodeBestCostKnowledge
    RefineAndSplitSketch nodeBestCostKnowledge -> do
      markAllChildrenSuccess scheduler nid
      markAllSiblingChildrenSuccess scheduler nid
      splitNode scheduler True nid
      refineNode scheduler nid nodeBestCostKnowledge

step ::
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
step
  scheduler@ProcessScheduler {config = ProcessSchedulerConfig {..}, ..} = do
    startCost <- getCurrentMinimalCost scheduler
    curNodeToProcess <- readIORef nodeToProcess
    cancelledNodes <-
      fmap HM.fromList
        $ mapMaybeM
          ( \(nid, _) -> do
              fmap (nid,) <$> killIfTimeout scheduler nid
          )
        $ HM.toList curNodeToProcess
    otherNodes <-
      fmap HM.fromList
        $ mapMaybeM
          ( \(nid, _) ->
              case HM.lookup nid cancelledNodes of
                Just _ -> return Nothing
                Nothing -> fmap (nid,) <$> checkResponse scheduler nid
          )
        $ HM.toList curNodeToProcess

    let allNodes = HM.union cancelledNodes otherNodes
    -- do the next step
    HM.traverseWithKey (runAction scheduler) allNodes
    -- remove failure
    curDcTree <- readIORef dcTree
    mapM_
      (\nid -> when (nodeFailed curDcTree nid) $ error "Should not happen")
      . HM.keys
      =<< readIORef nodeToProcess
    -- restart just started
    cost <- getCurrentMinimalCost scheduler
    case cost of
      Just curCost
        | curCost > targetCost ->
            when (isNothing startCost || cost < startCost) $
              mapM_ (restartIfJustStarted scheduler) $
                HM.keys allNodes
      _ -> return ()
    startQueued scheduler

shutdownScheduler ::
  Bool ->
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
shutdownScheduler printInfo scheduler@ProcessScheduler {..} = do
  writeIORef stopped True
  writeIORef nodeQueue (Q.empty (biasedDrawProbability config))
  when printInfo $
    logMultiLineDoc (logger config) NOTICE "Shutting down scheduler."
  nodeToProcess' <- readIORef nodeToProcess
  mapM_ (killNode scheduler) $ HM.keys nodeToProcess'
  when printInfo $
    logMultiLineDoc (logger config) NOTICE "All processes cancelled."
  when printInfo $
    logMultiLineDoc (logger config) NOTICE "Shutting down scheduler done."

installSchedulerSignalHandler ::
  LogConfig ->
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
installSchedulerSignalHandler logConfig scheduler@ProcessScheduler {..} = do
  let handler printInfo = do
        takeMVar queueLock
        -- TODO: implement
        results <- getParallelSynthesisResult scheduler
        printResults scheduler results
        writeResultsCSV (logRootDir logConfig <> "/results.csv") results scheduler
        shutdownScheduler printInfo scheduler
        -- TODO: implement
        (stats, layerStats) <- collectAllStats scheduler
        plotStatistics logConfig stats layerStats scheduler
        debugLogAllStats False scheduler
        debugLogAllStats True scheduler
        exitImmediately ExitSuccess
  setHandler sigINT $ Just (const $ handler True, toDyn ())
  setHandler sigTERM $ Just (const $ handler True, toDyn ())
  setHandler sigHUP $ Just (const $ handler False, toDyn ())
  return ()

initialSplit ::
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
initialSplit scheduler@ProcessScheduler {..} = do
  oldNodeInfo <- readIORef nodeInfo
  let workingList = HM.keys oldNodeInfo
  go workingList []
  where
    go [] [] = return ()
    go [] newWorkingList = go (reverse newWorkingList) []
    go (nid : rest) newWorkingList = do
      oldNodeInfo <- readIORef nodeInfo
      unless
        ( length oldNodeInfo
            >= initialSplitRatio config * parallelism config
        )
        $ do
          r <- splitNode scheduler False nid
          go rest $ r ++ newWorkingList

runWithScheduler ::
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
  [SymbolTable sketchSpec] ->
  IO ()
runWithScheduler
  config@ProcessSchedulerConfig {..}
  sketches = do
    scheduler <- newProcessScheduler config
    installSchedulerSignalHandler logConfig scheduler
    mapM_ (addRootSketch scheduler) sketches
    initialSplit scheduler
    iter <- newIORef 0

    curTime <- getCurrentTime
    let endTime = addUTCTime (maybe 0 fromIntegral schedulerTimeoutSeconds) curTime
    whileM $ do
      takeMVar $ queueLock scheduler
      step scheduler
      v <- getNumQueuedProcess scheduler
      x <- getNumRunningProcess scheduler
      minimalCost <- getCurrentMinimalCost scheduler
      when (isNothing minimalCost || minimalCost > Just targetCost) $
        startQueued scheduler
      -- TODO: implement
      -- dumpStates False scheduler
      threadDelay $ round $ pollIntervalSeconds * 1000000
      modifyIORef' iter (+ 1)
      curIter <- readIORef iter
      -- TODO: implement
      when (curIter `mod` 20 == 0) $ do
        (stats, layerStats) <- collectAllStats scheduler
        plotStatistics logConfig stats layerStats scheduler
      when (curIter `mod` 100 == 0) $
        debugLogAllStats False scheduler
      when (curIter `mod` 20 == 0) $
        debugLogAllStats True scheduler
      putMVar (queueLock scheduler) ()
      curTime <- getCurrentTime
      return $
        (v > 0 || x > 0)
          && (isNothing schedulerTimeoutSeconds || curTime < endTime)
          && ( isNothing minimalCost
                 || minimalCost > Just targetCost
             )
    -- TODO: implement
    results <- getParallelSynthesisResult scheduler
    printResults scheduler results
    writeResultsCSV (logRootDir logConfig <> "/results.csv") results scheduler
    shutdownScheduler True scheduler
    (stats, layerStats) <- collectAllStats scheduler
    plotStatistics logConfig stats layerStats scheduler
    debugLogAllStats False scheduler
    debugLogAllStats True scheduler

data ParallelSynthesisSolution conProg
  = ParallelSynthesisSolution
  { _nodeId :: NodeId,
    startTime :: UTCTime,
    resultTime :: UTCTime,
    _lastMsgTime :: UTCTime,
    _finished :: Bool,
    _program :: SymbolTable conProg
  }

data ParallelSynthesisNoSolutionResult = ParallelSynthesisNoSolutionResult
  { aggregatedTime :: UTCTime,
    numOfNodesInLattice :: Int,
    numOfUndeterminedLeaves :: Int,
    numOfRootPrograms :: Integer,
    numOfUndeterminedPrograms :: Integer,
    undeterminedRatio :: Double
  }

data ParallelSynthesisSolutionFoundResult conProg
  = ParallelSynthesisSolutionFoundResult
  { aggregatedTime :: UTCTime,
    initialCost :: Maybe Int,
    bestCost :: Int,
    solutions :: [ParallelSynthesisSolution conProg],
    numOfNodesInLattice :: Int,
    numOfUndeterminedLeaves :: Int,
    numOfRootPrograms :: Integer,
    numOfUndeterminedPrograms :: Integer,
    undeterminedRatio :: Double
  }

data ParallelSynthesisResult conProg
  = NoSolutionFound ParallelSynthesisNoSolutionResult
  | SolutionFound (ParallelSynthesisSolutionFoundResult conProg)

resultBestTime :: ParallelSynthesisResult conProg -> (UTCTime, UTCTime)
resultBestTime (NoSolutionFound ParallelSynthesisNoSolutionResult {..}) =
  (aggregatedTime, aggregatedTime)
resultBestTime (SolutionFound ParallelSynthesisSolutionFoundResult {..}) =
  bestTime solutions
  where
    bestTime = minimumOn snd . fmap (\x -> (startTime x, resultTime x))

resultNumOfNodesInLattice :: ParallelSynthesisResult conProg -> Int
resultNumOfNodesInLattice (NoSolutionFound ParallelSynthesisNoSolutionResult {..}) =
  numOfNodesInLattice
resultNumOfNodesInLattice (SolutionFound ParallelSynthesisSolutionFoundResult {..}) =
  numOfNodesInLattice

resultNumOfUndeterminedLeaves :: ParallelSynthesisResult conProg -> Int
resultNumOfUndeterminedLeaves (NoSolutionFound ParallelSynthesisNoSolutionResult {..}) =
  numOfUndeterminedLeaves
resultNumOfUndeterminedLeaves (SolutionFound ParallelSynthesisSolutionFoundResult {..}) =
  numOfUndeterminedLeaves

resultNumOfRootPrograms :: ParallelSynthesisResult conProg -> Integer
resultNumOfRootPrograms (NoSolutionFound ParallelSynthesisNoSolutionResult {..}) =
  numOfRootPrograms
resultNumOfRootPrograms (SolutionFound ParallelSynthesisSolutionFoundResult {..}) =
  numOfRootPrograms

resultNumOfUndeterminedPrograms :: ParallelSynthesisResult conProg -> Integer
resultNumOfUndeterminedPrograms (NoSolutionFound ParallelSynthesisNoSolutionResult {..}) =
  numOfUndeterminedPrograms
resultNumOfUndeterminedPrograms (SolutionFound ParallelSynthesisSolutionFoundResult {..}) =
  numOfUndeterminedPrograms

resultUndeterminedRatio :: ParallelSynthesisResult conProg -> Double
resultUndeterminedRatio (NoSolutionFound ParallelSynthesisNoSolutionResult {..}) =
  undeterminedRatio
resultUndeterminedRatio (SolutionFound ParallelSynthesisSolutionFoundResult {..}) =
  undeterminedRatio

resultAggregatedTime :: ParallelSynthesisResult conProg -> UTCTime
resultAggregatedTime (NoSolutionFound ParallelSynthesisNoSolutionResult {..}) =
  aggregatedTime
resultAggregatedTime (SolutionFound ParallelSynthesisSolutionFoundResult {..}) =
  aggregatedTime

getParallelSynthesisResult ::
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
  IO (ParallelSynthesisResult conProg)
getParallelSynthesisResult
  scheduler@ProcessScheduler
    { config = ProcessSchedulerConfig {..},
      ..
    } = do
    cost <- getCurrentMinimalCost scheduler
    curTime <- getCurrentTime
    nodeStates <- readIORef nodeStates
    dcTree <- readIORef dcTree
    let leaves = leafNodes dcTree
    let roots = rootNodes dcTree
    let numOfNodesInLattice = numNodes dcTree
    undeterminedLeaves <-
      filterM
        ( \nid -> do
            s <- getStatus scheduler nid
            return $ not $ nodeStatusIsDetermined s
        )
        $ HS.toList leaves
    let numOfUndeterminedLeaves = length undeterminedLeaves
    undeterminedSketches <-
      traverse (getSketchTable scheduler) undeterminedLeaves
    (numOfRootPrograms, totalNumOfUndeterminedPrograms, undeterminedRatio) <-
      case countNumProgsEvidence of
        Just CountNumProgsEvidence -> do
          let numOfPrograms = map countNumProgs undeterminedSketches
          let totalNumOfUndeterminedPrograms = sum numOfPrograms
          rootSketches <- traverse (getSketchTable scheduler) $ HS.toList roots
          let numOfRootPrograms = sum $ map countNumProgs rootSketches
          let undeterminedRatio =
                (fromIntegral totalNumOfUndeterminedPrograms :: Double)
                  / fromIntegral numOfRootPrograms
          return (numOfRootPrograms, totalNumOfUndeterminedPrograms, undeterminedRatio)
        Nothing -> return (-1, -1, 0 / 0)

    case cost of
      Nothing ->
        return $
          NoSolutionFound $
            ParallelSynthesisNoSolutionResult
              curTime
              numOfNodesInLattice
              numOfUndeterminedLeaves
              numOfRootPrograms
              totalNumOfUndeterminedPrograms
              undeterminedRatio
      Just cost -> do
        lst <- go cost $ HM.toList nodeStates
        if null lst
          then
            return $
              NoSolutionFound $
                ParallelSynthesisNoSolutionResult
                  curTime
                  numOfNodesInLattice
                  numOfUndeterminedLeaves
                  numOfRootPrograms
                  totalNumOfUndeterminedPrograms
                  undeterminedRatio
          else
            return $
              SolutionFound $
                ParallelSynthesisSolutionFoundResult
                  curTime
                  initialMinimalCost
                  cost
                  lst
                  numOfNodesInLattice
                  numOfUndeterminedLeaves
                  numOfRootPrograms
                  totalNumOfUndeterminedPrograms
                  undeterminedRatio
    where
      go _ [] = return []
      go cost ((nid, NodeState {..}) : rest) = do
        let best = nodeStatusBestProgWithCost nodeStatus
        case best of
          Just (curCost, prog) | cost == curCost -> do
            rest' <- go cost rest
            return $
              ParallelSynthesisSolution
                nid
                (fromJust nodeStartTime)
                ( fst $
                    head $
                      filter
                        ( \(_, r) ->
                            processResponseIsFastTrackSuccess r
                              || processResponseIsSlowTrackSuccess r
                        )
                        nodeMajorResponseReverseLog
                )
                ( case nodeEndTime of
                    Just t -> t
                    Nothing -> fst $ head nodeResponseReverseLog
                )
                (isJust nodeEndTime)
                prog
                : rest'
          _ -> go cost rest

showFloat :: (RealFloat a) => a -> String
showFloat x = showFFloat (Just 2) x []

showDiffTime :: NominalDiffTime -> String
showDiffTime dt = showFloat (realToFrac dt) <> "s"

printResults ::
  ProcessScheduler sketchSpec sketch conProg costObj cost symSemObj symVal conSemObj conVal matcher ->
  ParallelSynthesisResult conProg ->
  IO ()
printResults ProcessScheduler {config = ProcessSchedulerConfig {..}, ..} result = do
  let numOfNodesInLattice = resultNumOfNodesInLattice result
  let numOfUndeterminedLeaves = resultNumOfUndeterminedLeaves result
  let numOfUndeterminedPrograms = resultNumOfUndeterminedPrograms result
  let numOfRootPrograms = resultNumOfRootPrograms result
  let undeterminedRatio = resultUndeterminedRatio result
  case result of
    NoSolutionFound {} ->
      logMultiLineDoc logger WARNING $
        nest 2 $
          vsep
            [ "No solution found,",
              "Num of all nodes in lattice: " <> pformat numOfNodesInLattice,
              "Num of undetermined leaves: " <> pformat numOfUndeterminedLeaves,
              "Num of undetermined programs: " <> pformat numOfUndeterminedPrograms,
              "Num of root programs: " <> pformat numOfRootPrograms,
              "Undetermined ratio: " <> pformat undeterminedRatio
            ]
    SolutionFound ParallelSynthesisSolutionFoundResult {..} -> do
      let elapsedTime =
            diffUTCTime (resultAggregatedTime result) schedulerStartTime
      let (_, bestEndTime) = resultBestTime result
      let timeToBest = diffUTCTime bestEndTime schedulerStartTime
      logMultiLineDoc logger NOTICE $
        nest 2 $
          vsep $
            [ "Num of all nodes in lattice: " <> pformat numOfNodesInLattice,
              "Num of undetermined leaves: " <> pformat numOfUndeterminedLeaves,
              "Num of undetermined programs: " <> pformat numOfUndeterminedPrograms,
              "Num of root programs: " <> pformat numOfRootPrograms,
              "Undetermined ratio: " <> pformat undeterminedRatio,
              "Best solution found with cost "
                <> pformat bestCost
                <> " in "
                <> fromString (showDiffTime timeToBest)
                <> " (elapsed time: "
                <> fromString (showDiffTime elapsedTime)
                <> ")"
                <> ":"
            ]
              ++ fmap
                ( \(ParallelSynthesisSolution nid startTime lastResultTime lastMsgTime finished prog) ->
                    let elapsedTime = diffUTCTime lastMsgTime startTime
                        timeSinceSchedulerStart =
                          diffUTCTime lastMsgTime schedulerStartTime
                        elapsedTimeToBest =
                          diffUTCTime lastResultTime startTime
                        timeToBestSinceSchedulerStart =
                          diffUTCTime lastResultTime schedulerStartTime
                     in nest 2 $
                          vsep
                            [ "Node " <> pformat nid <> (if finished then "" else " (WIP)"),
                              "Elapsed time: "
                                <> fromString (showDiffTime elapsedTime),
                              "Time since scheduler start: "
                                <> fromString (showDiffTime timeSinceSchedulerStart),
                              "Time to best result: "
                                <> fromString (showDiffTime elapsedTimeToBest),
                              "Time to best result since scheduler start: "
                                <> fromString (showDiffTime timeToBestSinceSchedulerStart),
                              pformat prog
                            ]
                )
                solutions

writeResultsCSV ::
  FilePath ->
  ParallelSynthesisResult conProg ->
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
writeResultsCSV path result scheduler = do
  let columns =
        [ "initial_cost",
          "cost",
          "scheduler_elapsed_time",
          "time_to_best_since_scheduler_start",
          "time_to_best",
          "num_lattice_nodes",
          "num_undertermined_leaves",
          "num_root_programs",
          "num_undertermined_programs",
          "undetermined_ratio"
        ]
  let initialCost = case result of
        SolutionFound
          ParallelSynthesisSolutionFoundResult {initialCost = Just c} -> c
        _ -> -1
  let cost = case result of
        NoSolutionFound {} -> -1
        SolutionFound ParallelSynthesisSolutionFoundResult {bestCost = c} -> c
  let numOfNodesInLattice = resultNumOfNodesInLattice result
  let numOfUndeterminedLeaves = resultNumOfUndeterminedLeaves result
  let numOfRootPrograms = resultNumOfRootPrograms result
  let numOfUndeterminedPrograms = resultNumOfUndeterminedPrograms result
  let undeterminedRatio = resultUndeterminedRatio result
  let schedulerElapsedTime =
        realToFrac $
          diffUTCTime
            (resultAggregatedTime result)
            (schedulerStartTime scheduler) ::
          Double
  let (bestStartTime, bestResultTime) = resultBestTime result
  let timeToBestSinceSchedulerStart = case result of
        NoSolutionFound {} -> Nothing
        SolutionFound {} ->
          Just $
            realToFrac $
              diffUTCTime bestResultTime (schedulerStartTime scheduler) ::
            Maybe Double
  let timeToBest = case result of
        NoSolutionFound {} -> Nothing
        SolutionFound {} ->
          Just $
            realToFrac $
              diffUTCTime bestResultTime bestStartTime ::
            Maybe Double
  let values =
        [ show initialCost,
          show cost,
          show schedulerElapsedTime,
          maybe "inf" show timeToBestSinceSchedulerStart,
          maybe "inf" show timeToBest,
          show numOfNodesInLattice,
          show numOfUndeterminedLeaves,
          show numOfRootPrograms,
          show numOfUndeterminedPrograms,
          show undeterminedRatio
        ]
  writeFile path $ intercalate "," columns ++ "\n" ++ intercalate "," values

data NodeStats = NodeStats
  { nodeId :: NodeId,
    sortedIdx :: Int,
    nodeTime :: Double,
    collectedExamples :: Int,
    inProgressExamples :: Int
  }

data MessageStat = MessageStat
  { nodeIdOrigin :: NodeId,
    sortedIdx :: Int,
    msgTime :: Double
  }

data SummaryStats = SummaryStats
  { num :: Int,
    percentAll :: Double,
    percentEverStarted :: Double,
    nodeStats :: [NodeStats],
    avgTime :: Double,
    time75Percentile :: Double,
    time90Percentile :: Double,
    time95Percentile :: Double,
    avgCollectedExamples :: Double,
    avgInProgressExamples :: Double
  }

data Stats = Stats
  { allStartedNodeStats :: SummaryStats,
    numNotStarted :: Int,
    fastTrackViableStats :: SummaryStats,
    fastTrackRefiningStats :: SummaryStats,
    fastTrackImmSynthFailureStats :: SummaryStats,
    slowTrackRefiningStats :: SummaryStats,
    fastSucceedStats :: SummaryStats,
    slowSucceedStats :: SummaryStats,
    fastUnsatStats :: SummaryStats,
    slowUnsatStats :: SummaryStats,
    fastUnknownStats :: SummaryStats,
    slowUnknownStats :: SummaryStats,
    fastTerminatedStats :: SummaryStats,
    slowTerminatedStats :: SummaryStats,
    inferredFailureStats :: SummaryStats,
    justStartedStats :: SummaryStats,
    fastTrackViableMessageStats :: [MessageStat],
    fastTrackImmSynthFailureMessageStats :: [MessageStat],
    fastTrackSuccessMessageStats :: [MessageStat],
    slowTrackSuccessMessageStats :: [MessageStat]
  }

collectStats ::
  UTCTime ->
  [ ( NodeId,
      NodeState
        conProg
        symSemObj
        symVal
        conSemObj
        conVal
        matcher
    )
  ] ->
  IO Stats
collectStats curTime stats = do
  let allNodeNum = length stats
  let everStarted =
        filter (not . nodeStatusIsNotYetStarted . nodeStatus . snd) stats
  let everStartedNum = length everStarted
  let notStarted = filter (nodeStatusIsNotYetStarted . nodeStatus . snd) stats
  let startedWithLinspace = zip [0 ..] $ sortOn (nodeStateCurrentElapsedTime curTime . snd) everStarted
  let logsWithLinspace =
        fmap
          (second $ second nodeStateMajorRelativeTimeLog)
          startedWithLinspace
  let msg filt = do
        (idx, (nid, log)) <- logsWithLinspace
        (diffTime, msg) <- log
        guard $ filt msg
        return $ MessageStat nid idx (realToFrac diffTime :: Double)
  let toStats =
        fmap
          ( \(idx, (nid, s)) ->
              NodeStats
                nid
                idx
                (realToFrac $ nodeStateCurrentElapsedTime curTime s)
                (nodeStateNumCollectedExamples s)
                (nodeStateNumInProgressExamples s)
          )
  let filt f = filter (f . nodeStatus . snd . snd)
  let fastTrackViable = filt nodeStatusIsFastTrackViable startedWithLinspace
  let fastTrackRefining = filt nodeStatusIsFastTrackRefining startedWithLinspace
  let fastTrackImmSynthFailure =
        filt nodeStatusIsFastTrackEasySynthFailure startedWithLinspace
  let slowTrackRefining = filt nodeStatusIsSlowTrackRefining startedWithLinspace
  let fastSucceed = filt nodeStatusIsFastSuccess startedWithLinspace
  let slowSucceed = filt nodeStatusIsSlowSuccess startedWithLinspace
  let fastUnsat = filt nodeStatusIsFastUnsat startedWithLinspace
  let slowUnsat = filt nodeStatusIsSlowUnsat startedWithLinspace
  let fastUnknown = filt nodeStatusIsFastUnknown startedWithLinspace
  let slowUnknown = filt nodeStatusIsSlowUnknown startedWithLinspace
  let fastTerminated = filt nodeStatusIsFastTerminated startedWithLinspace
  let slowTerminated = filt nodeStatusIsSlowTerminated startedWithLinspace
  let inferredFailure = filt nodeStatusIsInferredFailure startedWithLinspace
  let justStarted = filt nodeStatusIsJustStarted startedWithLinspace
  let averageTime :: [NodeStats] -> Double
      averageTime [] = -1
      averageTime r =
        sum (fmap nodeTime r) / fromIntegral (length r) ::
          Double
  let percentile _ [] = -1
      percentile cutoff l =
        let times = sort l
            len = length times
            idx = min (len - 1) $ ceiling $ fromIntegral len * cutoff
         in times !! idx
  let toSummaryStats' nodeStats =
        SummaryStats
          (length nodeStats)
          (realToFrac $ 100 * length nodeStats % allNodeNum)
          (realToFrac $ 100 * length nodeStats % everStartedNum)
          nodeStats
          (averageTime nodeStats)
          (percentile 0.75 $ fmap nodeTime nodeStats)
          (percentile 0.90 $ fmap nodeTime nodeStats)
          (percentile 0.95 $ fmap nodeTime nodeStats)
          ( fromIntegral (sum (fmap collectedExamples nodeStats))
              / fromIntegral (length nodeStats) ::
              Double
          )
          ( fromIntegral (sum (fmap inProgressExamples nodeStats))
              / fromIntegral (length nodeStats) ::
              Double
          )
  let toSummaryStats = toSummaryStats' . toStats

  when
    ( sum
        [ length fastTrackViable,
          length fastTrackRefining,
          length fastTrackImmSynthFailure,
          length slowTrackRefining,
          length fastSucceed,
          length slowSucceed,
          length fastUnsat,
          length slowUnsat,
          length fastUnknown,
          length slowUnknown,
          length fastTerminated,
          length slowTerminated,
          length inferredFailure,
          length justStarted
        ]
        /= length everStarted
    )
    $ error "BUG: Not covering all possiblities"
  return $
    Stats
      (toSummaryStats startedWithLinspace)
      (length notStarted)
      (toSummaryStats fastTrackViable)
      (toSummaryStats fastTrackRefining)
      (toSummaryStats fastTrackImmSynthFailure)
      (toSummaryStats slowTrackRefining)
      (toSummaryStats fastSucceed)
      (toSummaryStats slowSucceed)
      (toSummaryStats fastUnsat)
      (toSummaryStats slowUnsat)
      (toSummaryStats fastUnknown)
      (toSummaryStats slowUnknown)
      (toSummaryStats fastTerminated)
      (toSummaryStats slowTerminated)
      (toSummaryStats inferredFailure)
      (toSummaryStats justStarted)
      (msg processResponseIsFastTrackViable)
      (msg processResponseIsFastTrackEasySynthFailure)
      (msg processResponseIsFastTrackSuccess)
      (msg processResponseIsSlowTrackSuccess)

plotStatistics'' :: FilePath -> String -> Stats -> IO ()
plotStatistics'' path title stats = do
  let toAnnotate =
        HS.fromList
          [ "fastTrackViable",
            "fastTrackRefining",
            "immSynthFailure",
            "slowTrackRefining",
            "fastSucceed",
            "slowSucceed"
          ]
  let msgStatToPoint MessageStat {nodeIdOrigin, sortedIdx, msgTime} =
        (nodeIdOrigin, (fromIntegral sortedIdx :: Double, msgTime))
  let msgDatum =
        HM.filter (not . null) $
          HM.fromList
            [ ( "fastViableMsg" :: String,
                msgStatToPoint <$> fastTrackViableMessageStats stats
              ),
              ( "immSynthFailureMsg",
                msgStatToPoint <$> fastTrackImmSynthFailureMessageStats stats
              ),
              ( "fastSuccessMsg",
                msgStatToPoint <$> fastTrackSuccessMessageStats stats
              ),
              ( "slowSuccessMsg",
                msgStatToPoint <$> slowTrackSuccessMessageStats stats
              )
            ]
  let msgDatumKeys = HM.keys msgDatum
  let nodeStatToPoint NodeStats {nodeId, sortedIdx, nodeTime} =
        (nodeId, (fromIntegral sortedIdx :: Double, nodeTime))
  let summaryStatsToPoints stats = nodeStatToPoint <$> nodeStats stats
  let nodeDatum =
        HM.filter (not . null) $
          HM.fromList
            [ ( "fastTrackViable" :: String,
                summaryStatsToPoints $ fastTrackViableStats stats
              ),
              ( "fastTrackRefining",
                summaryStatsToPoints $ fastTrackRefiningStats stats
              ),
              ( "immSynthFailure",
                summaryStatsToPoints $ fastTrackImmSynthFailureStats stats
              ),
              ( "slowTrackRefining",
                summaryStatsToPoints $ slowTrackRefiningStats stats
              ),
              ( "fastSucceed",
                summaryStatsToPoints $ fastSucceedStats stats
              ),
              ( "slowSucceed",
                summaryStatsToPoints $ slowSucceedStats stats
              ),
              ( "fastUnsat",
                summaryStatsToPoints $ fastUnsatStats stats
              ),
              ( "slowUnsat",
                summaryStatsToPoints $ slowUnsatStats stats
              ),
              ( "fastUnknown",
                summaryStatsToPoints $ fastUnknownStats stats
              ),
              ( "slowUnknown",
                summaryStatsToPoints $ slowUnknownStats stats
              ),
              ( "fastTerminated",
                summaryStatsToPoints $ fastTerminatedStats stats
              ),
              ( "slowTerminated",
                summaryStatsToPoints $ slowTerminatedStats stats
              ),
              ( "inferredFailure",
                summaryStatsToPoints $ inferredFailureStats stats
              ),
              ( "justStarted",
                summaryStatsToPoints $ justStartedStats stats
              )
            ]
  let nodeDatumKeys = HM.keys nodeDatum
  let toAnnotateDatum =
        HM.filterWithKey (\k _ -> k `HS.member` toAnnotate) nodeDatum
  let colors =
        HM.fromList
          [ ("fastTrackViable" :: String, aqua),
            ("fastTrackRefining", dodgerblue),
            ("immSynthFailure", mediumpurple),
            ("slowTrackRefining", blue),
            ("fastSucceed", green),
            ("slowSucceed", yellowgreen),
            ("fastUnsat", red),
            ("fastUnknown", deeppink),
            ("fastTerminated", gray),
            ("slowUnsat", red),
            ("slowUnknown", deeppink),
            ("slowTerminated", gray),
            ("inferredFailure", orange),
            ("justStarted", black),
            ("fastViableMsg", gray),
            ("immSynthFailureMsg", deeppink),
            ("fastSuccessMsg", green),
            ("slowSuccessMsg", goldenrod)
          ]
  let msgDatumColors = fmap (colors HM.!) msgDatumKeys
  let nodeDatumColors = fmap (colors HM.!) nodeDatumKeys
  let shapes =
        HM.fromList
          [ ("fastTrackViable" :: String, PointShapeCircle),
            ("fastTrackRefining", PointShapeCircle),
            ("immSynthFailure", PointShapeCircle),
            ("slowTrackRefining", PointShapeCircle),
            ("fastSucceed", PointShapeStar),
            ("slowSucceed", PointShapeStar),
            ("fastUnsat", PointShapeCross),
            ("fastUnknown", PointShapeCross),
            ("fastTerminated", PointShapeCross),
            ("slowUnsat", PointShapePlus),
            ("slowUnknown", PointShapePlus),
            ("slowTerminated", PointShapePlus),
            ("inferredFailure", PointShapeCross),
            ("justStarted", PointShapeCircle),
            ("fastViableMsg", PointShapePolygon 4 True),
            ("immSynthFailureMsg", PointShapePolygon 4 True),
            ("fastSuccessMsg", PointShapePolygon 4 True),
            ("slowSuccessMsg", PointShapePolygon 4 True)
          ]
  let msgDatumShapes = fmap (shapes HM.!) msgDatumKeys
  let nodeDatumShapes = fmap (shapes HM.!) nodeDatumKeys
  toFile (FileOptions (1600, 900) SVG) path $ do
    layout_title .= title
    plot $
      line
        "time"
        [fmap snd $ summaryStatsToPoints $ allStartedNodeStats stats]
    mapM_
      ( \dt -> do
          plot $ liftEC $ do
            plot_annotation_values
              .= fmap (\(nid, (x, y)) -> (x, y, show nid)) dt
            plot_annotation_style . font_size .= 8
      )
      $ fmap (msgDatum HM.!) msgDatumKeys ++ toList toAnnotateDatum
    setColors $ fmap opaque $ msgDatumColors ++ nodeDatumColors
    setShapes $ msgDatumShapes ++ nodeDatumShapes
    mapM_ (\name -> plot $ points name $ snd <$> msgDatum HM.! name) msgDatumKeys
    mapM_ (\name -> plot $ points name $ snd <$> nodeDatum HM.! name) nodeDatumKeys
  return ()

collectAllStats ::
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
  IO (Stats, HM.HashMap Int Stats)
collectAllStats ProcessScheduler {..} = do
  curTime <- getCurrentTime
  results <- HM.toList <$> readIORef nodeStates
  allStats <- collectStats curTime results
  dcTree <- readIORef dcTree
  let resultsWithDepth =
        fmap (\(nid, r) -> (nid, r, nodeDepth dcTree nid)) results
  let maxDepth = maximum $ fmap (\(_, _, d) -> d) resultsWithDepth
  let go depth
        | depth > maxDepth = return HM.empty
        | otherwise = do
            let resultsAtDepthWithNid =
                  (\(nid, r, _) -> (nid, r))
                    <$> filter (\(_, _, d) -> d == depth) resultsWithDepth
            stats <- collectStats curTime resultsAtDepthWithNid
            r <- go (depth + 1)
            return $ HM.insert depth stats r
  stats <- if null resultsWithDepth then return HM.empty else go 0
  return (allStats, stats)

logStatistics ::
  Doc ann ->
  Stats ->
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
logStatistics
  firstLine
  stats
  ProcessScheduler {config = ProcessSchedulerConfig {..}, ..} = do
    let statistics =
          [ ("Fast viable" :: String, fastTrackViableStats stats),
            ("Fast refining", fastTrackRefiningStats stats),
            ("Imm synth failed", fastTrackImmSynthFailureStats stats),
            ("Slow track refining", slowTrackRefiningStats stats),
            ("Fast succeed", fastSucceedStats stats),
            ("Slow succeed", slowSucceedStats stats),
            ("Fast unsat", fastUnsatStats stats),
            ("Fast unknown", fastUnknownStats stats),
            ("Fast terminated", fastTerminatedStats stats),
            ("Slow unsat", slowUnsatStats stats),
            ("Slow unknown", slowUnknownStats stats),
            ("Slow terminated", slowTerminatedStats stats),
            ("Inferred failure", inferredFailureStats stats),
            ("Just started", justStartedStats stats)
          ]
    let filteredStatistics = filter (\(_, s) -> num s > 0) statistics
    let maxNameLen = maximum $ fmap (length . fst) filteredStatistics
    let maxNumLen =
          maximum $ fmap (length . show . num . snd) filteredStatistics
    let maxPercentAllLen =
          maximum $ fmap (length . showFloat . percentAll . snd) filteredStatistics
    let maxPercentEverStartedLen =
          maximum $ fmap (length . showFloat . percentEverStarted . snd) filteredStatistics
    let maxAvgTimeLen =
          maximum $ fmap (length . showFloat . avgTime . snd) filteredStatistics
    let maxTime75PercentileLen =
          maximum $ fmap (length . showFloat . time75Percentile . snd) filteredStatistics
    let maxTime90PercentileLen =
          maximum $ fmap (length . showFloat . time90Percentile . snd) filteredStatistics
    let maxTime95PercentileLen =
          maximum $ fmap (length . showFloat . time95Percentile . snd) filteredStatistics
    let maxAvgCollectedExamplesLen =
          maximum $ fmap (length . showFloat . avgCollectedExamples . snd) filteredStatistics
    let maxAvgInProgressExamplesLen =
          maximum $ fmap (length . showFloat . avgInProgressExamples . snd) filteredStatistics
    let formatStats (name, SummaryStats {..}) =
          name
            <> ": "
            <> replicate
              ((maxNameLen + maxNumLen) - (length name + length (show num)))
              ' '
            <> show num
            <> replicate (maxPercentEverStartedLen - length (showFloat percentEverStarted)) ' '
            <> " ("
            <> showFloat percentEverStarted
            <> "%/"
            <> replicate (maxPercentAllLen - length (showFloat percentAll)) ' '
            <> showFloat percentAll
            <> "%), time(avg/75%/90%/95%): "
            <> replicate (maxAvgTimeLen - length (showFloat avgTime)) ' '
            <> showFloat avgTime
            <> "s/"
            <> replicate (maxTime75PercentileLen - length (showFloat time75Percentile)) ' '
            <> showFloat time75Percentile
            <> "s/"
            <> replicate (maxTime90PercentileLen - length (showFloat time90Percentile)) ' '
            <> showFloat time90Percentile
            <> "s/"
            <> replicate (maxTime95PercentileLen - length (showFloat time95Percentile)) ' '
            <> showFloat time95Percentile
            <> "s, examples(collected/in progress): "
            <> replicate (maxAvgCollectedExamplesLen - length (showFloat avgCollectedExamples)) ' '
            <> showFloat avgCollectedExamples
            <> "/"
            <> replicate (maxAvgInProgressExamplesLen - length (showFloat avgInProgressExamples)) ' '
            <> showFloat avgInProgressExamples
    logMultiLineDoc logger NOTICE $
      nest 2 $
        vsep $
          ( firstLine
              <> " ("
              <> pformat (num $ allStartedNodeStats stats)
              <> " nodes + "
              <> pformat (numNotStarted stats)
              <> " in queue):"
          )
            : (fromString . formatStats <$> filteredStatistics)

layeredStatistics ::
  HM.HashMap Int Stats ->
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
layeredStatistics
  depthStats
  scheduler@ProcessScheduler {config = ProcessSchedulerConfig {..}, ..} = do
    let maxDepth = maximum $ HM.keys depthStats
    let go depth
          | depth > maxDepth = return ()
          | otherwise = do
              let title = case cmdline of
                    Just cmdline -> cmdline <> " (depth " <> show depth <> ")"
                    Nothing -> "Depth " <> show depth
              let stats = depthStats HM.! depth
              plotStatistics''
                (logRootDir logConfig <> "/stats." <> show depth <> ".svg")
                title
                stats
              logStatistics ("Depth " <> pformat depth) stats scheduler
              go (depth + 1)
    unless (HM.null depthStats) $ go 0

plotStatistics ::
  LogConfig ->
  Stats ->
  HM.HashMap Int Stats ->
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
plotStatistics logConfig@LogConfig {..} stats layerStats scheduler@ProcessScheduler {..} = do
  let title = fromMaybe "Statistics" (cmdline config)
  logStatistics "All started" stats scheduler
  plotStatistics'' (logRootDir logConfig <> "/stats.svg") title stats
  layeredStatistics layerStats scheduler

debugLogAllStats ::
  Bool ->
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
debugLogAllStats
  onlyUndetermined
  scheduler@ProcessScheduler {config = ProcessSchedulerConfig {..}, ..} = do
    logMultiLineDoc logger DEBUG $
      if onlyUndetermined
        then "Dumping stats for undetermined nodes."
        else "Dumping stats for all nodes."
    curTime <- getCurrentTime
    dcTree <- readIORef dcTree
    let allRootNodes = sort $ HS.toList $ rootNodes dcTree
    let walkNode :: NodeId -> IO (Doc ann)
        walkNode nid = do
          result <- nodeResult nid
          status <- getStatus scheduler nid
          splitted <- getIsSplitted scheduler nid
          if not splitted
            then return result
            else do
              let children =
                    sort $
                      HS.toList $
                        fromMaybe mempty $
                          nodeDividedChildren dcTree nid
              if
                | null children -> return $ result <+> "{}"
                | onlyUndetermined && nodeStatusIsDetermined status ->
                    return $ result <+> "{...}"
                | otherwise -> do
                    childrenDocs <- mapM walkNode children
                    return $
                      vsep
                        [ nest 2 $ vsep $ [result <+> "{", vsep childrenDocs],
                          "}"
                        ]
        nodeResult :: NodeId -> IO (Doc ann)
        nodeResult nid = do
          state <- (HM.! nid) <$> readIORef nodeStates
          return $
            pformat nid
              <> ": "
              <> pformatNodeStateSummaryWithElapsedTime curTime state
    doc <- vsep <$> traverse walkNode allRootNodes
    logMultiLineDoc logger DEBUG doc
