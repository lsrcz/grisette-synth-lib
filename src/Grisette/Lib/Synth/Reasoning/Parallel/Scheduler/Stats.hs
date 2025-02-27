{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Stats
  ( reportStatistics,
  )
where

import Control.Monad (guard, unless)
import Data.Bifunctor (second)
import Data.Foldable (Foldable (toList))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (readIORef)
import Data.List (sort, sortOn)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.String (IsString (fromString))
import Data.Time
  ( UTCTime,
    getCurrentTime,
  )
import Graphics.Rendering.Chart.Backend.Cairo
  ( FileFormat (SVG),
    FileOptions (FileOptions),
    toFile,
  )
import Graphics.Rendering.Chart.Easy
  ( PointShape
      ( PointShapeCircle,
        PointShapeCross,
        PointShapePolygon,
        PointShapeStar
      ),
    aqua,
    black,
    deeppink,
    dodgerblue,
    font_size,
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
import Grisette
  ( Doc,
    PPrint (pformat),
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
    nodeDepth,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.LogConfig (logRootDir)
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.NodeState
  ( nodeStateCurrentElapsedTime,
    nodeStateMajorRelativeTimeLog,
    nodeStateNumCollectedExamples,
    nodeStateNumInProgressExamples,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Process
  ( processResponseIsEasySynthFailure,
    processResponseIsSuccess,
    processResponseIsViable,
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
    StatusType
      ( InferredFailure,
        JustStarted,
        NotYetStarted,
        Refining,
        Succeeded,
        Terminated,
        Unknown,
        Unsat,
        Viable
      ),
    getNodesByDepth,
    getNodesByStatus,
    getNodesByStatusAndDepth,
  )
import Grisette.Lib.Synth.Util.Logging (logMultiLineDoc)
import Grisette.Lib.Synth.Util.Show (showFloat)
import System.Log.Logger (Priority (NOTICE))

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
    viableStats :: SummaryStats,
    refiningStats :: SummaryStats,
    succeedStats :: SummaryStats,
    unsatStats :: SummaryStats,
    unknownStats :: SummaryStats,
    terminatedStats :: SummaryStats,
    inferredFailureStats :: SummaryStats,
    justStartedStats :: SummaryStats,
    viableMessageStats :: [MessageStat],
    easySynthFailureMessageStats :: [MessageStat],
    succeedMessageStats :: [MessageStat]
  }

_collectStats ::
  UTCTime ->
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
  Maybe Int -> -- Optional depth filter
  IO Stats
_collectStats curTime scheduler@Scheduler {config = SchedulerConfig {..}, ..} maybeDepth = do
  -- Get node sets by status type
  let getNodeSet statusType = case maybeDepth of
        Nothing -> getNodesByStatus scheduler statusType
        Just depth -> getNodesByStatusAndDepth scheduler statusType depth

  viableNodes <- getNodeSet Viable
  refiningNodes <- getNodeSet Refining
  succeedNodes <- getNodeSet Succeeded
  unsatNodes <- getNodeSet Unsat
  unknownNodes <- getNodeSet Unknown
  terminatedNodes <- getNodeSet Terminated
  inferredFailureNodes <- getNodeSet InferredFailure
  justStartedNodes <- getNodeSet JustStarted
  notYetStartedNodes <- getNodeSet NotYetStarted

  -- Read all node states
  nodeStatesMap <- readIORef nodeStates

  -- Sanity check: ensure all nodes have a status and are counted exactly once
  let allTrackedNodes =
        viableNodes
          `HS.union` refiningNodes
          `HS.union` succeedNodes
          `HS.union` unsatNodes
          `HS.union` unknownNodes
          `HS.union` terminatedNodes
          `HS.union` inferredFailureNodes
          `HS.union` justStartedNodes
          `HS.union` notYetStartedNodes

  allNodes <- case maybeDepth of
    Just depth -> getNodesByDepth scheduler depth
    Nothing -> return $ HS.fromList $ HM.keys nodeStatesMap
  let missingNodes = allNodes `HS.difference` allTrackedNodes
  -- Check for nodes appearing in multiple status sets
  let findDuplicates node =
        filter
          (\(_, nodes) -> node `HS.member` nodes)
          [ (Viable, viableNodes),
            (Refining, refiningNodes),
            (Succeeded, succeedNodes),
            (Unsat, unsatNodes),
            (Unknown, unknownNodes),
            (Terminated, terminatedNodes),
            (InferredFailure, inferredFailureNodes),
            (JustStarted, justStartedNodes),
            (NotYetStarted, notYetStartedNodes)
          ]

      multiStatusNodes = HM.fromList $ do
        node <- HS.toList allTrackedNodes
        let statuses = findDuplicates node
        [(node, map fst statuses) | length statuses > 1]

  -- Log any issues
  unless (HS.null missingNodes) $
    logMultiLineDoc logger NOTICE $
      "STATS ERROR: " <> pformat (HS.size missingNodes) <> " nodes without any status: " <> pformat (HS.toList missingNodes)

  unless (HM.null multiStatusNodes) $
    logMultiLineDoc logger NOTICE $
      "STATS ERROR: Nodes with multiple statuses: " <> pformat multiStatusNodes

  -- Continue with normal stats collection
  -- Convert to list of (NodeId, NodeState)
  let getNodeStats nid =
        case HM.lookup nid nodeStatesMap of
          Just s -> (nid, s)
          Nothing -> error $ "Node " ++ show nid ++ " not found in nodeStates"

  let mkNodeStatsFromSet nodeSet =
        map getNodeStats (HS.toList nodeSet)

  let viableStats = mkNodeStatsFromSet viableNodes
      refiningStats = mkNodeStatsFromSet refiningNodes
      succeedStats = mkNodeStatsFromSet succeedNodes
      unsatStats = mkNodeStatsFromSet unsatNodes
      unknownStats = mkNodeStatsFromSet unknownNodes
      terminatedStats = mkNodeStatsFromSet terminatedNodes
      inferredFailureStats = mkNodeStatsFromSet inferredFailureNodes
      justStartedStats = mkNodeStatsFromSet justStartedNodes

  -- All nodes that have ever started
  let everStarted =
        viableStats
          ++ refiningStats
          ++ succeedStats
          ++ unsatStats
          ++ unknownStats
          ++ terminatedStats
          ++ inferredFailureStats
          ++ justStartedStats

  let allNodeNum = length everStarted + HS.size notYetStartedNodes
  let everStartedNum = length everStarted
  let notStarted = HS.size notYetStartedNodes

  let startedWithLinspace = zip [0 ..] $ sortOn (nodeStateCurrentElapsedTime curTime . snd) everStarted

  -- Create a mapping from NodeId to sortedIdx to preserve global ordering
  let nodeIdToSortedIdx = HM.fromList [(nid, idx) | (idx, (nid, _)) <- startedWithLinspace]

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
          ( \(nid, s) ->
              let idx = nodeIdToSortedIdx HM.! nid
               in NodeStats
                    nid
                    idx
                    (realToFrac $ nodeStateCurrentElapsedTime curTime s)
                    (nodeStateNumCollectedExamples s)
                    (nodeStateNumInProgressExamples s)
          )

  let viable = toStats viableStats
  let refining = toStats refiningStats
  let succeed = toStats succeedStats
  let unsat = toStats unsatStats
  let unknown = toStats unknownStats
  let terminated = toStats terminatedStats
  let inferredFailure = toStats inferredFailureStats
  let justStarted = toStats justStartedStats

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

  return $
    Stats
      (toSummaryStats' $ toStats (map snd startedWithLinspace))
      notStarted
      (toSummaryStats' viable)
      (toSummaryStats' refining)
      (toSummaryStats' succeed)
      (toSummaryStats' unsat)
      (toSummaryStats' unknown)
      (toSummaryStats' terminated)
      (toSummaryStats' inferredFailure)
      (toSummaryStats' justStarted)
      (msg processResponseIsViable)
      (msg processResponseIsEasySynthFailure)
      (msg processResponseIsSuccess)

_plotStatistics :: FilePath -> String -> Stats -> IO ()
_plotStatistics path title stats = do
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
            [ ( "viableMsg" :: String,
                msgStatToPoint <$> viableMessageStats stats
              ),
              ( "easySynthFailureMsg",
                msgStatToPoint <$> easySynthFailureMessageStats stats
              ),
              ( "succeedMsg",
                msgStatToPoint <$> succeedMessageStats stats
              )
            ]
  let msgDatumKeys = HM.keys msgDatum
  let nodeStatToPoint NodeStats {nodeId, sortedIdx, nodeTime} =
        (nodeId, (fromIntegral sortedIdx :: Double, nodeTime))
  let summaryStatsToPoints stats = nodeStatToPoint <$> nodeStats stats
  let nodeDatum =
        HM.filter (not . null) $
          HM.fromList
            [ ( "viable",
                summaryStatsToPoints $ viableStats stats
              ),
              ( "refining",
                summaryStatsToPoints $ refiningStats stats
              ),
              ( "succeed",
                summaryStatsToPoints $ succeedStats stats
              ),
              ( "unsat",
                summaryStatsToPoints $ unsatStats stats
              ),
              ( "unknown",
                summaryStatsToPoints $ unknownStats stats
              ),
              ( "terminated",
                summaryStatsToPoints $ terminatedStats stats
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
          [ ("viable" :: String, aqua),
            ("refining", dodgerblue),
            ("succeed", green),
            ("unsat", mediumpurple),
            ("unknown", yellowgreen),
            ("terminated", red),
            ("inferredFailure", orange),
            ("justStarted", black),
            ("viableMsg", gray),
            ("easySynthFailureMsg", deeppink),
            ("succeedMsg", green)
          ]
  let msgDatumColors = fmap (colors HM.!) msgDatumKeys
  let nodeDatumColors = fmap (colors HM.!) nodeDatumKeys
  let shapes =
        HM.fromList
          [ ("viable" :: String, PointShapeCircle),
            ("refining", PointShapeCircle),
            ("succeed", PointShapeStar),
            ("unsat", PointShapeCross),
            ("unknown", PointShapeCross),
            ("terminated", PointShapeCross),
            ("inferredFailure", PointShapeCross),
            ("justStarted", PointShapeCircle),
            ("viableMsg", PointShapePolygon 4 True),
            ("easySynthFailureMsg", PointShapePolygon 4 True),
            ("succeedMsg", PointShapePolygon 4 True)
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

_collectAllStats ::
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
  IO (Stats, HM.HashMap Int Stats)
_collectAllStats scheduler@Scheduler {..} = do
  curTime <- getCurrentTime

  -- Collect overall stats (all depths)
  allStats <- _collectStats curTime scheduler Nothing

  -- Get max depth
  dcTree <- readIORef dcTree
  nodeStatesMap <- readIORef nodeStates
  let nodeIds = HM.keys nodeStatesMap
  let depths = map (nodeDepth dcTree) nodeIds

  if null depths
    then return (allStats, HM.empty)
    else do
      let maxDepth = maximum depths

      -- Collect stats for each depth
      let go depth
            | depth > maxDepth = return HM.empty
            | otherwise = do
                stats <- _collectStats curTime scheduler (Just depth)
                r <- go (depth + 1)
                return $ HM.insert depth stats r

      stats <- go 0
      return (allStats, stats)

_logStatistics ::
  Doc ann ->
  Stats ->
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
_logStatistics
  firstLine
  stats
  Scheduler {config = SchedulerConfig {..}, ..} = do
    let statistics =
          [ ("viable" :: String, viableStats stats),
            ("refining", refiningStats stats),
            ("succeed", succeedStats stats),
            ("unsat", unsatStats stats),
            ("unknown", unknownStats stats),
            ("terminated", terminatedStats stats),
            ("inferredFailure", inferredFailureStats stats),
            ("justStarted", justStartedStats stats)
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

_layeredStatistics ::
  HM.HashMap Int Stats ->
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
_layeredStatistics
  depthStats
  scheduler@Scheduler {config = SchedulerConfig {..}, ..} = do
    let maxDepth = maximum $ HM.keys depthStats
    let go depth
          | depth > maxDepth = return ()
          | otherwise = do
              let title = case cmdline of
                    Just cmdline -> cmdline <> " (depth " <> show depth <> ")"
                    Nothing -> "Depth " <> show depth
              let stats = depthStats HM.! depth
              _plotStatistics
                (logRootDir logConfig <> "/stats." <> show depth <> ".svg")
                title
                stats
              _logStatistics ("Depth " <> pformat depth) stats scheduler
              go (depth + 1)
    unless (HM.null depthStats) $ go 0

reportStatistics ::
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
reportStatistics
  scheduler@Scheduler
    { config = SchedulerConfig {..},
      ..
    } = do
    (stats, layerStats) <- _collectAllStats scheduler
    let title = fromMaybe "Statistics" cmdline
    _logStatistics "All started" stats scheduler
    _plotStatistics (logRootDir logConfig <> "/stats.svg") title stats
    _layeredStatistics layerStats scheduler
