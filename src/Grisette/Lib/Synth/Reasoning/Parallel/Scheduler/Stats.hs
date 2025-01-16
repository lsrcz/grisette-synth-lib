{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Stats
  ( reportStatistics,
  )
where

import Control.Monad (guard, unless, when)
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
    nodeDepth,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.LogConfig (logRootDir)
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.NodeState
  ( NodeState (nodeStatus),
    nodeStateCurrentElapsedTime,
    nodeStateMajorRelativeTimeLog,
    nodeStateNumCollectedExamples,
    nodeStateNumInProgressExamples,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.NodeStatus
  ( nodeStatusIsInferredFailure,
    nodeStatusIsJustStarted,
    nodeStatusIsNotYetStarted,
    nodeStatusIsRefining,
    nodeStatusIsSuccess,
    nodeStatusIsTerminated,
    nodeStatusIsUnknown,
    nodeStatusIsUnsat,
    nodeStatusIsViable,
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
_collectStats curTime stats = do
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
  let viable = filt nodeStatusIsViable startedWithLinspace
  let refining = filt nodeStatusIsRefining startedWithLinspace
  let succeed = filt nodeStatusIsSuccess startedWithLinspace
  let unsat = filt nodeStatusIsUnsat startedWithLinspace
  let unknown = filt nodeStatusIsUnknown startedWithLinspace
  let terminated = filt nodeStatusIsTerminated startedWithLinspace
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
        [ length viable,
          length refining,
          length succeed,
          length unsat,
          length unknown,
          length terminated,
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
      (toSummaryStats viable)
      (toSummaryStats refining)
      (toSummaryStats succeed)
      (toSummaryStats unsat)
      (toSummaryStats unknown)
      (toSummaryStats terminated)
      (toSummaryStats inferredFailure)
      (toSummaryStats justStarted)
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
_collectAllStats Scheduler {..} = do
  curTime <- getCurrentTime
  results <- HM.toList <$> readIORef nodeStates
  allStats <- _collectStats curTime results
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
            stats <- _collectStats curTime resultsAtDepthWithNid
            r <- go (depth + 1)
            return $ HM.insert depth stats r
  stats <- if null resultsWithDepth then return HM.empty else go 0
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
