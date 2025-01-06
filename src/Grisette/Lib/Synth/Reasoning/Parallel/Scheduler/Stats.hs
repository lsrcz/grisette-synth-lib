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
import Grisette
  ( Doc,
    PPrint (pformat),
    nest,
    vsep,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.DCTree
  ( NodeId,
    nodeDepth,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.LogConfig (logRootDir)
import Grisette.Lib.Synth.Reasoning.Parallel.NodeState
  ( NodeState (nodeStatus),
    nodeStateCurrentElapsedTime,
    nodeStateMajorRelativeTimeLog,
    nodeStateNumCollectedExamples,
    nodeStateNumInProgressExamples,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.NodeStatus
  ( nodeStatusIsFastSuccess,
    nodeStatusIsFastTerminated,
    nodeStatusIsFastTrackEasySynthFailure,
    nodeStatusIsFastTrackRefining,
    nodeStatusIsFastTrackViable,
    nodeStatusIsFastUnknown,
    nodeStatusIsFastUnsat,
    nodeStatusIsInferredFailure,
    nodeStatusIsJustStarted,
    nodeStatusIsNotYetStarted,
    nodeStatusIsSlowSuccess,
    nodeStatusIsSlowTerminated,
    nodeStatusIsSlowTrackRefining,
    nodeStatusIsSlowUnknown,
    nodeStatusIsSlowUnsat,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Process
  ( processResponseIsFastTrackEasySynthFailure,
    processResponseIsFastTrackSuccess,
    processResponseIsFastTrackViable,
    processResponseIsSlowTrackSuccess,
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
  ( ProcessScheduler
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

_collectAllStats ::
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
_collectAllStats ProcessScheduler {..} = do
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
_logStatistics
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

_layeredStatistics ::
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
_layeredStatistics
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
              _plotStatistics
                (logRootDir logConfig <> "/stats." <> show depth <> ".svg")
                title
                stats
              _logStatistics ("Depth " <> pformat depth) stats scheduler
              go (depth + 1)
    unless (HM.null depthStats) $ go 0

reportStatistics ::
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
reportStatistics
  scheduler@ProcessScheduler
    { config = ProcessSchedulerConfig {..},
      ..
    } = do
    (stats, layerStats) <- _collectAllStats scheduler
    let title = fromMaybe "Statistics" cmdline
    _logStatistics "All started" stats scheduler
    _plotStatistics (logRootDir logConfig <> "/stats.svg") title stats
    _layeredStatistics layerStats scheduler
