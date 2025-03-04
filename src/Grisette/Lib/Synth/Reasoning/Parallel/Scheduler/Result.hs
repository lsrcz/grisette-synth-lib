{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Result
  ( ParallelSynthesisResult (..),
    ParallelSynthesisNoSolutionResult (..),
    ParallelSynthesisSolutionFoundResult (..),
    ParallelSynthesisSolution (..),
    getParallelSynthesisResult,
    printResults,
    writeResultsCSV,
  )
where

import Control.Monad (filterM)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (readIORef)
import Data.List (intercalate)
import Data.List.Extra (minimumOn)
import Data.Maybe (fromJust, isJust)
import Data.String (IsString (fromString))
import Data.Time
  ( UTCTime,
    diffUTCTime,
    getCurrentTime,
  )
import Grisette
  ( PPrint (pformat),
    nest,
    vsep,
  )
import Grisette.Lib.Synth.Program.Choice.Counting
  ( ComponentChoicesNumResult (numComponents, numTotalChoices),
    CountNumProgs (countNumInsts, countNumProgs),
    CountNumProgsEvidence (CountNumProgsEvidence),
    avgNumComponentChoicesWithEvidence,
    countNumInstsWithEvidence,
  )
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable)
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
    leafNodes,
    numNodes,
    rootNodes,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.NodeState
  ( NodeState
      ( NodeState,
        nodeEndTime,
        nodeMajorResponseReverseLog,
        nodeResponseReverseLog,
        nodeStartTime,
        nodeStatus
      ),
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.NodeStatus
  ( nodeStatusBestProgWithCost,
    statusType,
    statusTypeIsDetermined,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Process
  ( MessageType (MessageSuccess),
    responseType,
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
    getCurrentMinimalCost,
    getSketchTable,
    getStatus,
  )
import Grisette.Lib.Synth.Util.Logging (logMultiLineDoc)
import Grisette.Lib.Synth.Util.Show (showDiffTime)
import System.Log.Logger (Priority (NOTICE, WARNING))

data ParallelSynthesisSolution conProg = ParallelSynthesisSolution
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
    undeterminedRatio :: Double,
    avgComponentChoices :: Double
  }

data ParallelSynthesisSolutionFoundResult conProg = ParallelSynthesisSolutionFoundResult
  { aggregatedTime :: UTCTime,
    initialCost :: Maybe Int,
    bestCost :: Int,
    solutions :: [ParallelSynthesisSolution conProg],
    numOfNodesInLattice :: Int,
    numOfUndeterminedLeaves :: Int,
    numOfRootPrograms :: Integer,
    numOfUndeterminedPrograms :: Integer,
    undeterminedRatio :: Double,
    avgComponentChoices :: Double,
    minNumInsts :: Integer
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

resultAvgComponentChoices :: ParallelSynthesisResult conProg -> Double
resultAvgComponentChoices (NoSolutionFound ParallelSynthesisNoSolutionResult {..}) =
  avgComponentChoices
resultAvgComponentChoices (SolutionFound ParallelSynthesisSolutionFoundResult {..}) =
  avgComponentChoices

resultMinNumInsts :: ParallelSynthesisResult conProg -> Maybe Integer
resultMinNumInsts (NoSolutionFound _) = Nothing
resultMinNumInsts (SolutionFound ParallelSynthesisSolutionFoundResult {..}) = Just minNumInsts

getParallelSynthesisResult ::
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
  IO (ParallelSynthesisResult conProg)
getParallelSynthesisResult
  scheduler@Scheduler
    { config = SchedulerConfig {..},
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
            return $ not $ statusTypeIsDetermined (statusType s)
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

    avgCompChoices <-
      case countNumProgsEvidence of
        Just countNumProgsEvidence -> do
          rootSketches <- traverse (getSketchTable scheduler) $ HS.toList roots
          let compChoicesResult =
                mconcat $ map (avgNumComponentChoicesWithEvidence countNumProgsEvidence) rootSketches
          return $ fromIntegral (numTotalChoices compChoicesResult) / fromIntegral (numComponents compChoicesResult)
        Nothing -> return (0 / 0)

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
              avgCompChoices
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
                  avgCompChoices
          else do
            -- Calculate minimum number of instructions across all solutions
            minInsts <- case countNumProgsEvidence of
              Just CountNumProgsEvidence ->
                -- We'll need to use easySketchFromFastResult to convert conProg to sketchSpec
                case easySketchFromFastResult of
                  Just converter -> do
                    let progInsts =
                          map
                            ( \ParallelSynthesisSolution {_program = prog} ->
                                countNumInsts (converter prog)
                            )
                            lst
                    return $ minimum progInsts
                  Nothing -> return 0 -- No converter available
              Nothing -> return 0

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
                  avgCompChoices
                  minInsts
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
                        (\(_, r) -> responseType r == Right MessageSuccess)
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

printResults ::
  Scheduler sketchSpec sketch conProg costObj cost symSemObj symVal conSemObj conVal matcher ->
  ParallelSynthesisResult conProg ->
  IO ()
printResults Scheduler {config = SchedulerConfig {..}, ..} result = do
  let numOfNodesInLattice = resultNumOfNodesInLattice result
  let numOfUndeterminedLeaves = resultNumOfUndeterminedLeaves result
  let numOfUndeterminedPrograms = resultNumOfUndeterminedPrograms result
  let numOfRootPrograms = resultNumOfRootPrograms result
  let undeterminedRatio = resultUndeterminedRatio result
  let avgCompChoices = resultAvgComponentChoices result
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
              "Undetermined ratio: " <> pformat undeterminedRatio,
              "Avg component choices: " <> pformat avgCompChoices
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
              "Avg component choices: " <> pformat avgCompChoices,
              "Min number of instructions: " <> pformat minNumInsts,
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
          "undetermined_ratio",
          "avg_component_choices",
          "min_num_instructions"
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
  let avgComponentChoices = resultAvgComponentChoices result
  let minNumInstructions = maybe "inf" show $ resultMinNumInsts result
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
          show undeterminedRatio,
          show avgComponentChoices,
          minNumInstructions
        ]
  writeFile path $ intercalate "," columns ++ "\n" ++ intercalate "," values
