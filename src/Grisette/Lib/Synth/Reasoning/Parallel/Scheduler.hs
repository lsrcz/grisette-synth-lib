{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Grisette.Lib.Synth.Reasoning.Parallel.Scheduler
  ( LogConfig (..),
    getDefaultLogger,
    getLogConfig,
    ProcessCostConstraint,
    ProcessConstraint,
    ConProgConstraint,
    SchedulerConfig (..),
    ParallelSynthesisResult (..),
    ParallelSynthesisSolutionFoundResult (..),
    ParallelSynthesisNoSolutionResult (..),
    ParallelSynthesisSolution (..),
    runWithScheduler,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Concurrent (putMVar, takeMVar, threadDelay)
import Control.Monad (unless, void, when)
import Control.Monad.Extra (mapMaybeM, whileM)
import Data.Dynamic (toDyn)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
import Data.Time
  ( addUTCTime,
    getCurrentTime,
  )
import GHC.Conc.Signal (setHandler)
import Grisette (PPrint (pformat), nest, vsep)
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable)
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Action
import qualified Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.BiasedQueue as Q
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Config
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.DCTree
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.LogConfig
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.NodeStatus
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Process
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Result
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Scheduler
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Sketch
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Stats
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.TreeStats
import Grisette.Lib.Synth.Util.Logging (logMultiLineDoc)
import System.Exit (ExitCode (ExitSuccess))
import System.Log.Logger (Priority (DEBUG, NOTICE))
import System.Posix
  ( exitImmediately,
    sigHUP,
    sigINT,
    sigTERM,
  )

-- Actions

runAction ::
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
step
  scheduler@Scheduler {config = SchedulerConfig {..}, ..} = do
    startCost <- getCurrentMinimalCost scheduler
    curNodeToProcess <- readIORef nodeToProcess
    cancelledNodes <-
      fmap HM.fromList
        $ mapMaybeM
          ( \(nid, _) -> do
              r <- killIfTimeout scheduler nid
              r2 <- killIfDividedChildrenAllStarted scheduler nid
              return $ fmap (nid,) $ r <|> r2
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

    -- Check if we need to split nodes at the first not fully split depth
    curQueue <- readIORef nodeQueue
    notFullySplitDepth <- getFirstNotFullySplitDepth scheduler

    -- Split at the first not fully split depth - do this regardless of queue status
    -- Only check minDepth condition if queue is not empty
    shouldSplit <-
      if Q.null curQueue
        then return True -- If queue is empty, always split
        else do
          minDepth <- Q.minQueuedDepth curQueue
          return (minDepth >= notFullySplitDepth)

    when shouldSplit $ do
      -- Get all nodes at the not fully split depth
      allNodesAtDepth <- getNodesByDepth scheduler notFullySplitDepth
      unless (null allNodesAtDepth) $ do
        -- Get split nodes at this depth
        splitNodesAtDepth <- getSplitNodesByDepth scheduler notFullySplitDepth
        -- Get nodes that don't need children
        nodesByStatusType <- do
          statusSets <- readIORef nodeStatusSets
          case HM.lookup notFullySplitDepth statusSets of
            Nothing -> return HM.empty
            Just statusMap -> return statusMap

        let doNotNeedChildNodes =
              HS.unions
                [ nodes
                | (statusType, nodes) <- HM.toList nodesByStatusType,
                  statusTypeDoNotNeedChild statusType
                ]

        -- Find nodes that need to be split (not already split and need children)
        let needSplitNodes =
              HS.difference
                (HS.difference allNodesAtDepth splitNodesAtDepth)
                doNotNeedChildNodes

        logMultiLineDoc logger NOTICE $
          nest 2 $
            vsep
              [ "Splitting all nodes at depth " <> pformat notFullySplitDepth <> ":",
                pformat needSplitNodes
              ]

        -- Split all nodes that need splitting at this depth
        mapM_ (splitNode scheduler False) (HS.toList needSplitNodes)

    -- restart just started
    cost <- getCurrentMinimalCost scheduler
    case cost of
      Just curCost
        | curCost > targetCost ->
            when (isNothing startCost || cost < startCost) $
              mapM_ (resetIfJustStarted scheduler) $
                HM.keys allNodes
      _ -> return ()
    startQueued scheduler

shutdownScheduler ::
  Bool ->
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
shutdownScheduler printInfo scheduler@Scheduler {..} = do
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
installSchedulerSignalHandler logConfig scheduler@Scheduler {..} = do
  let handler printInfo = do
        takeMVar queueLock
        results <- getParallelSynthesisResult scheduler
        printResults scheduler results
        writeResultsCSV
          (logRootDir logConfig <> "/results.csv")
          results
          scheduler
        shutdownScheduler printInfo scheduler
        reportStatistics scheduler
        logTreeStats DEBUG False scheduler
        logTreeStats NOTICE True scheduler
        exitImmediately ExitSuccess
  setHandler sigINT $ Just (const $ handler True, toDyn ())
  setHandler sigTERM $ Just (const $ handler True, toDyn ())
  setHandler sigHUP $ Just (const $ handler False, toDyn ())
  return ()

initialSplit ::
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
initialSplit scheduler@Scheduler {..} = do
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
  SchedulerConfig
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
  IO (ParallelSynthesisResult conProg)
runWithScheduler
  config@SchedulerConfig {..}
  sketches = do
    scheduler <- newScheduler config
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
      threadDelay $ round $ pollIntervalSeconds * 1000000
      modifyIORef' iter (+ 1)
      curIter <- readIORef iter
      when (curIter `mod` 20 == 0) $
        reportStatistics scheduler
      when (curIter `mod` 100 == 0) $
        logTreeStats DEBUG False scheduler
      when (curIter `mod` 20 == 0) $
        logTreeStats NOTICE True scheduler
      putMVar (queueLock scheduler) ()
      curTime <- getCurrentTime
      return $
        (v > 0 || x > 0)
          && (isNothing schedulerTimeoutSeconds || curTime < endTime)
          && ( isNothing minimalCost
                 || minimalCost > Just targetCost
             )
    results <- getParallelSynthesisResult scheduler
    printResults scheduler results
    writeResultsCSV (logRootDir logConfig <> "/results.csv") results scheduler
    shutdownScheduler True scheduler
    reportStatistics scheduler
    logTreeStats DEBUG False scheduler
    logTreeStats NOTICE True scheduler
    return results
