{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Config
  ( ProcessSchedulerConfig (..),
  )
where

import qualified Data.Text as T
import Grisette (GrisetteSMTConfig)
import Grisette.Lib.Synth.Program.Choice.Counting
  ( CountNumProgsEvidence,
  )
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable)
import Grisette.Lib.Synth.Reasoning.Parallel.LogConfig
  ( LogConfig,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Process
  ( ConProgConstraint,
    ProcessConstraint,
    TwoTrackVerifiers,
  )
import System.Log.Logger (Logger)

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
