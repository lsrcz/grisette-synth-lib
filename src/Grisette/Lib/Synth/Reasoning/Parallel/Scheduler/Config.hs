{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Config
  ( SchedulerConfig (..),
  )
where

import qualified Data.Text as T
import Grisette (GrisetteSMTConfig)
import Grisette.Lib.Synth.Program.Choice.Counting
  ( CountNumProgsEvidence,
  )
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable)
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.LogConfig
  ( LogConfig,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Process
  ( ConProgConstraint,
    ProcessConstraint,
  )
import Grisette.Lib.Synth.Reasoning.Synthesis (SomeVerifier)
import System.Log.Logger (Logger)

data
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
    matcher
  where
  SchedulerConfig ::
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
      verifiers :: Logger -> [[SomeVerifier sketch conProg]],
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
      matcher
