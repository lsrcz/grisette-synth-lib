{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Grisette.Lib.Synth.Reasoning.Fuzzing
  ( fuzzingTestProg,
    fuzzingTestSymProgWithModel,
    fuzzingTestStatefulVerifierFun,
    SynthesisWithFuzzerTask (..),
  )
where

import Grisette
  ( ConfigurableSolver,
    EvaluateSym,
    Model,
    SEq,
    StatefulVerifierFun,
    ToCon,
    ToSym,
    VerifierResult (CEGISVerifierFoundCex, CEGISVerifierNoCex),
    evaluateSymToCon,
  )
import Grisette.Lib.Synth.Context (ConcreteContext)
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Reasoning.IOPair (IOPair (IOPair))
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( SynthesisContext,
    SynthesisTask (SynthesisTask),
    ToSynthesisTask
      ( ConProgType,
        ConValType,
        ContextType,
        ExceptionType,
        SymProgType,
        SymValType,
        toSynthesisTask
      ),
  )
import Test.QuickCheck.Counterexamples
  ( Args (chatty, maxSuccess),
    Gen,
    forAll,
    quickCheckWith,
    stdArgs,
    type (:&:) ((:&:)),
  )

fuzzingTestProg ::
  ( Eq conVal,
    Show conVal,
    ProgSemantics semObj conProg conVal ConcreteContext
  ) =>
  Gen [conVal] ->
  ([conVal] -> [conVal]) ->
  Int ->
  semObj ->
  conProg ->
  IO (Maybe (IOPair conVal))
fuzzingTestProg gen spec maxTests sem prog = do
  maybeCex <-
    quickCheckWith
      stdArgs {maxSuccess = maxTests, chatty = False}
      (forAll gen $ \inputs -> Right (spec inputs) == runProg sem prog inputs)
  case maybeCex of
    Nothing -> return Nothing
    Just (cex :&: _) -> return $ Just $ IOPair cex (spec cex)

fuzzingTestSymProgWithModel ::
  forall conVal conProg symProg semObj p.
  ( Eq conVal,
    Show conVal,
    ProgSemantics semObj conProg conVal ConcreteContext,
    ToCon symProg conProg,
    EvaluateSym symProg
  ) =>
  Gen [conVal] ->
  ([conVal] -> [conVal]) ->
  Int ->
  p conProg ->
  semObj ->
  symProg ->
  Model ->
  IO (Maybe (IOPair conVal))
fuzzingTestSymProgWithModel gen spec maxTests _ sem prog model = do
  fuzzingTestProg
    gen
    spec
    maxTests
    sem
    (evaluateSymToCon model prog :: conProg)

fuzzingTestStatefulVerifierFun ::
  forall conVal conProg symProg semObj p.
  ( Eq conVal,
    Show conVal,
    ProgSemantics semObj conProg conVal ConcreteContext,
    ToCon symProg conProg,
    EvaluateSym symProg
  ) =>
  ([conVal] -> [conVal]) ->
  Int ->
  p conProg ->
  semObj ->
  symProg ->
  StatefulVerifierFun [Gen [conVal]] (IOPair conVal) ()
fuzzingTestStatefulVerifierFun spec maxTests p sem prog = go
  where
    go [] _ = return ([], CEGISVerifierNoCex)
    go (g : gs) model = do
      fuzzingResult <-
        fuzzingTestSymProgWithModel g spec maxTests p sem prog model
      case fuzzingResult of
        Just ioPair -> return (g : gs, CEGISVerifierFoundCex ioPair)
        Nothing -> go gs model

data SynthesisWithFuzzerTask conVal symVal conProg symProg ctx where
  SynthesisWithFuzzerTask ::
    forall config h conVal symVal ctx conProg symProg semObj.
    ( ConfigurableSolver config h,
      ProgSemantics semObj symProg symVal ctx,
      ProgSemantics semObj conProg conVal ConcreteContext,
      SynthesisContext ctx,
      SEq symVal,
      ToSym conVal symVal,
      EvaluateSym symProg,
      ToCon symProg conProg,
      Eq conVal,
      Show conVal,
      Show symVal
    ) =>
    { synthesisWithFuzzerTaskSolverConfig :: config,
      synthesisWithFuzzerTaskSpec :: [conVal] -> [conVal],
      synthesisWithFuzzerTaskMaxTests :: Int,
      synthesisWithFuzzerTaskGenerators :: [Gen [conVal]],
      synthesisWithFuzzerTaskSemantics :: semObj,
      synthesisWithFuzzerTaskSymProg :: symProg
    } ->
    SynthesisWithFuzzerTask conVal symVal conProg symProg ctx

instance
  ToSynthesisTask
    (SynthesisWithFuzzerTask conVal symVal conProg symProg ctx)
  where
  type
    ConValType (SynthesisWithFuzzerTask conVal symVal conProg symProg ctx) =
      conVal
  type
    SymValType (SynthesisWithFuzzerTask conVal symVal conProg symProg ctx) =
      symVal
  type
    ConProgType (SynthesisWithFuzzerTask conVal symVal conProg symProg ctx) =
      conProg
  type
    SymProgType (SynthesisWithFuzzerTask conVal symVal conProg symProg ctx) =
      symProg
  type
    ContextType (SynthesisWithFuzzerTask conVal symVal conProg symProg ctx) =
      ctx
  type
    ExceptionType (SynthesisWithFuzzerTask conVal symVal conProg symProg ctx) =
      ()
  toSynthesisTask (SynthesisWithFuzzerTask config spec maxTests gens sem prog) =
    SynthesisTask
      config
      gens
      (fuzzingTestStatefulVerifierFun spec maxTests)
      sem
      prog
