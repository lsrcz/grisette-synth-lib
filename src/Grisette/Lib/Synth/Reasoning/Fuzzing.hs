{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Reasoning.Fuzzing
  ( fuzzingTestProg,
    fuzzingTestSymProgWithModel,
    fuzzingTestStatefulVerifierFun,
  )
where

import Grisette
  ( EvaluateSym,
    Model,
    StatefulVerifierFun,
    ToCon,
    VerifierResult (CEGISVerifierFoundCex, CEGISVerifierNoCex),
    evaluateSymToCon,
  )
import Grisette.Lib.Synth.Context (ConcreteContext)
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Reasoning.IOPair (IOPair (IOPair))
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
