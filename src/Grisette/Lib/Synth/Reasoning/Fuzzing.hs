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
    SynthesisWithFuzzerMatcherTask (..),
  )
where

import Grisette
  ( ConfigurableSolver,
    EvaluateSym,
    Model,
    SEq,
    StatefulVerifierFun,
    SymBool,
    ToCon,
    ToSym,
    VerifierResult (CEGISVerifierFoundCex, CEGISVerifierNoCex),
    evaluateSymToCon,
  )
import Grisette.Lib.Synth.Context (ConcreteContext)
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Reasoning.IOPair (IOPair (IOPair))
import Grisette.Lib.Synth.Reasoning.Matcher
  ( EqMatcher (EqMatcher),
    Matcher (match),
  )
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( SynthesisContext,
    SynthesisTask (SynthesisTask),
    ToSynthesisTask
      ( ConProgType,
        ConValType,
        ContextType,
        ExceptionType,
        MatcherType,
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
  ( Matcher matcher Bool conVal,
    Show conVal,
    ProgSemantics semObj conProg conVal ConcreteContext
  ) =>
  Gen [conVal] ->
  ([conVal] -> ([conVal], matcher)) ->
  Int ->
  semObj ->
  conProg ->
  IO (Maybe (IOPair conVal, matcher))
fuzzingTestProg gen spec maxTests sem prog = do
  maybeCex <-
    quickCheckWith
      stdArgs {maxSuccess = maxTests, chatty = False}
      ( forAll gen $ \inputs ->
          let (expectedOutputs, matcher) = spec inputs
           in case runProg sem prog inputs of
                Left _ -> False
                Right actualOutputs ->
                  match matcher actualOutputs expectedOutputs
      )
  case maybeCex of
    Nothing -> return Nothing
    Just (cex :&: _) -> do
      let (outputs, matcher) = spec cex
      return $ Just (IOPair cex outputs, matcher)

fuzzingTestSymProgWithModel ::
  forall conVal conProg symProg matcher semObj p.
  ( Matcher matcher Bool conVal,
    Show conVal,
    ProgSemantics semObj conProg conVal ConcreteContext,
    ToCon symProg conProg,
    EvaluateSym symProg
  ) =>
  Gen [conVal] ->
  ([conVal] -> ([conVal], matcher)) ->
  Int ->
  p conProg ->
  semObj ->
  symProg ->
  Model ->
  IO (Maybe (IOPair conVal, matcher))
fuzzingTestSymProgWithModel gen spec maxTests _ sem prog model = do
  fuzzingTestProg
    gen
    spec
    maxTests
    sem
    (evaluateSymToCon model prog :: conProg)

fuzzingTestStatefulVerifierFun ::
  forall conVal conProg symProg matcher semObj p.
  ( Matcher matcher Bool conVal,
    Show conVal,
    ProgSemantics semObj conProg conVal ConcreteContext,
    ToCon symProg conProg,
    EvaluateSym symProg
  ) =>
  ([conVal] -> ([conVal], matcher)) ->
  Int ->
  p conProg ->
  semObj ->
  symProg ->
  StatefulVerifierFun [Gen [conVal]] (IOPair conVal, matcher) ()
fuzzingTestStatefulVerifierFun spec maxTests p sem prog = go
  where
    go [] _ = return ([], CEGISVerifierNoCex)
    go (g : gs) model = do
      fuzzingResult <-
        fuzzingTestSymProgWithModel g spec maxTests p sem prog model
      case fuzzingResult of
        Just cex ->
          return (g : gs, CEGISVerifierFoundCex cex)
        Nothing -> go gs model

data
  SynthesisWithFuzzerMatcherTask
    conVal
    symVal
    conProg
    symProg
    matcher
    ctx
  where
  SynthesisWithFuzzerMatcherTask ::
    forall config h conVal symVal ctx conProg symProg matcher semObj.
    ( ConfigurableSolver config h,
      ProgSemantics semObj symProg symVal ctx,
      ProgSemantics semObj conProg conVal ConcreteContext,
      SynthesisContext ctx,
      ToSym conVal symVal,
      EvaluateSym symProg,
      ToCon symProg conProg,
      Matcher matcher SymBool symVal,
      Matcher matcher Bool conVal,
      Show conVal,
      Show symVal
    ) =>
    { synthesisWithFuzzerMatcherTaskSolverConfig :: config,
      synthesisWithFuzzerMatcherTaskSpec :: [conVal] -> ([conVal], matcher),
      synthesisWithFuzzerMatcherTaskMaxTests :: Int,
      synthesisWithFuzzerMatcherTaskGenerators :: [Gen [conVal]],
      synthesisWithFuzzerMatcherTaskSemantics :: semObj,
      synthesisWithFuzzerMatcherTaskSymProg :: symProg
    } ->
    SynthesisWithFuzzerMatcherTask conVal symVal conProg symProg matcher ctx

data SynthesisWithFuzzerTask conVal symVal conProg symProg ctx where
  SynthesisWithFuzzerTask ::
    forall config h conVal symVal ctx conProg symProg semObj.
    ( ConfigurableSolver config h,
      ProgSemantics semObj symProg symVal ctx,
      ProgSemantics semObj conProg conVal ConcreteContext,
      SynthesisContext ctx,
      ToSym conVal symVal,
      EvaluateSym symProg,
      ToCon symProg conProg,
      SEq symVal,
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
    (SynthesisWithFuzzerMatcherTask conVal symVal conProg symProg matcher ctx)
  where
  type
    ConValType
      (SynthesisWithFuzzerMatcherTask conVal symVal conProg symProg matcher ctx) =
      conVal
  type
    SymValType
      (SynthesisWithFuzzerMatcherTask conVal symVal conProg symProg matcher ctx) =
      symVal
  type
    ConProgType
      (SynthesisWithFuzzerMatcherTask conVal symVal conProg symProg matcher ctx) =
      conProg
  type
    SymProgType
      (SynthesisWithFuzzerMatcherTask conVal symVal conProg symProg matcher ctx) =
      symProg
  type
    MatcherType
      (SynthesisWithFuzzerMatcherTask conVal symVal conProg symProg matcher ctx) =
      matcher
  type
    ContextType
      (SynthesisWithFuzzerMatcherTask conVal symVal conProg symProg matcher ctx) =
      ctx
  type
    ExceptionType
      (SynthesisWithFuzzerMatcherTask conVal symVal conProg symProg matcher ctx) =
      ()
  toSynthesisTask
    (SynthesisWithFuzzerMatcherTask config spec maxTests gens sem prog) =
      SynthesisTask
        config
        gens
        (fuzzingTestStatefulVerifierFun spec maxTests)
        sem
        prog

instance
  ToSynthesisTask
    (SynthesisWithFuzzerTask conVal symVal conProg symProg ctx)
  where
  type
    ConValType
      (SynthesisWithFuzzerTask conVal symVal conProg symProg ctx) =
      conVal
  type
    SymValType
      (SynthesisWithFuzzerTask conVal symVal conProg symProg ctx) =
      symVal
  type
    ConProgType
      (SynthesisWithFuzzerTask conVal symVal conProg symProg ctx) =
      conProg
  type
    SymProgType
      (SynthesisWithFuzzerTask conVal symVal conProg symProg ctx) =
      symProg
  type
    MatcherType
      (SynthesisWithFuzzerTask conVal symVal conProg symProg ctx) =
      EqMatcher
  type
    ContextType
      (SynthesisWithFuzzerTask conVal symVal conProg symProg ctx) =
      ctx
  type
    ExceptionType
      (SynthesisWithFuzzerTask conVal symVal conProg symProg ctx) =
      ()
  toSynthesisTask (SynthesisWithFuzzerTask config spec maxTests gens sem prog) =
    SynthesisTask
      config
      gens
      ( fuzzingTestStatefulVerifierFun
          (\inputs -> (spec inputs, EqMatcher))
          maxTests
      )
      sem
      prog
