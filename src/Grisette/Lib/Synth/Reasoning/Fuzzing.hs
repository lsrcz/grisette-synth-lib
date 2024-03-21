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

import Data.Proxy (Proxy)
import Grisette
  ( ConfigurableSolver,
    EvaluateSym,
    Mergeable,
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
import Grisette.Lib.Synth.Program.ProgConstraints
  ( ProgConstraints,
    runProgWithConstraints,
  )
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics)
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
        ExceptionType,
        MatcherType,
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
    ProgSemantics semObj conProg conVal ConcreteContext,
    ProgConstraints constrObj conProg ConcreteContext
  ) =>
  Gen [conVal] ->
  ([conVal] -> ([conVal], matcher)) ->
  Int ->
  semObj ->
  constrObj ->
  conProg ->
  IO (Maybe (IOPair conVal, matcher))
fuzzingTestProg gen spec maxTests sem constrObj prog = do
  maybeCex <-
    quickCheckWith
      stdArgs {maxSuccess = maxTests, chatty = False}
      ( forAll gen $ \inputs ->
          let (expectedOutputs, matcher) = spec inputs
           in case runProgWithConstraints sem constrObj prog inputs of
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
  forall conVal conProg symProg matcher semObj constrObj p.
  ( Matcher matcher Bool conVal,
    Show conVal,
    ProgSemantics semObj conProg conVal ConcreteContext,
    ProgConstraints constrObj conProg ConcreteContext,
    ToCon symProg conProg,
    EvaluateSym symProg
  ) =>
  Gen [conVal] ->
  ([conVal] -> ([conVal], matcher)) ->
  Int ->
  p conProg ->
  semObj ->
  constrObj ->
  symProg ->
  Model ->
  IO (Maybe (IOPair conVal, matcher))
fuzzingTestSymProgWithModel gen spec maxTests _ sem constr prog model = do
  fuzzingTestProg
    gen
    spec
    maxTests
    sem
    constr
    (evaluateSymToCon model prog :: conProg)

fuzzingTestStatefulVerifierFun ::
  forall conVal conProg symProg matcher semObj constrObj p.
  ( Matcher matcher Bool conVal,
    Show conVal,
    ProgSemantics semObj conProg conVal ConcreteContext,
    ProgConstraints constrObj conProg ConcreteContext,
    ToCon symProg conProg,
    EvaluateSym symProg
  ) =>
  ([conVal] -> ([conVal], matcher)) ->
  Int ->
  p conProg ->
  semObj ->
  constrObj ->
  symProg ->
  StatefulVerifierFun [Gen [conVal]] (IOPair conVal, matcher) ()
fuzzingTestStatefulVerifierFun spec maxTests p sem constr prog = go
  where
    go [] _ = return ([], CEGISVerifierNoCex)
    go (g : gs) model = do
      fuzzingResult <-
        fuzzingTestSymProgWithModel g spec maxTests p sem constr prog model
      case fuzzingResult of
        Just cex ->
          return (g : gs, CEGISVerifierFoundCex cex)
        Nothing -> go gs model

data SynthesisWithFuzzerMatcherTask conVal conProg matcher where
  SynthesisWithFuzzerMatcherTask ::
    forall
      config
      h
      conVal
      symVal
      ctx
      conProg
      symProg
      matcher
      conSemObj
      symSemObj
      conConstrObj
      symConstrObj.
    ( ConfigurableSolver config h,
      ProgSemantics symSemObj symProg symVal ctx,
      ProgSemantics conSemObj conProg conVal ConcreteContext,
      ProgConstraints symConstrObj symProg ctx,
      ProgConstraints conConstrObj conProg ConcreteContext,
      SynthesisContext ctx,
      ToSym conVal symVal,
      EvaluateSym symProg,
      ToCon symProg conProg,
      Matcher matcher SymBool symVal,
      Matcher matcher Bool conVal,
      Show conVal,
      Show symVal,
      Mergeable symVal
    ) =>
    { synthesisWithFuzzerMatcherTaskContextType :: Proxy ctx,
      synthesisWithFuzzerMatcherTaskSymValType :: Proxy symVal,
      synthesisWithFuzzerMatcherTaskSolverConfig :: config,
      synthesisWithFuzzerMatcherTaskSpec :: [conVal] -> ([conVal], matcher),
      synthesisWithFuzzerMatcherTaskMaxTests :: Int,
      synthesisWithFuzzerMatcherTaskGenerators :: [Gen [conVal]],
      synthesisWithFuzzerMatcherTaskConSemantics :: conSemObj,
      synthesisWithFuzzerMatcherTaskSymSemantics :: symSemObj,
      synthesisWithFuzzerMatcherTaskConConstraints :: conConstrObj,
      synthesisWithFuzzerMatcherTaskSymConstraints :: symConstrObj,
      synthesisWithFuzzerMatcherTaskSymProg :: symProg
    } ->
    SynthesisWithFuzzerMatcherTask conVal conProg matcher

data SynthesisWithFuzzerTask conVal conProg where
  SynthesisWithFuzzerTask ::
    ( ConfigurableSolver config h,
      ProgSemantics symSemObj symProg symVal ctx,
      ProgSemantics conSemObj conProg conVal ConcreteContext,
      ProgConstraints symConstrObj symProg ctx,
      ProgConstraints conConstrObj conProg ConcreteContext,
      SynthesisContext ctx,
      ToSym conVal symVal,
      EvaluateSym symProg,
      ToCon symProg conProg,
      SEq symVal,
      Eq conVal,
      Show conVal,
      Show symVal,
      Mergeable symVal
    ) =>
    { synthesisWithFuzzerTaskContextType :: Proxy ctx,
      synthesisWithFuzzerTaskSymValType :: Proxy symVal,
      synthesisWithFuzzerTaskSolverConfig :: config,
      synthesisWithFuzzerTaskSpec :: [conVal] -> [conVal],
      synthesisWithFuzzerTaskMaxTests :: Int,
      synthesisWithFuzzerTaskGenerators :: [Gen [conVal]],
      synthesisWithFuzzerTaskConSemantics :: conSemObj,
      synthesisWithFuzzerTaskSymSemantics :: symSemObj,
      synthesisWithFuzzerTaskConConstraints :: conConstrObj,
      synthesisWithFuzzerTaskSymConstraints :: symConstrObj,
      synthesisWithFuzzerTaskSymProg :: symProg
    } ->
    SynthesisWithFuzzerTask conVal conProg

instance
  ToSynthesisTask
    (SynthesisWithFuzzerMatcherTask conVal conProg matcher)
  where
  type
    ConValType (SynthesisWithFuzzerMatcherTask conVal conProg matcher) =
      conVal
  type
    ConProgType (SynthesisWithFuzzerMatcherTask conVal conProg matcher) =
      conProg
  type
    MatcherType (SynthesisWithFuzzerMatcherTask conVal conProg matcher) =
      matcher
  type
    ExceptionType (SynthesisWithFuzzerMatcherTask conVal conProg matcher) =
      ()
  toSynthesisTask
    ( SynthesisWithFuzzerMatcherTask
        pctx
        psymVal
        config
        spec
        maxTests
        gens
        conSem
        symSem
        conConstr
        symConstr
        prog
      ) =
      SynthesisTask
        pctx
        psymVal
        config
        gens
        (fuzzingTestStatefulVerifierFun spec maxTests)
        conSem
        symSem
        conConstr
        symConstr
        prog

instance ToSynthesisTask (SynthesisWithFuzzerTask conVal conProg) where
  type ConValType (SynthesisWithFuzzerTask conVal conProg) = conVal
  type ConProgType (SynthesisWithFuzzerTask conVal conProg) = conProg
  type MatcherType (SynthesisWithFuzzerTask conVal conProg) = EqMatcher
  type ExceptionType (SynthesisWithFuzzerTask conVal conProg) = ()
  toSynthesisTask
    ( SynthesisWithFuzzerTask
        pctx
        psymVal
        config
        spec
        maxTests
        gens
        conSem
        symSem
        conConstr
        symConstr
        prog
      ) =
      SynthesisTask
        pctx
        psymVal
        config
        gens
        ( fuzzingTestStatefulVerifierFun
            (\inputs -> (spec inputs, EqMatcher))
            maxTests
        )
        conSem
        symSem
        conConstr
        symConstr
        prog
