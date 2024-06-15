{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Grisette.Lib.Synth.Reasoning.Fuzzing
  ( fuzzingTestProg,
    fuzzingTestSymProgWithModel,
    QuickCheckFuzzer (..),
    defaultQuickCheckFuzzerWithConstraint,
    defaultQuickCheckFuzzer,
    defaultSemQuickCheckFuzzer,
  )
where

import Data.Data (Typeable)
import Data.Proxy (Proxy (Proxy))
import Grisette
  ( EvaluateSym,
    Mergeable,
    Model,
    SEq,
    SymBool,
    ToCon,
    ToSym (toSym),
    VerifierResult (CEGISVerifierFoundCex, CEGISVerifierNoCex),
    evaluateSymToCon,
  )
import Grisette.Lib.Synth.Context (ConcreteContext)
import Grisette.Lib.Synth.Operator.OpSemantics (DefaultSem (DefaultSem))
import Grisette.Lib.Synth.Program.ProgConstraints
  ( ProgConstraints,
    WithConstraints (WithConstraints),
  )
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Reasoning.IOPair (IOPair (IOPair))
import Grisette.Lib.Synth.Reasoning.Matcher (EqMatcher (EqMatcher), Matcher (match))
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( IsVerifier (toVerifierFuns),
    SynthesisContext,
    VerificationCex (VerificationCex),
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
    ProgSemantics conSemObj conProg conVal ConcreteContext
  ) =>
  Gen [conVal] ->
  ([conVal] -> ([conVal], matcher)) ->
  Int ->
  conSemObj ->
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
  forall conVal conProg symProg matcher conSemObj p.
  ( Matcher matcher Bool conVal,
    Show conVal,
    ProgSemantics conSemObj conProg conVal ConcreteContext,
    ToCon symProg conProg,
    EvaluateSym symProg
  ) =>
  Gen [conVal] ->
  ([conVal] -> ([conVal], matcher)) ->
  Int ->
  p conProg ->
  conSemObj ->
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

data QuickCheckFuzzer symProg conProg symVal conVal symCtx where
  QuickCheckFuzzer ::
    ( ProgSemantics symSemObj symProg symVal symCtx,
      ProgConstraints symConstObj symProg symCtx,
      ProgSemantics conSemObj conProg conVal ConcreteContext,
      ToCon symProg conProg,
      ToSym conVal symVal,
      EvaluateSym symProg,
      SynthesisContext symCtx,
      Matcher matcher SymBool symVal,
      Matcher matcher Bool conVal,
      Show conVal,
      Mergeable symVal,
      Typeable symProg,
      Typeable symSemObj,
      Typeable symConstObj,
      Typeable symVal,
      Typeable matcher
    ) =>
    { quickCheckFuzzerSymSemantics :: WithConstraints symSemObj symConstObj,
      quickCheckFuzzerConSemantics :: conSemObj,
      quickCheckFuzzerMaxTests :: Int,
      quickCheckFuzzerGenerators :: [Gen [conVal]],
      quickCheckFuzzerSpec :: [conVal] -> ([conVal], matcher)
    } ->
    QuickCheckFuzzer symProg conProg symVal conVal symCtx

instance
  IsVerifier
    (QuickCheckFuzzer symProg conProg symVal conVal symCtx)
    symProg
    conProg
  where
  toVerifierFuns (QuickCheckFuzzer symSem conSem maxTests gens spec) prog =
    flip fmap gens $ \gen model -> do
      fuzzingResult <-
        fuzzingTestSymProgWithModel
          gen
          spec
          maxTests
          (Proxy :: Proxy conProg)
          conSem
          prog
          model
      case fuzzingResult of
        Just (ioPair, matcher) ->
          return
            ( CEGISVerifierFoundCex $
                VerificationCex
                  (Proxy :: Proxy symCtx)
                  (Proxy :: Proxy symProg)
                  symSem
                  (toSym ioPair :: IOPair symVal)
                  matcher
            )
        Nothing -> return CEGISVerifierNoCex

defaultQuickCheckFuzzerWithConstraint ::
  ( ProgSemantics semObj symProg symVal symCtx,
    ProgSemantics semObj conProg conVal ConcreteContext,
    ToCon symProg conProg,
    ToSym conVal symVal,
    EvaluateSym symProg,
    SynthesisContext symCtx,
    Show conVal,
    Mergeable symVal,
    Typeable symProg,
    Typeable semObj,
    Typeable symVal,
    SEq symVal,
    Eq conVal,
    ProgConstraints constObj symProg symCtx,
    Typeable constObj
  ) =>
  semObj ->
  constObj ->
  Gen [conVal] ->
  ([conVal] -> [conVal]) ->
  QuickCheckFuzzer symProg conProg symVal conVal symCtx
defaultQuickCheckFuzzerWithConstraint semObj constObj gen spec =
  QuickCheckFuzzer
    { quickCheckFuzzerSymSemantics = WithConstraints semObj constObj,
      quickCheckFuzzerConSemantics = semObj,
      quickCheckFuzzerMaxTests = 100,
      quickCheckFuzzerGenerators = [gen],
      quickCheckFuzzerSpec = (,EqMatcher) . spec
    }

defaultQuickCheckFuzzer ::
  ( ProgSemantics semObj symProg symVal symCtx,
    ProgSemantics semObj conProg conVal ConcreteContext,
    ToCon symProg conProg,
    ToSym conVal symVal,
    EvaluateSym symProg,
    SynthesisContext symCtx,
    Show conVal,
    Mergeable symVal,
    Typeable symProg,
    Typeable semObj,
    Typeable symVal,
    SEq symVal,
    Eq conVal
  ) =>
  semObj ->
  Gen [conVal] ->
  ([conVal] -> [conVal]) ->
  QuickCheckFuzzer symProg conProg symVal conVal symCtx
defaultQuickCheckFuzzer semObj =
  defaultQuickCheckFuzzerWithConstraint semObj ()

defaultSemQuickCheckFuzzer ::
  ( ProgSemantics DefaultSem symProg symVal symCtx,
    ProgSemantics DefaultSem conProg conVal ConcreteContext,
    ToCon symProg conProg,
    ToSym conVal symVal,
    EvaluateSym symProg,
    SynthesisContext symCtx,
    Show conVal,
    Mergeable symVal,
    Typeable symProg,
    Typeable symVal,
    SEq symVal,
    Eq conVal
  ) =>
  Gen [conVal] ->
  ([conVal] -> [conVal]) ->
  QuickCheckFuzzer symProg conProg symVal conVal symCtx
defaultSemQuickCheckFuzzer = defaultQuickCheckFuzzer DefaultSem
