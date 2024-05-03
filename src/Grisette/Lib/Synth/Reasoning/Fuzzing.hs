{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Grisette.Lib.Synth.Reasoning.Fuzzing
  ( fuzzingTestProg,
    fuzzingTestSymProgWithModel,
    QuickCheckFuzzer (..),
  )
where

import Data.Data (Typeable)
import Data.Proxy (Proxy (Proxy))
import Grisette
  ( EvaluateSym,
    Mergeable,
    Model,
    SymBool,
    ToCon,
    ToSym (toSym),
    VerifierResult (CEGISVerifierFoundCex, CEGISVerifierNoCex),
    evaluateSymToCon,
  )
import Grisette.Lib.Synth.Context (ConcreteContext)
import Grisette.Lib.Synth.Program.ProgConstraints
  ( ProgConstraints,
    WithConstraints,
  )
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Reasoning.IOPair (IOPair (IOPair))
import Grisette.Lib.Synth.Reasoning.Matcher (Matcher (match))
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
                  symSem
                  (toSym ioPair :: IOPair symVal)
                  matcher
            )
        Nothing -> return CEGISVerifierNoCex
