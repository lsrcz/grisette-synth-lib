{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

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
import Grisette.Lib.Synth.Context (AngelicContext, ConcreteContext)
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
    SomeVerifier (SomeVerifier),
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

data QuickCheckFuzzer symVal conVal symProg conProg symCtx where
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
    QuickCheckFuzzer symVal conVal symProg conProg symCtx

instance
  IsVerifier
    (QuickCheckFuzzer symVal conVal symProg conProg symCtx)
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
  forall symVal conVal symProg conProg semObj constObj.
  ( ProgSemantics semObj symProg symVal AngelicContext,
    ProgSemantics semObj conProg conVal ConcreteContext,
    ToCon symProg conProg,
    ToSym conVal symVal,
    EvaluateSym symProg,
    Show conVal,
    Mergeable symVal,
    Typeable symProg,
    Typeable semObj,
    Typeable symVal,
    SEq symVal,
    Eq conVal,
    ProgConstraints constObj symProg AngelicContext,
    Typeable constObj
  ) =>
  semObj ->
  constObj ->
  Gen [conVal] ->
  ([conVal] -> [conVal]) ->
  SomeVerifier symProg conProg
defaultQuickCheckFuzzerWithConstraint semObj constObj gen spec =
  SomeVerifier
    ( QuickCheckFuzzer
        { quickCheckFuzzerSymSemantics = WithConstraints semObj constObj,
          quickCheckFuzzerConSemantics = semObj,
          quickCheckFuzzerMaxTests = 100,
          quickCheckFuzzerGenerators = [gen],
          quickCheckFuzzerSpec = (,EqMatcher) . spec
        } ::
        QuickCheckFuzzer symVal conVal symProg conProg AngelicContext
    )

defaultQuickCheckFuzzer ::
  forall symVal conVal symProg conProg semObj.
  ( ProgSemantics semObj symProg symVal AngelicContext,
    ProgSemantics semObj conProg conVal ConcreteContext,
    ToCon symProg conProg,
    ToSym conVal symVal,
    EvaluateSym symProg,
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
  SomeVerifier symProg conProg
defaultQuickCheckFuzzer semObj =
  defaultQuickCheckFuzzerWithConstraint @symVal semObj ()

defaultSemQuickCheckFuzzer ::
  forall symVal conVal symProg conProg.
  ( ProgSemantics DefaultSem symProg symVal AngelicContext,
    ProgSemantics DefaultSem conProg conVal ConcreteContext,
    ToCon symProg conProg,
    ToSym conVal symVal,
    EvaluateSym symProg,
    Show conVal,
    Mergeable symVal,
    Typeable symProg,
    Typeable symVal,
    SEq symVal,
    Eq conVal
  ) =>
  Gen [conVal] ->
  ([conVal] -> [conVal]) ->
  SomeVerifier symProg conProg
defaultSemQuickCheckFuzzer = defaultQuickCheckFuzzer @symVal DefaultSem
