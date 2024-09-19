{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Lib.Synth.Reasoning.Fuzzing
  ( fuzzingTestProg,
    fuzzingTestSymProgWithModel,
    QuickCheckFuzzer (..),
    defaultQuickCheckFuzzerWithConstraint,
    defaultQuickCheckFuzzer,
    defaultSemQuickCheckFuzzer,
  )
where

import Control.DeepSeq (NFData)
import Control.Exception.Safe (Exception (fromException), throw)
import Data.Data (Typeable)
import Data.Proxy (Proxy (Proxy))
import Grisette
  ( EvalSym,
    Mergeable,
    Model,
    PPrint,
    SymBool,
    SymEq,
    ToCon,
    ToSym (toSym),
    VerifierResult (CEGISVerifierFoundCex, CEGISVerifierNoCex),
    evalSymToCon,
  )
import Grisette.Lib.Synth.Context (AngelicContext, ConcreteContext)
import Grisette.Lib.Synth.Operator.OpSemantics (DefaultSem (DefaultSem))
import Grisette.Lib.Synth.Program.ProgConstraints
  ( WithConstraints (WithConstraints),
  )
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Reasoning.IOPair (IOPair (IOPair))
import Grisette.Lib.Synth.Reasoning.Matcher
  ( EqMatcher (EqMatcher),
    Matcher (match),
  )
import Grisette.Lib.Synth.Reasoning.Parallel.ThreadPool (CancellingException)
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( Example (Example),
    IsVerifier (toVerifierFuns),
    SomeExample (SomeExample),
    SomeVerifier (SomeVerifier),
    SynthesisContext,
  )
import Test.QuickCheck.Counterexamples
  ( Args (chatty),
    Gen,
    PropertyOf,
    Result (Failure, theException),
    Testable (Counterexample),
    forAll,
    quickCheckWithResult,
    stdArgs,
    withMaxSuccess,
    type (:&:) ((:&:)),
  )

fuzzingTestProg' ::
  ( Matcher matcher Bool conVal,
    Show conVal,
    Testable prop,
    Counterexample prop ~ ([conVal] :&: ())
  ) =>
  prop ->
  ([conVal] -> ([conVal], matcher)) ->
  IO (Maybe (IOPair conVal, matcher))
fuzzingTestProg' prop spec = do
  (maybeCex, result) <- quickCheckWithResult stdArgs {chatty = False} prop
  case maybeCex of
    Nothing -> return Nothing
    Just (cex :&: _) -> do
      case result of
        Failure {theException = Just e} ->
          case fromException e of
            Just (se :: CancellingException) -> throw se
            _ -> return ()
        _ -> return ()
      let (outputs, matcher) = spec cex
      return $ Just (IOPair cex outputs, matcher)

propertyWithMatcher ::
  ( Matcher matcher Bool conVal,
    Show conVal,
    ProgSemantics conSemObj conProg conVal ConcreteContext
  ) =>
  Gen [conVal] ->
  ([conVal] -> ([conVal], matcher)) ->
  Int ->
  conSemObj ->
  conProg ->
  PropertyOf ([conVal] :&: ())
propertyWithMatcher gen spec maxTests sem prog = forAll gen $ \inputs ->
  withMaxSuccess maxTests $
    let (expectedOutputs, matcher) = spec inputs
     in case runProg sem prog inputs of
          Left _ -> False
          Right actualOutputs ->
            match matcher actualOutputs expectedOutputs

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
fuzzingTestProg gen spec maxTests sem prog =
  fuzzingTestProg' (propertyWithMatcher gen spec maxTests sem prog) spec

fuzzingTestSymProgWithModel ::
  forall conVal conProg symProg matcher conSemObj p.
  ( Matcher matcher Bool conVal,
    Show conVal,
    ProgSemantics conSemObj conProg conVal ConcreteContext,
    ToCon symProg conProg,
    EvalSym symProg
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
    (evalSymToCon model prog :: conProg)

data QuickCheckFuzzer symVal conVal symProg conProg symCtx where
  QuickCheckFuzzer ::
    ( ProgSemantics (WithConstraints symSemObj symConstObj) symProg symVal symCtx,
      ProgSemantics conSemObj conProg conVal ConcreteContext,
      Matcher matcher SymBool symVal,
      Matcher matcher Bool conVal,
      Typeable symSemObj,
      Typeable symConstObj,
      Typeable matcher,
      NFData symSemObj,
      NFData symConstObj,
      NFData matcher
    ) =>
    { quickCheckFuzzerSymSemantics :: WithConstraints symSemObj symConstObj,
      quickCheckFuzzerConSemantics :: conSemObj,
      quickCheckFuzzerMaxTests :: Int,
      quickCheckFuzzerGenerators :: [Gen [conVal]],
      quickCheckFuzzerSpec :: [conVal] -> ([conVal], matcher)
    } ->
    QuickCheckFuzzer symVal conVal symProg conProg symCtx

instance
  ( Show conVal,
    ToCon symProg conProg,
    EvalSym symProg,
    SynthesisContext symCtx,
    Mergeable symVal,
    Show symVal,
    PPrint symVal,
    NFData symVal,
    Typeable symVal,
    ToSym conVal symVal
  ) =>
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
                SomeExample (Proxy :: Proxy symCtx) $
                  Example
                    symSem
                    (toSym ioPair :: IOPair symVal)
                    matcher
            )
        Nothing -> return CEGISVerifierNoCex

defaultQuickCheckFuzzerWithConstraint ::
  forall symVal conVal symProg conProg semObj constObj.
  ( ProgSemantics
      (WithConstraints semObj constObj)
      symProg
      symVal
      AngelicContext,
    ProgSemantics semObj conProg conVal ConcreteContext,
    ToCon symProg conProg,
    ToSym conVal symVal,
    EvalSym symProg,
    Show conVal,
    Mergeable symVal,
    Typeable symProg,
    Typeable semObj,
    Typeable symVal,
    SymEq symVal,
    Show symVal,
    PPrint symVal,
    Eq conVal,
    Typeable constObj,
    NFData symVal,
    NFData semObj,
    NFData constObj
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
  ( ProgSemantics
      (WithConstraints semObj ())
      symProg
      symVal
      AngelicContext,
    ProgSemantics semObj conProg conVal ConcreteContext,
    ToCon symProg conProg,
    ToSym conVal symVal,
    EvalSym symProg,
    Show conVal,
    Mergeable symVal,
    Typeable symProg,
    Typeable semObj,
    Typeable symVal,
    Show symVal,
    PPrint symVal,
    SymEq symVal,
    Eq conVal,
    NFData symVal,
    NFData semObj
  ) =>
  semObj ->
  Gen [conVal] ->
  ([conVal] -> [conVal]) ->
  SomeVerifier symProg conProg
defaultQuickCheckFuzzer semObj =
  defaultQuickCheckFuzzerWithConstraint @symVal semObj ()

defaultSemQuickCheckFuzzer ::
  forall symVal conVal symProg conProg.
  ( ProgSemantics
      (WithConstraints DefaultSem ())
      symProg
      symVal
      AngelicContext,
    ProgSemantics DefaultSem conProg conVal ConcreteContext,
    ToCon symProg conProg,
    ToSym conVal symVal,
    EvalSym symProg,
    Show conVal,
    Mergeable symVal,
    Typeable symProg,
    Typeable symVal,
    Show symVal,
    PPrint symVal,
    SymEq symVal,
    Eq conVal,
    NFData symVal
  ) =>
  Gen [conVal] ->
  ([conVal] -> [conVal]) ->
  SomeVerifier symProg conProg
defaultSemQuickCheckFuzzer = defaultQuickCheckFuzzer @symVal DefaultSem
