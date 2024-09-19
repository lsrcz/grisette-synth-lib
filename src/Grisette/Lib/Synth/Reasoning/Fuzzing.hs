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
    ToSym,
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

data QuickCheckFuzzer symVal conVal symProg conProg where
  QuickCheckFuzzer ::
    ( ProgSemantics symSemObj symProg symVal AngelicContext,
      ProgSemantics conSemObj conProg conVal ConcreteContext,
      Matcher matcher SymBool symVal,
      Matcher matcher Bool conVal,
      Eq conSemObj,
      Typeable conSemObj,
      Typeable symSemObj,
      Typeable matcher,
      NFData conSemObj,
      NFData symSemObj,
      NFData matcher,
      Eq conSemObj,
      Eq symSemObj,
      Eq matcher
    ) =>
    { quickCheckFuzzerSymSemantics :: symSemObj,
      quickCheckFuzzerConSemantics :: conSemObj,
      quickCheckFuzzerMaxTests :: Int,
      quickCheckFuzzerGenerators :: [Gen [conVal]],
      quickCheckFuzzerSpec :: [conVal] -> ([conVal], matcher)
    } ->
    QuickCheckFuzzer symVal conVal symProg conProg

instance
  ( Show conVal,
    ToCon symProg conProg,
    EvalSym symProg,
    Mergeable symVal,
    Show symVal,
    PPrint conVal,
    Eq conVal,
    NFData conVal,
    Typeable conVal,
    Typeable symVal,
    ToSym conVal symVal
  ) =>
  IsVerifier
    (QuickCheckFuzzer symVal conVal symProg conProg)
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
                SomeExample $
                  Example
                    conSem
                    symSem
                    (Proxy :: Proxy symVal)
                    (ioPair :: IOPair conVal)
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
    Typeable conVal,
    Typeable symVal,
    SymEq symVal,
    Show symVal,
    PPrint conVal,
    Eq conVal,
    Typeable constObj,
    NFData conVal,
    NFData semObj,
    NFData constObj,
    Eq semObj,
    Eq conVal,
    Eq constObj
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
        QuickCheckFuzzer symVal conVal symProg conProg
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
    Typeable conVal,
    Show symVal,
    PPrint conVal,
    SymEq symVal,
    Eq conVal,
    NFData conVal,
    NFData semObj,
    Eq semObj,
    Eq symVal
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
    Typeable conVal,
    Typeable symVal,
    Show symVal,
    PPrint conVal,
    SymEq symVal,
    Eq conVal,
    NFData conVal,
    Eq symVal
  ) =>
  Gen [conVal] ->
  ([conVal] -> [conVal]) ->
  SomeVerifier symProg conProg
defaultSemQuickCheckFuzzer = defaultQuickCheckFuzzer @symVal DefaultSem
