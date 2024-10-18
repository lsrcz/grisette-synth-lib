{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Lib.Synth.Reasoning.Verification
  ( SMTVerifier (..),
    defaultSMTVerifier,
    defaultSemSMTVerifier,
  )
where

import Control.Monad.Except (runExceptT)
import Data.Typeable (Proxy (Proxy))
import Grisette
  ( EvalSym,
    GrisetteSMTConfig,
    LogicalOp (false, symNot, true),
    Mergeable,
    SolvingFailure (SolvingError, Unsat),
    SymBool,
    ToCon,
    ToSym,
    VerifierResult
      ( CEGISVerifierException,
        CEGISVerifierFoundCex,
        CEGISVerifierNoCex
      ),
    evalSymToCon,
    runFreshT,
    simpleMerge,
    solve,
    uniqueIdentifier,
  )
import Grisette.Lib.Synth.Context (AngelicContext, ConcreteContext, SymbolicContext)
import Grisette.Lib.Synth.Operator.OpSemantics (DefaultSem (DefaultSem))
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics, runSymbol)
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable)
import Grisette.Lib.Synth.Reasoning.IOPair (IOPair (IOPair))
import Grisette.Lib.Synth.Reasoning.Matcher (EqMatcher (EqMatcher), Matcher (match))
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( ConExampleConstraint,
    Example (Example),
    IsVerifier (toVerifierFuns),
    SomeExample (SomeExample),
    SomeVerifier (SomeVerifier),
    SymExampleConstraint,
  )
import System.Time.Extra (Seconds, timeout)

data SMTVerifier symVal conVal symProg conProg where
  SMTVerifier ::
    ( SymExampleConstraint symSemObj symProg symVal matcher,
      ConExampleConstraint conSemObj conProg conVal matcher,
      ProgSemantics conSemObj conProg symVal SymbolicContext,
      Mergeable matcher,
      EvalSym matcher
    ) =>
    { smtVerifierSolverConfig :: GrisetteSMTConfig,
      -- | After this timeout, the verification is considered success for the
      -- synthesis purpose
      smtVerifierTimeoutSeconds :: Maybe Seconds,
      smtVerifierSymSemantics :: symSemObj,
      smtVerifierConSemantics :: conSemObj,
      smtVerifierInputs :: [AngelicContext [symVal]],
      smtVerifierSpec :: [symVal] -> SymbolicContext ([symVal], matcher)
    } ->
    SMTVerifier symVal conVal symProg conProg

instance
  ( EvalSym symProg,
    Mergeable symVal,
    ToCon symVal conVal,
    EvalSym symVal,
    ToSym conVal symVal,
    ToCon symProg conProg,
    Show symVal
  ) =>
  IsVerifier (SMTVerifier symVal conVal symProg conProg) symProg conProg
  where
  toVerifierFuns
    ( SMTVerifier
        config
        timeoutSeconds
        symSem
        conSem
        inputs
        (spec :: ([symVal] -> SymbolicContext ([symVal], matcher)))
      )
    table
    sym =
      flip fmap inputs $ \input model -> do
        let evaledTable = evalSymToCon model table :: SymbolTable conProg
        ident <- uniqueIdentifier "input"
        let generatedInputs = runFreshT input ident
        let specOutput = generatedInputs >>= spec
        let actual =
              generatedInputs
                >>= runSymbol conSem evaledTable sym ::
                SymbolicContext [symVal]
        let result :: SymBool = simpleMerge $ do
              expected <- runExceptT specOutput
              actual <- runExceptT actual
              case (expected, actual) of
                (Left _, _) -> return false
                (_, Left _) -> return true
                (Right (expected, matcher), Right actual) ->
                  return $ symNot $ match matcher actual expected
        let timeoutFun = case timeoutSeconds of
              Nothing -> fmap Just
              Just timeoutSeconds -> timeout timeoutSeconds
        r <- timeoutFun $ solve config result
        case r of
          Nothing -> return CEGISVerifierNoCex
          Just (Left Unsat) -> return CEGISVerifierNoCex
          Just (Left (SolvingError f)) -> return $ CEGISVerifierException f
          Just (Left e) -> error $ "Unexpected solver error: " ++ show e
          Just (Right m) -> do
            let Right evaledInput =
                  evalSymToCon m generatedInputs :: ConcreteContext [conVal]
            let Right (output, matcher) =
                  evalSymToCon m specOutput :: ConcreteContext ([conVal], matcher)
            return $
              CEGISVerifierFoundCex $
                SomeExample $
                  Example
                    conSem
                    symSem
                    (Proxy @symVal)
                    (IOPair evaledInput output)
                    matcher

defaultSMTVerifier ::
  forall conVal symVal conProg symProg semObj.
  ( SymExampleConstraint semObj symProg symVal EqMatcher,
    ConExampleConstraint semObj conProg conVal EqMatcher,
    EvalSym symProg,
    Mergeable symVal,
    ToCon symVal conVal,
    EvalSym symVal,
    ToSym conVal symVal,
    ToCon symProg conProg,
    ProgSemantics semObj conProg symVal SymbolicContext,
    Show symVal
  ) =>
  semObj ->
  GrisetteSMTConfig ->
  Maybe Seconds ->
  [AngelicContext [symVal]] ->
  ([symVal] -> SymbolicContext [symVal]) ->
  SomeVerifier symProg conProg
defaultSMTVerifier semObj config seconds inputs spec =
  SomeVerifier
    ( SMTVerifier
        { smtVerifierSolverConfig = config,
          smtVerifierSymSemantics = semObj,
          smtVerifierTimeoutSeconds = seconds,
          smtVerifierConSemantics = semObj,
          smtVerifierInputs = inputs,
          smtVerifierSpec = fmap (,EqMatcher) . spec
        } ::
        SMTVerifier symVal conVal symProg conProg
    )

defaultSemSMTVerifier ::
  forall conVal symVal conProg symProg.
  ( SymExampleConstraint DefaultSem symProg symVal EqMatcher,
    ConExampleConstraint DefaultSem conProg conVal EqMatcher,
    EvalSym symProg,
    Mergeable symVal,
    ToCon symVal conVal,
    EvalSym symVal,
    ToSym conVal symVal,
    ToCon symProg conProg,
    ProgSemantics DefaultSem conProg symVal SymbolicContext,
    Show symVal
  ) =>
  GrisetteSMTConfig ->
  Maybe Seconds ->
  [AngelicContext [symVal]] ->
  ([symVal] -> SymbolicContext [symVal]) ->
  SomeVerifier symProg conProg
defaultSemSMTVerifier config seconds inputs spec =
  SomeVerifier
    ( SMTVerifier
        { smtVerifierSolverConfig = config,
          smtVerifierTimeoutSeconds = seconds,
          smtVerifierSymSemantics = DefaultSem,
          smtVerifierConSemantics = DefaultSem,
          smtVerifierInputs = inputs,
          smtVerifierSpec = fmap (,EqMatcher) . spec
        } ::
        SMTVerifier symVal conVal symProg conProg
    )
