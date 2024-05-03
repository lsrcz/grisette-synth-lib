{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Grisette.Lib.Synth.Reasoning.Synthesis
  ( SynthesisContext (..),
    SynthesisResult (..),
    SynthesisTask (..),
    runSynthesisTask,
    IsVerifier (..),
    SomeVerifier (..),
    VerificationCex (..),
    runSynthesisTaskExtractCex,
  )
where

import Control.Monad.Except (runExceptT)
import Data.Data (Typeable)
import Data.Proxy (Proxy)
import qualified Data.Text as T
import Grisette
  ( CEGISResult (CEGISSolverFailure, CEGISSuccess, CEGISVerifierFailure),
    ConfigurableSolver,
    EvaluateSym,
    Mergeable,
    Solvable (con),
    SolvingFailure,
    SymBool,
    SynthesisConstraintFun,
    ToCon,
    VerifierFun,
    evaluateSymToCon,
    genericCEGIS,
    identifier,
    runFreshT,
    simpleMerge,
    withInfo,
  )
import Grisette.Lib.Synth.Context (AngelicContext, SymbolicContext)
import Grisette.Lib.Synth.Program.ProgConstraints
  ( ProgConstraints,
    WithConstraints,
    runProgWithConstraints,
  )
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics)
import Grisette.Lib.Synth.Reasoning.IOPair
  ( IOPair (ioPairInputs, ioPairOutputs),
  )
import Grisette.Lib.Synth.Reasoning.Matcher (Matcher (match))
import Grisette.Lib.Synth.Util.Show (showText)

class SynthesisContext ctx where
  genSynthesisConstraint ::
    (Matcher matcher SymBool val, Mergeable val) =>
    Int ->
    matcher ->
    ctx [val] ->
    [val] ->
    SymBool

instance SynthesisContext SymbolicContext where
  genSynthesisConstraint _ matcher actual expectedOutputs = simpleMerge $ do
    actualVal <- runExceptT actual
    case actualVal of
      Left _ -> return $ con False
      Right actualOutputs ->
        return $ match matcher actualOutputs expectedOutputs

instance SynthesisContext AngelicContext where
  genSynthesisConstraint i matcher actual =
    genSynthesisConstraint
      i
      matcher
      ( runFreshT
          actual
          (withInfo (identifier $ showText i) ("synth" :: T.Text))
      )

data VerificationCex symProg where
  VerificationCex ::
    forall symSemObj symConstObj symProg symVal ctx matcher.
    ( ProgSemantics symSemObj symProg symVal ctx,
      ProgConstraints symConstObj symProg ctx,
      SynthesisContext ctx,
      Matcher matcher SymBool symVal,
      Mergeable symVal,
      Typeable symSemObj,
      Typeable symConstObj,
      Typeable symVal,
      Typeable matcher
    ) =>
    { verificationCexContext :: Proxy ctx,
      verificationCexSymSemantics :: WithConstraints symSemObj symConstObj,
      verificationCexIOPair :: IOPair symVal,
      verificationCexMatcher :: matcher
    } ->
    VerificationCex symProg

class
  IsVerifier verifier symProg conProg
    | verifier -> symProg conProg
  where
  toVerifierFuns ::
    verifier -> symProg -> [VerifierFun (VerificationCex symProg) ()]

data SomeVerifier symProg conProg where
  SomeVerifier ::
    forall verifier symProg conProg.
    (IsVerifier verifier symProg conProg) =>
    verifier ->
    SomeVerifier symProg conProg

data SynthesisTask symProg conProg where
  SynthesisTask ::
    forall symProg conProg.
    ( EvaluateSym symProg,
      ToCon symProg conProg
    ) =>
    { synthesisTaskVerifiers :: [SomeVerifier symProg conProg],
      synthesisTaskSymProg :: symProg
    } ->
    SynthesisTask symProg conProg

synthesisConstraintFun ::
  forall symProg.
  symProg ->
  SynthesisConstraintFun (VerificationCex symProg)
synthesisConstraintFun
  prog
  i
  (VerificationCex (_ :: Proxy ctx) symSem (iop :: IOPair symVal) matcher) =
    return $
      genSynthesisConstraint
        i
        matcher
        (runProgWithConstraints symSem prog (ioPairInputs iop) :: ctx [symVal])
        (ioPairOutputs iop)

data SynthesisResult conProg
  = SynthesisSuccess conProg
  | SynthesisVerifierFailure
  | SynthesisSolverFailure SolvingFailure
  deriving (Show)

runSynthesisTask ::
  ( ConfigurableSolver config h
  ) =>
  config ->
  SynthesisTask symProg conProg ->
  IO (SynthesisResult conProg)
runSynthesisTask config (SynthesisTask verifiers symProg) = do
  (_, r) <-
    genericCEGIS
      config
      True
      (con True)
      (synthesisConstraintFun symProg)
      ( concatMap
          (\(SomeVerifier verifier) -> toVerifierFuns verifier symProg)
          verifiers
      )
  case r of
    CEGISSuccess model ->
      return $ SynthesisSuccess $ evaluateSymToCon model symProg
    CEGISVerifierFailure () -> return SynthesisVerifierFailure
    CEGISSolverFailure failure -> return $ SynthesisSolverFailure failure

runSynthesisTaskExtractCex ::
  ( ConfigurableSolver config h
  ) =>
  config ->
  SynthesisTask symProg conProg ->
  IO ([VerificationCex symProg], SynthesisResult conProg)
runSynthesisTaskExtractCex config (SynthesisTask verifiers symProg) = do
  (cex, r) <-
    genericCEGIS
      config
      True
      (con True)
      (synthesisConstraintFun symProg)
      ( concatMap
          (\(SomeVerifier verifier) -> toVerifierFuns verifier symProg)
          verifiers
      )
  case r of
    CEGISSuccess model ->
      return (cex, SynthesisSuccess $ evaluateSymToCon model symProg)
    CEGISVerifierFailure () -> return (cex, SynthesisVerifierFailure)
    CEGISSolverFailure failure -> return (cex, SynthesisSolverFailure failure)
