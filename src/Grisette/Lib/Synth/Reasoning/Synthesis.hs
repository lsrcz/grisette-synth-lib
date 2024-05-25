{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Grisette.Lib.Synth.Reasoning.Synthesis
  ( SynthesisContext (..),
    SynthesisResult (..),
    IsVerifier (..),
    SomeVerifier (..),
    VerificationCex (..),
    SynthesisTask (..),
    synthesisConstraintFun,
    runSynthesisTask,
    runSynthesisTaskExtractCex,
    solverRunRefinableSynthesisTask,
    solverRunRefinableSynthesisTaskExtractCex,
    SynthesisMinimalCostTask (..),
    runSynthesisMinimalCostTask,
    runSynthesisMinimalCostTaskExtractCex,
    solverRunSynthesisMinimalCostTask,
    solverRunSynthesisMinimalCostTaskExtractCex,
  )
where

import Control.Monad.Except (runExceptT)
import Data.Data (Typeable, eqT, type (:~:) (Refl))
import Data.Proxy (Proxy)
import Grisette
  ( CEGISResult (CEGISSolverFailure, CEGISSuccess, CEGISVerifierFailure),
    ConfigurableSolver,
    EvaluateSym,
    Mergeable,
    SOrd ((.<)),
    Solvable (con),
    Solver,
    SolvingFailure,
    SymBool,
    SynthesisConstraintFun,
    ToCon,
    VerifierFun,
    evaluateSymToCon,
    runFreshT,
    simpleMerge,
    solverGenericCEGIS,
    solverGenericCEGISWithRefinement,
    uniqueIdentifier,
    withSolver,
  )
import Grisette.Lib.Synth.Context
  ( AngelicContext,
    ConcreteContext,
    SymbolicContext,
  )
import Grisette.Lib.Synth.Program.ProgConstraints
  ( ProgConstraints,
    WithConstraints,
    runProgWithConstraints,
  )
import Grisette.Lib.Synth.Program.ProgCost (ProgCost (progCost))
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics)
import Grisette.Lib.Synth.Reasoning.IOPair
  ( IOPair (ioPairInputs, ioPairOutputs),
  )
import Grisette.Lib.Synth.Reasoning.Matcher (Matcher (match))

class SynthesisContext ctx where
  genSynthesisConstraint ::
    (Matcher matcher SymBool val, Mergeable val) =>
    matcher ->
    ctx [val] ->
    [val] ->
    IO SymBool

instance SynthesisContext SymbolicContext where
  genSynthesisConstraint matcher actual expectedOutputs =
    return $ simpleMerge $ do
      actualVal <- runExceptT actual
      case actualVal of
        Left _ -> return $ con False
        Right actualOutputs ->
          return $ match matcher actualOutputs expectedOutputs

instance SynthesisContext AngelicContext where
  genSynthesisConstraint matcher actual expectedOutputs = do
    ident <- uniqueIdentifier
    genSynthesisConstraint matcher (runFreshT actual ident) expectedOutputs

data VerificationCex where
  VerificationCex ::
    forall symSemObj symConstObj symProg symVal ctx matcher.
    ( ProgSemantics symSemObj symProg symVal ctx,
      ProgConstraints symConstObj symProg ctx,
      SynthesisContext ctx,
      Matcher matcher SymBool symVal,
      Mergeable symVal,
      Typeable symProg,
      Typeable symSemObj,
      Typeable symConstObj,
      Typeable symVal,
      Typeable matcher
    ) =>
    { verificationCexContext :: Proxy ctx,
      verificationCexSymProg :: Proxy symProg,
      verificationCexSymSemantics :: WithConstraints symSemObj symConstObj,
      verificationCexIOPair :: IOPair symVal,
      verificationCexMatcher :: matcher
    } ->
    VerificationCex

class
  IsVerifier verifier symProg conProg
    | verifier -> symProg conProg
  where
  toVerifierFuns ::
    verifier -> symProg -> [VerifierFun VerificationCex ()]

data SomeVerifier symProg conProg where
  SomeVerifier ::
    forall verifier symProg conProg.
    (IsVerifier verifier symProg conProg) =>
    verifier ->
    SomeVerifier symProg conProg

data SynthesisTask conProg where
  SynthesisTask ::
    forall symProg conProg.
    ( EvaluateSym symProg,
      ToCon symProg conProg,
      Typeable symProg
    ) =>
    { synthesisTaskVerifiers :: [SomeVerifier symProg conProg],
      synthesisTaskSymProg :: symProg
    } ->
    SynthesisTask conProg

data SynthesisMinimalCostTask conProg where
  SynthesisMinimalCostTask ::
    forall symProg conProg cost conCostObj symCostObj.
    ( EvaluateSym symProg,
      ToCon symProg conProg,
      Typeable symProg,
      ProgCost conCostObj conProg cost ConcreteContext,
      ProgCost symCostObj symProg cost AngelicContext,
      SOrd cost
    ) =>
    { synthesisMinimalCostTaskVerifiers :: [SomeVerifier symProg conProg],
      synthesisMinimalCostTaskSymProg :: symProg,
      synthesisMinimalCostTaskInitialMaxCost :: Maybe cost,
      synthesisMinimalCostTaskConCostObj :: conCostObj,
      synthesisMinimalCostTaskSymCostObj :: symCostObj
    } ->
    SynthesisMinimalCostTask conProg

synthesisConstraintFun ::
  forall symProg.
  (Typeable symProg) =>
  symProg ->
  SynthesisConstraintFun VerificationCex
synthesisConstraintFun
  prog
  ( VerificationCex
      (_ :: Proxy ctx)
      (_ :: Proxy symProg')
      symSem
      (iop :: IOPair symVal)
      matcher
    ) =
    case eqT @symProg @symProg' of
      Just Refl ->
        genSynthesisConstraint
          matcher
          ( runProgWithConstraints symSem prog (ioPairInputs iop) ::
              ctx [symVal]
          )
          (ioPairOutputs iop)
      Nothing -> error "Should not happen"

data SynthesisResult conProg
  = SynthesisSuccess conProg
  | SynthesisVerifierFailure
  | SynthesisSolverFailure SolvingFailure
  deriving (Show)

runSynthesisMinimalCostTask ::
  (ConfigurableSolver config h) =>
  config ->
  SynthesisMinimalCostTask conProg ->
  IO (SynthesisResult conProg)
runSynthesisMinimalCostTask config task =
  snd <$> runSynthesisMinimalCostTaskExtractCex config task

runSynthesisMinimalCostTaskExtractCex ::
  forall config h conProg.
  (ConfigurableSolver config h) =>
  config ->
  SynthesisMinimalCostTask conProg ->
  IO ([VerificationCex], SynthesisResult conProg)
runSynthesisMinimalCostTaskExtractCex config task = withSolver config $ \solver ->
  solverRunSynthesisMinimalCostTaskExtractCex solver task

solverRunSynthesisMinimalCostTask ::
  (Solver solver) =>
  solver ->
  SynthesisMinimalCostTask conProg ->
  IO (SynthesisResult conProg)
solverRunSynthesisMinimalCostTask solver task =
  snd <$> solverRunSynthesisMinimalCostTaskExtractCex solver task

solverRunSynthesisMinimalCostTaskExtractCex ::
  forall solver conProg.
  (Solver solver) =>
  solver ->
  SynthesisMinimalCostTask conProg ->
  IO ([VerificationCex], SynthesisResult conProg)
solverRunSynthesisMinimalCostTaskExtractCex
  solver
  ( SynthesisMinimalCostTask
      verifiers
      symProg
      (initialMaxCost :: Maybe cost)
      conCostObj
      symCostObj
    ) = do
    (cex, r) <-
      solverGenericCEGISWithRefinement
        solver
        True
        ( case initialMaxCost of
            Nothing -> con True
            Just maxCost -> symProgCostLessThanMaxCost maxCost
        )
        (synthesisConstraintFun symProg)
        (Just refineFun)
        ( concatMap
            (\(SomeVerifier verifier) -> toVerifierFuns verifier symProg)
            verifiers
        )
    case r of
      CEGISSuccess model ->
        return (cex, SynthesisSuccess $ evaluateSymToCon model symProg)
      CEGISVerifierFailure () -> return (cex, SynthesisVerifierFailure)
      CEGISSolverFailure failure -> return (cex, SynthesisSolverFailure failure)
    where
      symProgCost =
        flip runFreshT "cost" $ progCost symCostObj symProg ::
          SymbolicContext cost
      symProgCostLessThanMaxCost :: cost -> SymBool
      symProgCostLessThanMaxCost maxCost = simpleMerge $ do
        eitherCost <- runExceptT symProgCost
        case eitherCost of
          Left _ -> return $ con False
          Right cost -> return $ cost .< maxCost
      refineFun model = do
        let conProg = evaluateSymToCon model symProg :: conProg
        let conCost = progCost conCostObj conProg :: ConcreteContext cost
        case conCost of
          Left _ -> return $ con False
          Right cost -> return $ symProgCostLessThanMaxCost cost

runSynthesisTask ::
  (ConfigurableSolver config h) =>
  config ->
  SynthesisTask conProg ->
  IO (SynthesisResult conProg)
runSynthesisTask config task = snd <$> runSynthesisTaskExtractCex config task

runSynthesisTaskExtractCex ::
  forall config h conProg.
  (ConfigurableSolver config h) =>
  config ->
  SynthesisTask conProg ->
  IO ([VerificationCex], SynthesisResult conProg)
runSynthesisTaskExtractCex config task = withSolver config $ \solver ->
  solverRunRefinableSynthesisTaskExtractCex solver task

solverRunRefinableSynthesisTask ::
  (Solver solver) =>
  solver ->
  SynthesisTask conProg ->
  IO (SynthesisResult conProg)
solverRunRefinableSynthesisTask solver task =
  snd <$> solverRunRefinableSynthesisTaskExtractCex solver task

solverRunRefinableSynthesisTaskExtractCex ::
  forall solver conProg.
  (Solver solver) =>
  solver ->
  SynthesisTask conProg ->
  IO ([VerificationCex], SynthesisResult conProg)
solverRunRefinableSynthesisTaskExtractCex
  solver
  (SynthesisTask verifiers symProg) = do
    (cex, r) <-
      solverGenericCEGIS
        solver
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
