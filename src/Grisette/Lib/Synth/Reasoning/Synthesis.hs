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
    IsVerifier (..),
    SomeVerifier (..),
    Example (..),
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
import Data.Data (Typeable)
import Data.Proxy (Proxy)
import Grisette
  ( CEGISResult (CEGISSolverFailure, CEGISSuccess, CEGISVerifierFailure),
    ConfigurableSolver,
    EvalSym,
    Mergeable,
    PPrint (pformat),
    Solvable (con),
    Solver,
    SolvingFailure,
    SymBool,
    SymOrd ((.<)),
    SynthesisConstraintFun,
    ToCon,
    VerifierFun,
    evalSymToCon,
    runFreshT,
    simpleMerge,
    solverGenericCEGIS,
    solverGenericCEGISWithRefinement,
    symAnd,
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
    ident <- uniqueIdentifier "synth"
    genSynthesisConstraint matcher (runFreshT actual ident) expectedOutputs

data Example symProg where
  Example ::
    forall symSemObj symConstObj symProg symVal ctx matcher.
    ( ProgSemantics symSemObj symProg symVal ctx,
      ProgConstraints symConstObj symProg ctx,
      SynthesisContext ctx,
      Matcher matcher SymBool symVal,
      Mergeable symVal,
      Typeable symSemObj,
      Typeable symConstObj,
      Typeable symVal,
      Show symVal,
      PPrint symVal,
      Typeable matcher
    ) =>
    { exampleContext :: Proxy ctx,
      exampleSymSemantics :: WithConstraints symSemObj symConstObj,
      exampleIOPair :: IOPair symVal,
      exampleMatcher :: matcher
    } ->
    Example symProg

instance Show (Example symProg) where
  show (Example _ _ iop _) = show iop

instance PPrint (Example symProg) where
  pformat (Example _ _ iop _) = pformat iop

class
  IsVerifier verifier symProg conProg
    | verifier -> symProg conProg
  where
  toVerifierFuns ::
    verifier -> symProg -> [VerifierFun (Example symProg) ()]

data SomeVerifier symProg conProg where
  SomeVerifier ::
    forall verifier symProg conProg.
    (IsVerifier verifier symProg conProg) =>
    verifier ->
    SomeVerifier symProg conProg

data SynthesisTask symProg conProg where
  SynthesisTask ::
    forall symProg conProg.
    ( EvalSym symProg,
      ToCon symProg conProg,
      Typeable symProg
    ) =>
    { synthesisVerifiers :: [SomeVerifier symProg conProg],
      synthesisInitialExamples :: [Example symProg],
      synthesisSketch :: symProg
    } ->
    SynthesisTask symProg conProg

data SynthesisMinimalCostTask symProg conProg where
  SynthesisMinimalCostTask ::
    forall symProg conProg cost conCostObj symCostObj.
    ( EvalSym symProg,
      ToCon symProg conProg,
      ProgCost conCostObj conProg cost ConcreteContext,
      ProgCost symCostObj symProg cost AngelicContext,
      SymOrd cost
    ) =>
    { synthesisMinimalCostTaskVerifiers :: [SomeVerifier symProg conProg],
      synthesisMinimalCostTaskSymProg :: symProg,
      synthesisMinimalCostTaskInitialMaxCost :: Maybe cost,
      synthesisMinimalCostTaskConCostObj :: conCostObj,
      synthesisMinimalCostTaskSymCostObj :: symCostObj
    } ->
    SynthesisMinimalCostTask symProg conProg

synthesisConstraintFun ::
  forall symProg.
  (Typeable symProg) =>
  symProg ->
  SynthesisConstraintFun (Example symProg)
synthesisConstraintFun
  prog
  (Example (_ :: Proxy ctx) symSem (iop :: IOPair symVal) matcher) =
    genSynthesisConstraint
      matcher
      (runProgWithConstraints symSem prog (ioPairInputs iop) :: ctx [symVal])
      (ioPairOutputs iop)

data SynthesisResult conProg
  = SynthesisSuccess conProg
  | SynthesisVerifierFailure
  | SynthesisSolverFailure SolvingFailure
  deriving (Show)

runSynthesisMinimalCostTask ::
  (ConfigurableSolver config h, Typeable symProg) =>
  config ->
  SynthesisMinimalCostTask symProg conProg ->
  IO (SynthesisResult conProg)
runSynthesisMinimalCostTask config task =
  snd <$> runSynthesisMinimalCostTaskExtractCex config task

runSynthesisMinimalCostTaskExtractCex ::
  forall config h symProg conProg.
  (ConfigurableSolver config h, Typeable symProg) =>
  config ->
  SynthesisMinimalCostTask symProg conProg ->
  IO ([Example symProg], SynthesisResult conProg)
runSynthesisMinimalCostTaskExtractCex config task =
  withSolver config $ \solver ->
    solverRunSynthesisMinimalCostTaskExtractCex solver task

solverRunSynthesisMinimalCostTask ::
  (Solver solver, Typeable symProg) =>
  solver ->
  SynthesisMinimalCostTask symProg conProg ->
  IO (SynthesisResult conProg)
solverRunSynthesisMinimalCostTask solver task =
  snd <$> solverRunSynthesisMinimalCostTaskExtractCex solver task

solverRunSynthesisMinimalCostTaskExtractCex ::
  forall solver symProg conProg.
  (Solver solver, Typeable symProg) =>
  solver ->
  SynthesisMinimalCostTask symProg conProg ->
  IO ([Example symProg], SynthesisResult conProg)
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
        return (cex, SynthesisSuccess $ evalSymToCon model symProg)
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
        let conProg = evalSymToCon model symProg :: conProg
        let conCost = progCost conCostObj conProg :: ConcreteContext cost
        case conCost of
          Left _ -> return $ con False
          Right cost -> return $ symProgCostLessThanMaxCost cost

runSynthesisTask ::
  (ConfigurableSolver config h) =>
  config ->
  SynthesisTask symProg conProg ->
  IO (SynthesisResult conProg)
runSynthesisTask config task = snd <$> runSynthesisTaskExtractCex config task

runSynthesisTaskExtractCex ::
  (ConfigurableSolver config h) =>
  config ->
  SynthesisTask symProg conProg ->
  IO ([Example symProg], SynthesisResult conProg)
runSynthesisTaskExtractCex config task = withSolver config $ \solver ->
  solverRunRefinableSynthesisTaskExtractCex solver task

solverRunRefinableSynthesisTask ::
  (Solver solver) =>
  solver ->
  SynthesisTask symProg conProg ->
  IO (SynthesisResult conProg)
solverRunRefinableSynthesisTask solver task =
  snd <$> solverRunRefinableSynthesisTaskExtractCex solver task

solverRunRefinableSynthesisTaskExtractCex ::
  (Solver solver) =>
  solver ->
  SynthesisTask symProg conProg ->
  IO ([Example symProg], SynthesisResult conProg)
solverRunRefinableSynthesisTaskExtractCex
  solver
  (SynthesisTask verifiers examples symProg) = do
    initialExampleConstraints <-
      traverse (synthesisConstraintFun symProg) examples
    (cex, r) <-
      solverGenericCEGIS
        solver
        True
        (symAnd initialExampleConstraints)
        (synthesisConstraintFun symProg)
        ( concatMap
            (\(SomeVerifier verifier) -> toVerifierFuns verifier symProg)
            verifiers
        )
    case r of
      CEGISSuccess model ->
        return (cex, SynthesisSuccess $ evalSymToCon model symProg)
      CEGISVerifierFailure () -> return (cex, SynthesisVerifierFailure)
      CEGISSolverFailure failure -> return (cex, SynthesisSolverFailure failure)
