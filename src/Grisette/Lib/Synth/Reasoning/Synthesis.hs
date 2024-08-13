{-# LANGUAGE DuplicateRecordFields #-}
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
    synthesisConstraintFun,
    RunSynthesisTask (..),
    solverRunSynthesisTask,
    runSynthesisTask,
    runSynthesisTaskExtractCex,
    SynthesisTask (..),
    SynthesisBoundCostTask (..),
    SynthesisMinimalCostTask (..),
  )
where

import Control.Monad.Except (runExceptT)
import Data.Data (Typeable)
import Data.Proxy (Proxy)
import Grisette
  ( CEGISResult (CEGISSolverFailure, CEGISSuccess, CEGISVerifierFailure),
    ConfigurableSolver,
    EvalSym,
    LogicalOp ((.&&)),
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
      synthesisSketch :: symProg,
      synthesisPrecondition :: SymBool
    } ->
    SynthesisTask symProg conProg

data SynthesisBoundCostTask symProg conProg where
  SynthesisBoundCostTask ::
    forall symProg conProg cost symCostObj.
    ( EvalSym symProg,
      ToCon symProg conProg,
      ProgCost symCostObj symProg cost AngelicContext,
      SymOrd cost
    ) =>
    { synthesisVerifiers :: [SomeVerifier symProg conProg],
      synthesisInitialExamples :: [Example symProg],
      synthesisSketch :: symProg,
      synthesisPrecondition :: SymBool,
      synthesisInitialMaxCost :: cost,
      synthesisSymCostObj :: symCostObj
    } ->
    SynthesisBoundCostTask symProg conProg

data SynthesisMinimalCostTask symProg conProg where
  SynthesisMinimalCostTask ::
    forall symProg conProg cost conCostObj symCostObj.
    ( EvalSym symProg,
      ToCon symProg conProg,
      ProgCost conCostObj conProg cost ConcreteContext,
      ProgCost symCostObj symProg cost AngelicContext,
      SymOrd cost
    ) =>
    { synthesisVerifiers :: [SomeVerifier symProg conProg],
      synthesisInitialExamples :: [Example symProg],
      synthesisSketch :: symProg,
      synthesisPrecondition :: SymBool,
      synthesisMaybeInitialMaxCost :: Maybe cost,
      synthesisConCostObj :: conCostObj,
      synthesisSymCostObj :: symCostObj
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

class RunSynthesisTask task symProg conProg | task -> symProg conProg where
  solverRunSynthesisTaskExtractCex ::
    forall solver.
    (Solver solver) =>
    solver ->
    task ->
    IO ([Example symProg], SynthesisResult conProg)
  taskRefinable :: task -> Bool

instance
  (Typeable symProg) =>
  RunSynthesisTask (SynthesisBoundCostTask symProg conProg) symProg conProg
  where
  solverRunSynthesisTaskExtractCex
    solver
    ( SynthesisBoundCostTask
        verifiers
        examples
        symProg
        precond
        (initialMaxCost :: cost)
        symCostObj
      ) = do
      initialExampleConstraints <-
        traverse (synthesisConstraintFun symProg) examples
      let costConstraint = symProgCostLessThanMaxCost initialMaxCost
      (cex, r) <-
        solverGenericCEGIS
          solver
          True
          (precond .&& symAnd initialExampleConstraints .&& costConstraint)
          (synthesisConstraintFun symProg)
          ( concatMap
              (\(SomeVerifier verifier) -> toVerifierFuns verifier symProg)
              verifiers
          )
      case r of
        CEGISSuccess model ->
          return (cex, SynthesisSuccess $ evalSymToCon model symProg)
        CEGISVerifierFailure () -> return (cex, SynthesisVerifierFailure)
        CEGISSolverFailure failure ->
          return (cex, SynthesisSolverFailure failure)
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
  taskRefinable _ = True

instance
  (Typeable symProg) =>
  RunSynthesisTask (SynthesisMinimalCostTask symProg conProg) symProg conProg
  where
  solverRunSynthesisTaskExtractCex
    solver
    ( SynthesisMinimalCostTask
        verifiers
        examples
        symProg
        precond
        (initialMaxCost :: Maybe cost)
        conCostObj
        symCostObj
      ) = do
      initialExampleConstraints <-
        traverse (synthesisConstraintFun symProg) examples
      let costConstraint = case initialMaxCost of
            Nothing -> con True
            Just maxCost -> symProgCostLessThanMaxCost maxCost
      (cex, r) <-
        solverGenericCEGISWithRefinement
          solver
          True
          (precond .&& symAnd initialExampleConstraints .&& costConstraint)
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
        CEGISSolverFailure failure ->
          return (cex, SynthesisSolverFailure failure)
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
  taskRefinable _ = False

instance
  (Typeable symProg) =>
  RunSynthesisTask (SynthesisTask symProg conProg) symProg conProg
  where
  solverRunSynthesisTaskExtractCex
    solver
    (SynthesisTask verifiers examples symProg precond) = do
      initialExampleConstraints <-
        traverse (synthesisConstraintFun symProg) examples
      (cex, r) <-
        solverGenericCEGIS
          solver
          True
          (precond .&& symAnd initialExampleConstraints)
          (synthesisConstraintFun symProg)
          ( concatMap
              (\(SomeVerifier verifier) -> toVerifierFuns verifier symProg)
              verifiers
          )
      case r of
        CEGISSuccess model ->
          return (cex, SynthesisSuccess $ evalSymToCon model symProg)
        CEGISVerifierFailure () -> return (cex, SynthesisVerifierFailure)
        CEGISSolverFailure failure ->
          return (cex, SynthesisSolverFailure failure)
  taskRefinable _ = True

solverRunSynthesisTask ::
  ( Solver solver,
    RunSynthesisTask task symProg conProg
  ) =>
  solver ->
  task ->
  IO (SynthesisResult conProg)
solverRunSynthesisTask solver task =
  snd <$> solverRunSynthesisTaskExtractCex solver task

runSynthesisTask ::
  ( ConfigurableSolver config h,
    RunSynthesisTask task symProg conProg
  ) =>
  config ->
  task ->
  IO (SynthesisResult conProg)
runSynthesisTask config task =
  snd <$> runSynthesisTaskExtractCex config task

runSynthesisTaskExtractCex ::
  forall config h task symProg conProg.
  ( ConfigurableSolver config h,
    RunSynthesisTask task symProg conProg
  ) =>
  config ->
  task ->
  IO ([Example symProg], SynthesisResult conProg)
runSynthesisTaskExtractCex config task =
  withSolver config $ \solver ->
    solverRunSynthesisTaskExtractCex solver task
