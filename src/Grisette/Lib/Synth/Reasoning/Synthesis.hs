{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
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
    SomeExample (..),
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

import Control.DeepSeq (NFData (rnf))
import Control.Monad.Except (runExceptT)
import qualified Data.Binary as Binary
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Data (Typeable)
import Data.Hashable (Hashable)
import Data.Proxy (Proxy)
import qualified Data.Serialize as Cereal
import GHC.Generics (Generic)
import Grisette
  ( CEGISResult (CEGISSolverFailure, CEGISSuccess, CEGISVerifierFailure),
    ConfigurableSolver,
    Default (Default),
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
import Grisette.Lib.Synth.Program.ProgConstraints (WithConstraints)
import Grisette.Lib.Synth.Program.ProgCost (ProgCost (progCost))
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
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

data Example symSemObj symConstObj symVal matcher where
  Example ::
    { exampleSymSemantics :: WithConstraints symSemObj symConstObj,
      exampleIOPair :: IOPair symVal,
      exampleMatcher :: matcher
    } ->
    Example symSemObj symConstObj symVal matcher
  deriving (Eq, Generic)
  deriving anyclass (Serial, NFData, Hashable)

instance
  (Serial symSemObj, Serial symConstObj, Serial symVal, Serial matcher) =>
  Cereal.Serialize (Example symSemObj symConstObj symVal matcher)
  where
  put = serialize
  get = deserialize

instance
  (Serial symSemObj, Serial symConstObj, Serial symVal, Serial matcher) =>
  Binary.Binary (Example symSemObj symConstObj symVal matcher)
  where
  put = serialize
  get = deserialize

instance
  (Show symVal) =>
  Show (Example symSemObj symConstObj symVal matcher)
  where
  show (Example _ iop _) = show iop

instance
  (PPrint symVal) =>
  PPrint (Example symSemObj symConstObj symVal matcher)
  where
  pformat (Example _ iop _) = pformat iop

data SomeExample symProg where
  SomeExample ::
    forall symSemObj symConstObj symProg symVal ctx matcher.
    ( ProgSemantics (WithConstraints symSemObj symConstObj) symProg symVal ctx,
      SynthesisContext ctx,
      Matcher matcher SymBool symVal,
      Mergeable symVal,
      Typeable symSemObj,
      Typeable symConstObj,
      Typeable symVal,
      Typeable matcher,
      Show symVal,
      PPrint symVal,
      NFData symSemObj,
      NFData symConstObj,
      NFData symVal,
      NFData matcher
    ) =>
    Proxy ctx ->
    Example symSemObj symConstObj symVal matcher ->
    SomeExample symProg

instance Show (SomeExample symProg) where
  show (SomeExample _ ex) = show ex

instance PPrint (SomeExample symProg) where
  pformat (SomeExample _ ex) = pformat ex

instance NFData (SomeExample symProg) where
  rnf (SomeExample p ex) = rnf p `seq` rnf ex

class
  IsVerifier verifier symProg conProg
    | verifier -> symProg conProg
  where
  toVerifierFuns ::
    verifier -> symProg -> [VerifierFun (SomeExample symProg) ()]

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
      synthesisInitialExamples :: [SomeExample symProg],
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
      synthesisInitialExamples :: [SomeExample symProg],
      synthesisSketch :: symProg,
      synthesisPrecondition :: SymBool,
      synthesisInitialMaxCost :: Maybe cost,
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
      synthesisInitialExamples :: [SomeExample symProg],
      synthesisSketch :: symProg,
      synthesisPrecondition :: SymBool,
      synthesisInitialMaxCost :: Maybe cost,
      synthesisConCostObj :: conCostObj,
      synthesisSymCostObj :: symCostObj
    } ->
    SynthesisMinimalCostTask symProg conProg

synthesisConstraintFun ::
  forall symProg.
  (Typeable symProg) =>
  symProg ->
  SynthesisConstraintFun (SomeExample symProg)
synthesisConstraintFun
  prog
  (SomeExample (_ :: Proxy ctx) (Example symSem (iop :: IOPair symVal) matcher)) =
    genSynthesisConstraint
      matcher
      (runProg symSem prog (ioPairInputs iop) :: ctx [symVal])
      (ioPairOutputs iop)

data SynthesisResult conProg
  = SynthesisSuccess conProg
  | SynthesisVerifierFailure
  | SynthesisSolverFailure SolvingFailure
  deriving (Show, Generic)
  deriving (Serial, NFData)
  deriving (PPrint) via (Default (SynthesisResult conProg))

instance (Serial conProg) => Cereal.Serialize (SynthesisResult conProg) where
  put = serialize
  get = deserialize

instance (Serial conProg) => Binary.Binary (SynthesisResult conProg) where
  put = serialize
  get = deserialize

class RunSynthesisTask task symProg conProg | task -> symProg conProg where
  solverRunSynthesisTaskExtractCex ::
    forall solver.
    (Solver solver) =>
    solver ->
    task ->
    IO ([SomeExample symProg], SynthesisResult conProg)
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
        (initialMaxCost :: Maybe cost)
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
      let allCexes = examples ++ cex
      case r of
        CEGISSuccess model ->
          return (allCexes, SynthesisSuccess $ evalSymToCon model symProg)
        CEGISVerifierFailure () ->
          return (allCexes, SynthesisVerifierFailure)
        CEGISSolverFailure failure ->
          return (allCexes, SynthesisSolverFailure failure)
      where
        symProgCost =
          flip runFreshT "cost" $ progCost symCostObj symProg ::
            SymbolicContext cost
        symProgCostLessThanMaxCost :: Maybe cost -> SymBool
        symProgCostLessThanMaxCost Nothing = con True
        symProgCostLessThanMaxCost (Just maxCost) = simpleMerge $ do
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
      let allCexes = examples ++ cex
      case r of
        CEGISSuccess model ->
          return (allCexes, SynthesisSuccess $ evalSymToCon model symProg)
        CEGISVerifierFailure () -> return (allCexes, SynthesisVerifierFailure)
        CEGISSolverFailure failure ->
          return (allCexes, SynthesisSolverFailure failure)
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
      let allCexes = examples ++ cex
      case r of
        CEGISSuccess model ->
          return (allCexes, SynthesisSuccess $ evalSymToCon model symProg)
        CEGISVerifierFailure () -> return (allCexes, SynthesisVerifierFailure)
        CEGISSolverFailure failure ->
          return (allCexes, SynthesisSolverFailure failure)
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
  IO ([SomeExample symProg], SynthesisResult conProg)
runSynthesisTaskExtractCex config task =
  withSolver config $ \solver ->
    solverRunSynthesisTaskExtractCex solver task
