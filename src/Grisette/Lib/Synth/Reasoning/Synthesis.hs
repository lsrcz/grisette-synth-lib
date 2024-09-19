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
import qualified Data.Serialize as Cereal
import Data.Typeable (Proxy (Proxy), cast)
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
    ToSym (toSym),
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

data Example symSemObj symVal conSemObj conVal matcher where
  Example ::
    { exampleConSemantics :: conSemObj,
      exampleSymSemantics :: symSemObj,
      exampleSymValType :: Proxy symVal,
      exampleIOPair :: IOPair conVal,
      exampleMatcher :: matcher
    } ->
    Example symSemObj symVal conSemObj conVal matcher
  deriving (Eq, Generic)
  deriving anyclass (NFData, Hashable)

instance
  (Serial conSemObj, Serial symSemObj, Serial conVal, Serial matcher) =>
  Serial (Example symSemObj symVal conSemObj conVal matcher)
  where
  serialize (Example conSem symSem _ iop matcher) = do
    serialize conSem
    serialize symSem
    serialize iop
    serialize matcher
  deserialize = do
    conSem <- deserialize
    symSem <- deserialize
    iop <- deserialize
    Example conSem symSem Proxy iop <$> deserialize

instance
  (Serial conSemObj, Serial symSemObj, Serial conVal, Serial matcher) =>
  Cereal.Serialize (Example symSemObj symVal conSemObj conVal matcher)
  where
  put = serialize
  get = deserialize

instance
  (Serial conSemObj, Serial symSemObj, Serial conVal, Serial matcher) =>
  Binary.Binary (Example symSemObj symVal conSemObj conVal matcher)
  where
  put = serialize
  get = deserialize

instance
  (Show conVal) =>
  Show (Example symSemObj symVal conSemObj conVal matcher)
  where
  show (Example _ _ _ iop _) = show iop

instance
  (PPrint conVal) =>
  PPrint (Example symSemObj symVal conSemObj conVal matcher)
  where
  pformat (Example _ _ _ iop _) = pformat iop

data SomeExample symProg conProg where
  SomeExample ::
    forall symSemObj symProg symVal conSemObj conProg conVal matcher.
    ( ProgSemantics symSemObj symProg symVal AngelicContext,
      ProgSemantics conSemObj conProg conVal ConcreteContext,
      Matcher matcher SymBool symVal,
      Matcher matcher Bool conVal,
      Mergeable symVal,
      Eq conSemObj,
      Eq symSemObj,
      Eq conVal,
      Eq matcher,
      Typeable conSemObj,
      Typeable symSemObj,
      Typeable conVal,
      Typeable symVal,
      Typeable matcher,
      Show conVal,
      PPrint conVal,
      NFData conSemObj,
      NFData symSemObj,
      NFData conVal,
      NFData matcher,
      ToSym conVal symVal
    ) =>
    Example symSemObj symVal conSemObj conVal matcher ->
    SomeExample symProg conProg

instance Show (SomeExample symProg conProg) where
  show (SomeExample ex) = show ex

instance PPrint (SomeExample symProg conProg) where
  pformat (SomeExample ex) = pformat ex

eqHetero :: (Typeable a, Typeable b, Eq a) => a -> b -> Bool
eqHetero a b = case cast b of
  Just b' -> a == b'
  Nothing -> False

instance Eq (SomeExample symProg conProg) where
  (SomeExample ex1) == (SomeExample ex2) =
    eqHetero ex1 ex2

instance NFData (SomeExample symProg conProg) where
  rnf (SomeExample ex) = rnf ex

class
  IsVerifier verifier symProg conProg
    | verifier -> symProg conProg
  where
  toVerifierFuns ::
    verifier -> symProg -> [VerifierFun (SomeExample symProg conProg) ()]

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
      synthesisInitialExamples :: [SomeExample symProg conProg],
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
      synthesisInitialExamples :: [SomeExample symProg conProg],
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
      synthesisInitialExamples :: [SomeExample symProg conProg],
      synthesisSketch :: symProg,
      synthesisPrecondition :: SymBool,
      synthesisInitialMaxCost :: Maybe cost,
      synthesisConCostObj :: conCostObj,
      synthesisSymCostObj :: symCostObj
    } ->
    SynthesisMinimalCostTask symProg conProg

synthesisConstraintFun ::
  forall symProg conProg.
  (Typeable symProg) =>
  symProg ->
  SynthesisConstraintFun (SomeExample symProg conProg)
synthesisConstraintFun
  prog
  ( SomeExample
      ( Example _ symSem _ (iop :: IOPair conVal) matcher ::
          Example symSemObj symVal conSemObj conVal matcher
        )
    ) =
    genSynthesisConstraint
      matcher
      ( runProg symSem prog (toSym $ ioPairInputs iop :: [symVal]) ::
          AngelicContext [symVal]
      )
      (toSym $ ioPairOutputs iop :: [symVal])

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
    IO ([SomeExample symProg conProg], SynthesisResult conProg)
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
  ( ConfigurableSolver config h,
    RunSynthesisTask task symProg conProg
  ) =>
  config ->
  task ->
  IO ([SomeExample symProg conProg], SynthesisResult conProg)
runSynthesisTaskExtractCex config task =
  withSolver config $ \solver ->
    solverRunSynthesisTaskExtractCex solver task
