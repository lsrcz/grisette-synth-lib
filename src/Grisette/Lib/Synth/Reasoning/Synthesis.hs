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
    SynthesisTask (..),
    runSynthesisTask,
    IsVerifier (..),
    SomeVerifier (..),
    VerificationCex (..),
    runSynthesisTaskExtractCex,
  )
where

import Control.Monad.Except (runExceptT)
import Data.Data (Typeable, eqT, type (:~:) (Refl))
import Data.Proxy (Proxy)
import qualified Data.Text as T
import Grisette
  ( CEGISResult (CEGISSolverFailure, CEGISSuccess, CEGISVerifierFailure),
    ConfigurableSolver,
    EvaluateSym,
    Mergeable,
    SOrd ((.<)),
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
import Grisette.Lib.Synth.Context (AngelicContext, ConcreteContext, SymbolicContext)
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
    forall symProg conProg cost conCostObj symCostObj.
    ( EvaluateSym symProg,
      ToCon symProg conProg,
      Typeable symProg,
      ProgCost conCostObj conProg cost ConcreteContext,
      ProgCost symCostObj symProg cost AngelicContext,
      SOrd cost
    ) =>
    { synthesisTaskVerifiers :: [SomeVerifier symProg conProg],
      synthesisTaskSymProg :: symProg,
      synthesisTaskInitialMaxCost :: Maybe cost,
      synthesisTaskConCostObj :: conCostObj,
      synthesisTaskSymCostObj :: symCostObj
    } ->
    SynthesisTask conProg

synthesisConstraintFun ::
  forall symProg.
  (Typeable symProg) =>
  symProg ->
  SynthesisConstraintFun VerificationCex
synthesisConstraintFun
  prog
  i
  ( VerificationCex
      (_ :: Proxy ctx)
      (_ :: Proxy symProg')
      symSem
      (iop :: IOPair symVal)
      matcher
    ) =
    case eqT @symProg @symProg' of
      Just Refl ->
        return $
          genSynthesisConstraint
            i
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
runSynthesisTaskExtractCex
  config
  ( SynthesisTask
      verifiers
      symProg
      (initialMaxCost :: Maybe cost)
      conCostObj
      symCostObj
    ) = do
    (cex, r) <-
      genericCEGIS
        config
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
