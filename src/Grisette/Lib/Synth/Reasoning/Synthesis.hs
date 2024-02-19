{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Lib.Synth.Reasoning.Synthesis
  ( SynthesisContext (..),
    SynthesisResult (..),
    SynthesisTask (..),
    synthesizeProgWithVerifier,
    ToSynthesisTask (..),
  )
where

import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import Grisette
  ( CEGISResult (CEGISSolverFailure, CEGISSuccess, CEGISVerifierFailure),
    ConfigurableSolver,
    EvaluateSym,
    FreshIdent (FreshIdentWithInfo),
    Mergeable,
    SEq ((.==)),
    Solvable (con),
    SolvingFailure,
    StatefulVerifierFun,
    SymBool,
    SynthesisConstraintFun,
    ToCon,
    ToSym (toSym),
    evaluateSymToCon,
    genericCEGIS,
    runFreshT,
  )
import Grisette.Lib.Synth.Context (AngelicContext, SymbolicContext)
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Reasoning.IOPair (IOPair (IOPair))

class SynthesisContext ctx where
  genSynthesisConstraint ::
    (SEq val, Show val, Mergeable val) => Int -> ctx [val] -> [val] -> SymBool

instance SynthesisContext SymbolicContext where
  genSynthesisConstraint _ actual expected = actual .== return expected

instance SynthesisContext AngelicContext where
  genSynthesisConstraint i actual expected =
    runFreshT actual (FreshIdentWithInfo "::synth::" i) .== return expected

synthesisConstraintFun ::
  forall semObj symProg conVal symVal ctx p q.
  ( ProgSemantics semObj symProg symVal ctx,
    SynthesisContext ctx,
    SEq symVal,
    ToSym conVal symVal,
    Show symVal,
    Mergeable symVal
  ) =>
  p ctx ->
  q symVal ->
  semObj ->
  symProg ->
  SynthesisConstraintFun (IOPair conVal)
synthesisConstraintFun _ _ sem prog i (IOPair inputs expectedOutputs) =
  return $
    genSynthesisConstraint
      i
      (runProg sem prog (toSym inputs) :: ctx [symVal])
      (toSym expectedOutputs)

data SynthesisResult conProg exception
  = SynthesisSuccess conProg
  | SynthesisVerifierFailure exception
  | SynthesisSolverFailure SolvingFailure
  deriving (Show)

data SynthesisTask conVal symVal conProg symProg ctx exception where
  SynthesisTask ::
    forall config h conVal symVal state exception ctx conProg symProg semObj.
    ( ConfigurableSolver config h,
      ProgSemantics semObj symProg symVal ctx,
      SynthesisContext ctx,
      SEq symVal,
      ToSym conVal symVal,
      EvaluateSym symProg,
      ToCon symProg conProg,
      Show symVal
    ) =>
    { synthesisTaskSolverConfig :: config,
      synthesisTaskInitialVerifierState :: state,
      synthesisTaskVerifier ::
        forall p.
        p conProg ->
        semObj ->
        symProg ->
        StatefulVerifierFun state (IOPair conVal) exception,
      synthesisTaskSemantics :: semObj,
      synthesisTaskSymProg :: symProg
    } ->
    SynthesisTask conVal symVal conProg symProg ctx exception

class ToSynthesisTask task where
  type ConValType task
  type SymValType task
  type ConProgType task
  type SymProgType task
  type ContextType task :: Type -> Type
  type ExceptionType task
  toSynthesisTask ::
    task ->
    SynthesisTask
      (ConValType task)
      (SymValType task)
      (ConProgType task)
      (SymProgType task)
      (ContextType task)
      (ExceptionType task)

instance
  ToSynthesisTask
    (SynthesisTask conVal symVal conProg symProg ctx exception)
  where
  type
    ConValType (SynthesisTask conVal symVal conProg symProg ctx exception) =
      conVal
  type
    SymValType (SynthesisTask conVal symVal conProg symProg ctx exception) =
      symVal
  type
    ConProgType (SynthesisTask conVal symVal conProg symProg ctx exception) =
      conProg
  type
    SymProgType (SynthesisTask conVal symVal conProg symProg ctx exception) =
      symProg
  type
    ContextType (SynthesisTask conVal symVal conProg symProg ctx exception) =
      ctx
  type
    ExceptionType (SynthesisTask conVal symVal conProg symProg ctx exception) =
      exception
  toSynthesisTask = id

synthesizeProgWithVerifier ::
  forall task conVal conProg exception.
  ( ToSynthesisTask task,
    conVal ~ ConValType task,
    conProg ~ ConProgType task,
    exception ~ ExceptionType task,
    Mergeable (SymValType task)
  ) =>
  task ->
  IO ([IOPair conVal], SynthesisResult conProg exception)
synthesizeProgWithVerifier task =
  case toSynthesisTask task of
    SynthesisTask config initialState verifier sem prog -> do
      (ioPairs, r) <-
        genericCEGIS
          config
          (con True)
          ( synthesisConstraintFun
              (Proxy :: Proxy (ContextType task))
              (Proxy :: Proxy (SymValType task))
              sem
              prog
          )
          initialState
          (verifier (Proxy :: Proxy conProg) sem prog)
      case r of
        CEGISSuccess model ->
          return (ioPairs, SynthesisSuccess $ evaluateSymToCon model prog)
        CEGISVerifierFailure ex -> return (ioPairs, SynthesisVerifierFailure ex)
        CEGISSolverFailure failure ->
          return (ioPairs, SynthesisSolverFailure failure)
