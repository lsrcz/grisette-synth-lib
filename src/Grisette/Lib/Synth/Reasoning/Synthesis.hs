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

import Control.Monad.Except (runExceptT)
import Data.Data (Proxy (Proxy))
import qualified Data.Text as T
import Grisette
  ( CEGISResult (CEGISSolverFailure, CEGISSuccess, CEGISVerifierFailure),
    ConfigurableSolver,
    EvaluateSym,
    Mergeable,
    Solvable (con),
    SolvingFailure,
    StatefulVerifierFun,
    SymBool,
    SynthesisConstraintFun,
    ToCon,
    ToSym (toSym),
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
import Grisette.Lib.Synth.Reasoning.IOPair (IOPair (IOPair))
import Grisette.Lib.Synth.Reasoning.Matcher (Matcher (match))
import Grisette.Lib.Synth.Util.Show (showText)

class SynthesisContext ctx where
  genSynthesisConstraint ::
    (Matcher matcher SymBool val, Show val, Mergeable val) =>
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

synthesisConstraintFun ::
  forall semObj constObj symProg conVal symVal ctx matcher p q.
  ( ProgSemantics semObj symProg symVal ctx,
    ProgConstraints constObj symProg ctx,
    SynthesisContext ctx,
    Matcher matcher SymBool symVal,
    ToSym conVal symVal,
    Show symVal,
    Mergeable symVal
  ) =>
  p ctx ->
  q symVal ->
  WithConstraints semObj constObj ->
  symProg ->
  SynthesisConstraintFun (IOPair conVal, matcher)
synthesisConstraintFun
  _
  _
  sem
  prog
  i
  (IOPair inputs expectedOutputs, matcher) =
    return $
      genSynthesisConstraint
        i
        matcher
        (runProgWithConstraints sem prog (toSym inputs) :: ctx [symVal])
        (toSym expectedOutputs)

data SynthesisResult conProg exception
  = SynthesisSuccess conProg
  | SynthesisVerifierFailure exception
  | SynthesisSolverFailure SolvingFailure
  deriving (Show)

data SynthesisTask conVal conProg matcher exception where
  SynthesisTask ::
    forall
      config
      h
      conVal
      symVal
      state
      exception
      ctx
      conProg
      symProg
      matcher
      conSemObj
      symSemObj
      conConstObj
      symConstObj.
    ( ConfigurableSolver config h,
      ProgSemantics symSemObj symProg symVal ctx,
      ProgConstraints symConstObj symProg ctx,
      SynthesisContext ctx,
      Matcher matcher SymBool symVal,
      ToSym conVal symVal,
      EvaluateSym symProg,
      ToCon symProg conProg,
      Show symVal,
      Mergeable symVal
    ) =>
    { synthesisTaskContextType :: Proxy ctx,
      synthesisTaskSymValType :: Proxy symVal,
      synthesisTaskSolverConfig :: config,
      synthesisTaskInitialVerifierState :: state,
      synthesisTaskVerifier ::
        forall p.
        p conProg ->
        WithConstraints conSemObj conConstObj ->
        symProg ->
        StatefulVerifierFun state (IOPair conVal, matcher) exception,
      synthesisTaskConSemantics :: WithConstraints conSemObj conConstObj,
      synthesisTaskSymSemantics :: WithConstraints symSemObj symConstObj,
      synthesisTaskSymProg :: symProg
    } ->
    SynthesisTask conVal conProg matcher exception

class ToSynthesisTask task where
  type ConValType task
  type ConProgType task
  type MatcherType task
  type ExceptionType task
  toSynthesisTask ::
    task ->
    SynthesisTask
      (ConValType task)
      (ConProgType task)
      (MatcherType task)
      (ExceptionType task)

instance ToSynthesisTask (SynthesisTask conVal conProg matcher exception) where
  type ConValType (SynthesisTask conVal conProg matcher exception) = conVal
  type ConProgType (SynthesisTask conVal conProg matcher exception) = conProg
  type MatcherType (SynthesisTask conVal conProg matcher exception) = matcher
  type ExceptionType (SynthesisTask conVal conProg matcher exception) = exception
  toSynthesisTask = id

synthesizeProgWithVerifier ::
  forall task conVal conProg matcher exception.
  ( ToSynthesisTask task,
    matcher ~ MatcherType task,
    conVal ~ ConValType task,
    conProg ~ ConProgType task,
    exception ~ ExceptionType task
  ) =>
  task ->
  IO ([(IOPair conVal, matcher)], SynthesisResult conProg exception)
synthesizeProgWithVerifier task =
  case toSynthesisTask task of
    SynthesisTask
      pctx
      psymVal
      config
      initialState
      verifier
      conSem
      symSem
      prog -> do
        (ioPairs, r) <-
          genericCEGIS
            config
            (con True)
            ( synthesisConstraintFun
                pctx
                psymVal
                symSem
                prog
            )
            initialState
            (verifier (Proxy :: Proxy conProg) conSem prog)
        case r of
          CEGISSuccess model ->
            return (ioPairs, SynthesisSuccess $ evaluateSymToCon model prog)
          CEGISVerifierFailure ex -> return (ioPairs, SynthesisVerifierFailure ex)
          CEGISSolverFailure failure ->
            return (ioPairs, SynthesisSolverFailure failure)
