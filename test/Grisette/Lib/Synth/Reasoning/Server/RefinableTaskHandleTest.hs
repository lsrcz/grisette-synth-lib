{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Lib.Synth.Reasoning.Server.RefinableTaskHandleTest
  ( refinableTaskHandleTest,
  )
where

import qualified Control.Exception as C
import Data.Typeable (Proxy (Proxy))
import GHC.Stack (HasCallStack)
import Grisette
  ( ITEOp (symIte),
    SEq ((.==)),
    Solver (solverSolve),
    SolvingFailure (Unsat),
    SymInteger,
    evaluateSymToCon,
    precise,
    z3,
  )
import Grisette.Lib.Synth.Context (ConcreteContext, SymbolicContext)
import Grisette.Lib.Synth.Program.CostModel.PerStmtCostModel
  ( PerStmtCostObj (PerStmtCostObj),
  )
import Grisette.Lib.Synth.Program.ProgCost (ProgCost (progCost))
import Grisette.Lib.Synth.Reasoning.Server.BaseTaskHandle
  ( cancel,
    enqueueTask,
    waitCatch,
  )
import Grisette.Lib.Synth.Reasoning.Server.BaseTaskHandleTestCommon
  ( baseTaskHandleTestCommon,
  )
import Grisette.Lib.Synth.Reasoning.Server.Exception (SynthesisTaskException (SynthesisTaskSolverDead))
import Grisette.Lib.Synth.Reasoning.Server.RefinableTaskHandle
  ( RefinableTaskHandle,
    checkRefinableSolverAlive,
    enqueueRefineAction,
    pollAtIndex,
    waitCatchAtIndex,
  )
import Grisette.Lib.Synth.Reasoning.Server.ThreadPool (newThreadPool)
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( SynthesisResult (SynthesisSolverFailure, SynthesisSuccess),
  )
import Grisette.Lib.Synth.Reasoning.Synthesis.ComponentSketchTest
  ( ConProg,
    task,
    times4Sketch,
  )
import Grisette.Lib.Synth.Reasoning.Synthesis.Problem (times4Gen, times4Spec)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@?=))

type Handle = RefinableTaskHandle ConProg

shouldHaveCost ::
  ( Show conProg,
    ProgCost PerStmtCostObj conProg SymInteger ConcreteContext
  ) =>
  Either C.SomeException (SynthesisResult conProg) ->
  SymInteger ->
  Assertion
shouldHaveCost (Right (SynthesisSuccess prog)) cost = do
  let Right finalCost =
        progCost PerStmtCostObj prog ::
          ConcreteContext SymInteger
  finalCost @?= cost
shouldHaveCost r _ = error $ "Unexpected result " <> show r

shouldUnsat ::
  (HasCallStack, Show conProg) =>
  Either C.SomeException (SynthesisResult conProg) ->
  Assertion
shouldUnsat (Right (SynthesisSolverFailure Unsat)) = return ()
shouldUnsat r = error $ "Unexpected result " <> show r

shouldSolverDead ::
  (HasCallStack, Show conProg) =>
  Either C.SomeException (SynthesisResult conProg) ->
  Assertion
shouldSolverDead (Left e) = case C.fromException e of
  Just SynthesisTaskSolverDead -> return ()
  _ -> error $ "Unexpected exception " <> show e
shouldSolverDead r = error $ "Unexpected result " <> show r

refinableTaskHandleTest :: Test
refinableTaskHandleTest =
  testGroup
    "RefinableTaskHandle"
    [ baseTaskHandleTestCommon "BaseTaskHandle" (Proxy @Handle),
      testCase "refine" $ do
        pool <- newThreadPool 2
        handle :: Handle <-
          enqueueTask pool (precise z3) $
            task times4Spec times4Gen times4Sketch
        r <- waitCatch handle
        case r of
          Right (SynthesisSuccess prog) -> do
            let Right cost =
                  progCost PerStmtCostObj prog ::
                    ConcreteContext SymInteger
            enqueueRefineAction handle $ \solver -> do
              let newCost =
                    progCost PerStmtCostObj times4Sketch ::
                      SymbolicContext SymInteger
              r <-
                solverSolve solver $
                  newCost .== return (symIte (cost .== 2) 3 2)
              case r of
                Left err -> return $ SynthesisSolverFailure err
                Right m ->
                  return $ SynthesisSuccess $ evaluateSymToCon m times4Sketch
            r <- waitCatch handle
            shouldHaveCost r $ symIte (cost .== 2) 3 2
            checkRefinableSolverAlive handle >>= (@?= True)
          _ -> fail "Unexpected result"
        cancel handle,
      testCase "refine all submitted at once" $ do
        pool <- newThreadPool 2
        handle :: Handle <-
          enqueueTask pool (precise z3) $
            task times4Spec times4Gen times4Sketch
        let newCost =
              progCost PerStmtCostObj times4Sketch ::
                SymbolicContext SymInteger
        let refineWithCost solver cost = do
              r <- solverSolve solver $ newCost .== return cost
              case r of
                Left err -> return $ SynthesisSolverFailure err
                Right m ->
                  return $ SynthesisSuccess $ evaluateSymToCon m times4Sketch
        enqueueRefineAction handle $ \solver -> refineWithCost solver 2
        enqueueRefineAction handle $ \solver -> refineWithCost solver 3
        enqueueRefineAction handle $ \solver -> refineWithCost solver 2

        r <- waitCatch handle
        shouldHaveCost r 2
        Just r1 <- pollAtIndex handle 1
        Just r2 <- pollAtIndex handle 2
        Just r3 <- pollAtIndex handle 3
        shouldHaveCost r1 2
        shouldUnsat r2
        shouldSolverDead r3
        r1 <- waitCatchAtIndex handle 1
        r2 <- waitCatchAtIndex handle 2
        r3 <- waitCatchAtIndex handle 3
        shouldHaveCost r1 2
        shouldUnsat r2
        shouldSolverDead r3
        checkRefinableSolverAlive handle >>= (@?= False)
    ]
