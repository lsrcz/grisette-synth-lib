{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Lib.Synth.Reasoning.Parallel.RefinableTaskHandleTest
  ( refinableTaskHandleTest,
  )
where

import qualified Control.Exception as C
import Data.Typeable (Proxy (Proxy))
import GHC.Stack (HasCallStack)
import Grisette
  ( ITEOp (symIte),
    Solvable (con),
    SolvingFailure (Unsat),
    SymEq ((.==)),
    SymInteger,
    z3,
  )
import Grisette.Lib.Synth.Context (ConcreteContext, SymbolicContext)
import Grisette.Lib.Synth.Program.CostModel.PerStmtCostModel
  ( PerStmtCostObj (PerStmtCostObj),
  )
import Grisette.Lib.Synth.Program.ProgCost (ProgCost (progCost))
import Grisette.Lib.Synth.Reasoning.Parallel.BaseTaskHandle
  ( cancel,
    enqueueTask,
    waitCatch,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.BaseTaskHandleTestCommon
  ( baseTaskHandleTestCommon,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.Exception
  ( SynthesisTaskException (SynthesisTaskSolverDead),
  )
import Grisette.Lib.Synth.Reasoning.Parallel.RefinableTaskHandle
  ( RefinableTaskHandle,
    checkRefinableSolverAlive,
    enqueueRefineCond,
    pollAtIndex,
    waitCatchAtIndex,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.ThreadPool (newThreadPool)
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( SomeExample,
    SynthesisResult (SynthesisSolverFailure, SynthesisSuccess),
  )
import Grisette.Lib.Synth.Reasoning.Synthesis.ComponentSketchTest
  ( ConProg,
    SymProg,
    task,
    times4Sketch,
  )
import Grisette.Lib.Synth.Reasoning.Synthesis.Problem (times4Gen, times4Spec)
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsCost (TestSemanticsCost),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@?=))

type Handle = RefinableTaskHandle SymProg ConProg

shouldHaveCost ::
  ( Show conProg,
    ProgCost
      (PerStmtCostObj TestSemanticsCost)
      conProg
      SymInteger
      ConcreteContext
  ) =>
  Either C.SomeException ([SomeExample symProg], SynthesisResult conProg) ->
  SymInteger ->
  Assertion
shouldHaveCost (Right (_, SynthesisSuccess prog)) cost = do
  let Right finalCost =
        progCost (PerStmtCostObj TestSemanticsCost) prog ::
          ConcreteContext SymInteger
  finalCost @?= cost
shouldHaveCost r _ = error $ "Unexpected result " <> show (snd <$> r)

shouldUnsat ::
  (HasCallStack, Show conProg) =>
  Either C.SomeException ([SomeExample symProg], SynthesisResult conProg) ->
  Assertion
shouldUnsat (Right (_, SynthesisSolverFailure Unsat)) = return ()
shouldUnsat r = error $ "Unexpected result " <> show (snd <$> r)

shouldSolverDead ::
  (HasCallStack, Show conProg) =>
  Either C.SomeException ([SomeExample symProg], SynthesisResult conProg) ->
  Assertion
shouldSolverDead (Left e) = case C.fromException e of
  Just SynthesisTaskSolverDead -> return ()
  _ -> error $ "Unexpected exception " <> show e
shouldSolverDead r = error $ "Unexpected result " <> show (snd <$> r)

refinableTaskHandleTest :: Test
refinableTaskHandleTest =
  testGroup
    "RefinableTaskHandle"
    [ baseTaskHandleTestCommon "BaseTaskHandle" (Proxy @Handle),
      testCase "refine" $ do
        pool <- newThreadPool 2
        handle :: Handle <-
          enqueueTask pool z3 0 $
            return $
              task times4Spec times4Gen [] times4Sketch (con True)
        r <- waitCatch handle
        case r of
          Right (_, SynthesisSuccess prog) -> do
            let Right cost =
                  progCost (PerStmtCostObj TestSemanticsCost) prog ::
                    ConcreteContext SymInteger
            enqueueRefineCond 0 handle $ do
              let newCost =
                    progCost (PerStmtCostObj TestSemanticsCost) times4Sketch ::
                      SymbolicContext SymInteger
              return $ newCost .== return (symIte (cost .== 2) 3 2)
            r <- waitCatch handle
            shouldHaveCost r $ symIte (cost .== 2) 3 2
            checkRefinableSolverAlive handle >>= (@?= True)
          _ -> fail "Unexpected result"
        cancel handle,
      testCase "refine all submitted at once" $ do
        pool <- newThreadPool 2
        handle :: Handle <-
          enqueueTask pool z3 0 $
            return $
              task times4Spec times4Gen [] times4Sketch (con True)
        let newCost =
              progCost (PerStmtCostObj TestSemanticsCost) times4Sketch ::
                SymbolicContext SymInteger
        enqueueRefineCond 0 handle $ return $ newCost .== return 2
        enqueueRefineCond 0 handle $ return $ newCost .== return 3
        enqueueRefineCond 0 handle $ return $ newCost .== return 2

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
