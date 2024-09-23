{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Lib.Synth.Reasoning.Parallel.RefinableTaskHandleTest
  ( refinableTaskHandleTest,
  )
where

import qualified Control.Exception as C
import qualified Data.Text as T
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
import Grisette.Lib.Synth.Program.ProgCost (ProgCost, symbolCost)
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
    times4SketchTable,
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
  Either C.SomeException ([SomeExample symProg conProg], SynthesisResult conProg) ->
  T.Text ->
  SymInteger ->
  Assertion
shouldHaveCost (Right (_, SynthesisSuccess result)) symbol cost = do
  let Right finalCost =
        symbolCost (PerStmtCostObj TestSemanticsCost) result symbol ::
          ConcreteContext SymInteger
  finalCost @?= cost
shouldHaveCost r _ _ = error $ "Unexpected result " <> show (snd <$> r)

shouldUnsat ::
  (HasCallStack, Show conProg) =>
  Either C.SomeException ([SomeExample symProg conProg], SynthesisResult conProg) ->
  Assertion
shouldUnsat (Right (_, SynthesisSolverFailure Unsat)) = return ()
shouldUnsat r = error $ "Unexpected result " <> show (snd <$> r)

shouldSolverDead ::
  (HasCallStack, Show conProg) =>
  Either C.SomeException ([SomeExample symProg conProg], SynthesisResult conProg) ->
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
              task times4Spec times4Gen [] times4SketchTable "test" (con True)
        r <- waitCatch handle
        case r of
          Right (_, SynthesisSuccess result) -> do
            let Right cost =
                  symbolCost (PerStmtCostObj TestSemanticsCost) result "test" ::
                    ConcreteContext SymInteger
            enqueueRefineCond 0 handle $ do
              let newCost =
                    symbolCost
                      (PerStmtCostObj TestSemanticsCost)
                      times4SketchTable
                      "test" ::
                      SymbolicContext SymInteger
              return $ newCost .== return (symIte (cost .== 2) 3 2)
            r <- waitCatch handle
            shouldHaveCost r "test" $ symIte (cost .== 2) 3 2
            checkRefinableSolverAlive handle >>= (@?= True)
          _ -> fail "Unexpected result"
        cancel handle,
      testCase "refine all submitted at once" $ do
        pool <- newThreadPool 2
        handle :: Handle <-
          enqueueTask pool z3 0 $
            return $
              task times4Spec times4Gen [] times4SketchTable "test" (con True)
        let newCost =
              symbolCost (PerStmtCostObj TestSemanticsCost) times4SketchTable "test" ::
                SymbolicContext SymInteger
        enqueueRefineCond 0 handle $ return $ newCost .== return 2
        enqueueRefineCond 0 handle $ return $ newCost .== return 3
        enqueueRefineCond 0 handle $ return $ newCost .== return 2

        r <- waitCatch handle
        shouldHaveCost r "test" 2
        Just r1 <- pollAtIndex handle 1
        Just r2 <- pollAtIndex handle 2
        Just r3 <- pollAtIndex handle 3
        shouldHaveCost r1 "test" 2
        shouldUnsat r2
        shouldSolverDead r3
        r1 <- waitCatchAtIndex handle 1
        r2 <- waitCatchAtIndex handle 2
        r3 <- waitCatchAtIndex handle 3
        shouldHaveCost r1 "test" 2
        shouldUnsat r2
        shouldSolverDead r3
        checkRefinableSolverAlive handle >>= (@?= False)
    ]
