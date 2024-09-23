{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.ComponentSketch.ProgCostTest (progCostTest) where

import Grisette (ITEOp (symIte), SymInteger)
import Grisette.Lib.Synth.Context (SymbolicContext)
import qualified Grisette.Lib.Synth.Program.ComponentSketch as Component
import Grisette.Lib.Synth.Program.CostModel.PerStmtCostModel
  ( PerStmtCostObj (PerStmtCostObj),
  )
import Grisette.Lib.Synth.Program.ProgCost (ProgCost (progCost))
import Grisette.Lib.Synth.TestOperator.TestCostOperator
  ( TestCost (TestCost),
    TestCostOperator (TestCostOperator),
  )
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.SymbolicAssertion ((.@?=))

progCostTest :: Test
progCostTest = testCase "ProgCost" $ do
  let prog =
        Component.Prog
          "test"
          []
          [ Component.Stmt (TestCostOperator 10) [] 0 [] 0 "x" [],
            Component.Stmt (TestCostOperator 20) [] 0 [] 0 "y" []
          ]
          []
  let cost =
        progCost (PerStmtCostObj TestCost) mempty prog ::
          SymbolicContext SymInteger
  cost .@?= return (symIte "x" 0 10 + symIte "y" 0 20)
