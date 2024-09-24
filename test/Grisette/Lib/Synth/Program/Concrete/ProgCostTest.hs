{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.Concrete.ProgCostTest (progCostTest) where

import Grisette.Lib.Synth.Context (ConcreteContext)
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
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
import Test.HUnit ((@?=))

progCostTest :: Test
progCostTest = testCase "ProgCost" $ do
  let prog =
        Concrete.Prog
          []
          [ Concrete.Stmt (TestCostOperator 10) [] [],
            Concrete.Stmt (TestCostOperator 20) [] []
          ]
          []
  let cost =
        progCost (PerStmtCostObj TestCost) mempty prog ::
          ConcreteContext Integer
  cost @?= Right 30
