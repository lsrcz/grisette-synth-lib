{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.TestOperator.TestCostOperator
  ( TestCostOperator (..),
    TestCost (..),
  )
where

import Grisette (Mergeable, mrgReturn)
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Program.CostModel.PerStmtCostModel (OpCost (opCost))

data TestCost = TestCost
  deriving (Show, Eq)

newtype TestCostOperator = TestCostOperator Integer
  deriving (Show, Eq)

instance
  (Num cost, MonadContext ctx, Mergeable cost) =>
  OpCost TestCost TestCostOperator cost ctx
  where
  opCost _ _ (TestCostOperator x) = mrgReturn $ fromIntegral x
