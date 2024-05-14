{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.TestOperator.TestCostOperator
  ( TestCostOperator (..),
  )
where

import Grisette (Mergeable, mrgReturn)
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Program.CostModel.PerStmtCostModel (OpCost (opCost))

newtype TestCostOperator = TestCostOperator Integer
  deriving (Show, Eq)

instance
  (Num cost, MonadContext ctx, Mergeable cost) =>
  OpCost TestCostOperator cost ctx
  where
  opCost (TestCostOperator x) = mrgReturn $ fromIntegral x
