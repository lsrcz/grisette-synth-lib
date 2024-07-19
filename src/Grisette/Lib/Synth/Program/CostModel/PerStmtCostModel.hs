{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.Program.CostModel.PerStmtCostModel
  ( PerStmtCostObj (..),
    OpCost (..),
  )
where

import Grisette (Mergeable, MonadUnion, Union, liftUnion)
import Grisette.Lib.Synth.Context (MonadContext)

newtype PerStmtCostObj opCostObj = PerStmtCostObj opCostObj

class (MonadContext ctx) => OpCost opCostObj op cost ctx where
  opCost :: opCostObj -> op -> ctx cost

instance
  (MonadUnion ctx, OpCost opCostObj op cost ctx, Mergeable op) =>
  OpCost opCostObj (Union op) cost ctx
  where
  opCost obj op = liftUnion op >>= opCost obj
