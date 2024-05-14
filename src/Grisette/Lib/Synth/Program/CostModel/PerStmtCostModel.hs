{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.Program.CostModel.PerStmtCostModel
  ( PerStmtCostObj (..),
    OpCost (..),
  )
where

import Grisette (Mergeable, MonadUnion, UnionM, liftUnionM)
import Grisette.Lib.Synth.Context (MonadContext)

data PerStmtCostObj = PerStmtCostObj

class (MonadContext ctx) => OpCost op cost ctx where
  opCost :: op -> ctx cost

instance
  (MonadUnion ctx, OpCost op cost ctx, Mergeable op) =>
  OpCost (UnionM op) cost ctx
  where
  opCost op = liftUnionM op >>= opCost
