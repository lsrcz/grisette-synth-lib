{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.Program.CostModel.PerStmtCostModel
  ( PerStmtCostObj (..),
    OpCost (..),
  )
where

import Grisette.Lib.Synth.Context (MonadContext)

data PerStmtCostObj = PerStmtCostObj

class (MonadContext ctx) => OpCost op cost ctx where
  opCost :: op -> ctx cost
