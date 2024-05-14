{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.Program.CostModel.NoCostModel (NoCostObj (..)) where

import Grisette (Mergeable, mrgReturn)
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Program.ProgCost (ProgCost (progCost))

data NoCostObj = NoCostObj

instance
  (MonadContext ctx, Mergeable cost, Num cost) =>
  ProgCost NoCostObj prog cost ctx
  where
  progCost _ _ = mrgReturn 0
