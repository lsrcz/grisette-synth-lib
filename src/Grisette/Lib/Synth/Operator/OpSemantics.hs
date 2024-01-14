{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (..)) where

import Grisette.Lib.Synth.Context (MonadContext)

class (MonadContext ctx) => OpSemantics semObj op val ctx where
  applyOp :: semObj -> op -> [val] -> ctx [val]
