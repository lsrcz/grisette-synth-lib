{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (..)) where

import Grisette (Mergeable, MonadUnion, UnionM, liftUnionM, tryMerge)
import Grisette.Lib.Synth.Context (MonadContext)

class (MonadContext ctx) => OpSemantics semObj op val ctx where
  applyOp :: semObj -> op -> [val] -> ctx [val]

instance
  ( MonadUnion ctx,
    OpSemantics semObj op val ctx,
    Mergeable op,
    Mergeable val
  ) =>
  OpSemantics semObj (UnionM op) val ctx
  where
  applyOp semObj op args = tryMerge $ do
    op' <- liftUnionM op
    applyOp semObj op' args
