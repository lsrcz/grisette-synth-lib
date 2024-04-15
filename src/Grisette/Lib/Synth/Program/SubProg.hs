{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Program.SubProg (HasSubProg (..)) where

import Grisette (Mergeable, MonadUnion, UnionM, liftUnionM)
import Grisette.Lib.Synth.Context (MonadContext)

class (MonadContext ctx) => HasSubProg op subProg ctx | op -> subProg where
  getSubProg :: op -> ctx [subProg]

instance
  ( MonadContext ctx,
    MonadUnion ctx,
    HasSubProg op subProg ctx,
    Mergeable subProg,
    Mergeable op
  ) =>
  HasSubProg (UnionM op) subProg ctx
  where
  getSubProg op = liftUnionM op >>= getSubProg
