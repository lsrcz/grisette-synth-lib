{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Program.SubProg
  ( HasSubProgs (..),
    HasAnyPathSubProgs (..),
  )
where

import Grisette
  ( Mergeable,
    MonadUnion,
    PlainUnion (overestimateUnionValues),
    UnionM,
    liftUnionM,
  )
import Grisette.Lib.Synth.Context (MonadContext)

class (MonadContext ctx) => HasSubProgs op subProg ctx | op -> subProg where
  getSubProgs :: op -> ctx [subProg]

instance
  ( MonadContext ctx,
    MonadUnion ctx,
    HasSubProgs op subProg ctx,
    Mergeable subProg,
    Mergeable op
  ) =>
  HasSubProgs (UnionM op) subProg ctx
  where
  getSubProgs op = liftUnionM op >>= getSubProgs

class HasAnyPathSubProgs op subProg | op -> subProg where
  getAnyPathSubProgs :: op -> [subProg]

instance
  ( HasAnyPathSubProgs op subProg,
    Mergeable subProg,
    Mergeable op
  ) =>
  HasAnyPathSubProgs (UnionM op) subProg
  where
  getAnyPathSubProgs op =
    concatMap getAnyPathSubProgs (overestimateUnionValues op)
