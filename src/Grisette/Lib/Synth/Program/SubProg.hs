{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Program.SubProg
  ( -- HasSubProgs (..),
    -- HasAnyPathSubProgs (..),
  )
where

{-
import Grisette
  ( Mergeable,
    MonadUnion,
    PlainUnion (overestimateUnionValues),
    Union,
    liftUnion,
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
  HasSubProgs (Union op) subProg ctx
  where
  getSubProgs op = liftUnion op >>= getSubProgs

class HasAnyPathSubProgs op subProg | op -> subProg where
  getAnyPathSubProgs :: op -> [subProg]

instance
  ( HasAnyPathSubProgs op subProg,
    Mergeable subProg,
    Mergeable op
  ) =>
  HasAnyPathSubProgs (Union op) subProg
  where
  getAnyPathSubProgs op =
    concatMap getAnyPathSubProgs (overestimateUnionValues op)
-}