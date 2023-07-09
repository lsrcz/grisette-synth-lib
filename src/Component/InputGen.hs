{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Component.InputGen where

import Grisette

class IntermediateSGen gen op s | gen -> op s where
  intermediateGen ::
    (MonadFresh m) =>
    gen ->
    op ->
    Int ->
    m s

newtype SimpleSGen op s = SimpleSGen
  { unSimpleSGen ::
    forall m. MonadFresh m => Int -> m s
  }

instance IntermediateSGen (SimpleSGen op s) op s where
  intermediateGen (SimpleSGen g) _ = g
