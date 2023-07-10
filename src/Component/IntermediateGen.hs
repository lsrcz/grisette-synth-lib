{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module Component.IntermediateGen
  ( IntermediateSGen (..),
    HomogeneousSGen (..),
    SimpleSGen (..),
  )
where

import Grisette

class IntermediateSGen gen op s | gen -> op s where
  intermediateGen ::
    (MonadFresh m) =>
    gen ->
    Int ->
    op ->
    Int ->
    m s

newtype HomogeneousSGen op s = HomogeneousSGen
  { unHomogeneousSGen ::
      forall m.
      MonadFresh m =>
      Int ->
      m s
  }

newtype SimpleSGen op s = SimpleSGen
  { unSimpleSGen ::
      forall m.
      MonadFresh m =>
      Int ->
      op ->
      Int ->
      m s
  }

instance IntermediateSGen (HomogeneousSGen op s) op s where
  intermediateGen (HomogeneousSGen g) size _ _ = g size

instance IntermediateSGen (SimpleSGen op s) op s where
  intermediateGen (SimpleSGen g) = g
