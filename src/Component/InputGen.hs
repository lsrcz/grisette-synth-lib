{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module Component.InputGen
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
    op ->
    Int ->
    m s

newtype HomogeneousSGen op s = HomogeneousSGen
  { unHomogeneousSGen ::
      forall m.
      MonadFresh m =>
      m s
  }

newtype SimpleSGen op s = SimpleSGen
  { unSimpleSGen ::
      forall m.
      MonadFresh m =>
      op ->
      Int ->
      m s
  }

instance IntermediateSGen (HomogeneousSGen op s) op s where
  intermediateGen (HomogeneousSGen g) _ _ = g

instance IntermediateSGen (SimpleSGen op s) op s where
  intermediateGen (SimpleSGen g) = g
