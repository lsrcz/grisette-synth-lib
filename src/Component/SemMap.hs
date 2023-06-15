{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Component.SemMap where

import Control.Monad.Except
import Grisette

data NOperands
  = NOperands Int
  | CommutativeTwoOperands
  | ListOperands

data OpSem a g where
  OpSem ::
    g ->
    NOperands ->
    ( ( MonadError VerificationConditions m,
        MonadUnion m,
        MonadFresh m,
        Mergeable a
      ) =>
      [a] ->
      m a
    ) ->
    OpSem a g

class SemMap fm op a g | fm -> op a g where
  opSemMaybe :: fm -> op -> Maybe (OpSem a g)
  opSem :: fm -> op -> OpSem a g

data OpCSem a where
  OpCSem ::
    NOperands ->
    ([a] ->
     Either VerificationConditions a
    ) ->
    OpCSem a

