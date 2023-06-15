{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Component.Circuit where

import GHC.Generics
import Grisette
import Component.Index
import Control.Monad.Except

data Node op idx = Node
  { nodeOp :: op,
    nodeIdx :: idx,
    inputIdx :: [idx]
  }
  deriving (Show, Eq, Generic)
  deriving (Mergeable, SEq, EvaluateSym) via (Default (Node op idx))

data Circuit op idx = Circuit
  { circuitNodes :: [Node op idx],
    circuitOutputIdx :: [idx]
  } deriving (Show, Eq, Generic)
  deriving (Mergeable, SEq, EvaluateSym) via (Default (Circuit op idx))

data ComponentSpec op = ComponentSpec
  { componentOp :: op,
    componentInputNum :: Int
  }

data CircuitSpec op = CircuitSpec
  { circuitComponents :: [(ComponentSpec op, Int)],
    circuitInputNum :: Int,
    circuitOutputNum :: Int
  }

instance (Num idx, SOrd idx,
  GenSymSimpleExcept (SOrdBound idx ()) idx,
  Mergeable op) =>
  GenSymSimpleExcept (CircuitSpec op) (Circuit op idx) where
  simpleFreshExcept :: forall idx op m e. (Num idx, SOrd idx, GenSymSimpleExcept (SOrdBound idx ()) idx,
     Mergeable op, MonadFresh m,
     MonadError e m, UnionLike m) =>
    e -> CircuitSpec op -> m (Circuit op idx)
  simpleFreshExcept e (CircuitSpec components inum onum) = do
    o :: [idx] <- simpleFreshExcept e (SimpleListSpec onum (SOrdBound 0 (fromIntegral $ inum + compNum) () :: SOrdBound idx ()))
    n <- mrgTraverse genNode specs
    let circuit = Circuit n o 
    mrgSingle circuit
    where
      specs' [] = []
      specs' ((x,n):xs) = replicate n x ++ specs' xs
      specs = specs' components
      compNum = length specs
      genNode :: ComponentSpec op -> m (Node op idx)
      genNode (ComponentSpec op opinum) = do
        o :: idx <- simpleFreshExcept e (SOrdBound (fromIntegral inum) (fromIntegral $ inum + compNum) () :: SOrdBound idx ())
        i :: [idx] <- simpleFreshExcept e
          (SimpleListSpec opinum (SOrdBound (fromIntegral inum) (fromIntegral $ inum + compNum) () :: SOrdBound idx ()))
        mrgSingle $ Node op o i

