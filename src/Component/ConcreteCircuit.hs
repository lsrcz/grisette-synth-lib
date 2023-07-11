{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Component.ConcreteCircuit where

import Component.Circuit
import Component.Index
import Component.SemMap
import Data.Either (fromRight)
import Data.List
import GHC.Generics
import Grisette

data CNode op idx = CNode
  { cnodeOp :: op,
    cnodeIdx :: [idx],
    cnodeInputIdx :: [idx]
  }
  deriving (Show, Eq, Generic)

deriving via
  (Default (CNode cop cidx))
  instance
    (ToCon op cop, ToCon idx cidx) =>
    ToCon (Node op idx) (CNode cop cidx)

data CCircuit op idx = CCircuit
  { ccirInputNum :: Int,
    ccirNodes :: [CNode op idx],
    ccirOutputIdx :: [idx]
  }
  deriving (Show, Eq, Generic)

deriving via
  (Default (CCircuit cop cidx))
  instance
    (ToCon op cop, ToCon idx cidx) =>
    ToCon (Circuit op idx) (CCircuit cop cidx)

interpretCCircuit ::
  forall cidx cfm cop c e.
  (CIndex cidx, CSemMap cfm cop e c) =>
  [c] ->
  CCircuit cop cidx ->
  cfm ->
  Either e [c]
interpretCCircuit inputs c@(CCircuit ninput nodes oidx) sem | ninput /= length inputs = error "Bad inputs"
interpretCCircuit inputs c@(CCircuit ninput nodes oidx) sem =
  go inputs (sortOn (head . cnodeIdx) nodes)
  where
    go l [] = traverse (atCIndex (error "Bad circuit") l) oidx
    go l (CNode _ o _ : _) | mkCIndex ninput (length l) /= (head o) = error "Bad circuit"
    go l (CNode op o i : xs) = do
      next <- applyCOp (opCSem sem op) $ fromRight (error "Bad circuit") . atCIndex (error "Bad circuit") l <$> i
      go (l ++ next) xs
