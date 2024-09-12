{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Grisette.Lib.Synth.Reasoning.IOPair (IOPair (..)) where

import Control.DeepSeq (NFData)
import qualified Data.Binary as Binary
import Data.Bytes.Serial (Serial (deserialize, serialize))
import qualified Data.Serialize as Cereal
import GHC.Generics (Generic)
import Grisette (Default (Default), Mergeable, PPrint, ToCon, ToSym)

data IOPair val = IOPair {ioPairInputs :: [val], ioPairOutputs :: [val]}
  deriving (Show, Eq, Generic)
  deriving anyclass (NFData, Serial)
  deriving
    (ToSym (IOPair conVal), ToCon (IOPair conVal), Mergeable, PPrint)
    via (Default (IOPair val))

instance (Serial val) => Cereal.Serialize (IOPair val) where
  put = serialize
  get = deserialize

instance (Serial val) => Binary.Binary (IOPair val) where
  put = serialize
  get = deserialize
