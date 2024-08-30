{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Grisette.Lib.Synth.Reasoning.IOPair (IOPair (..)) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Grisette (Default (Default), Mergeable, PPrint, ToCon, ToSym)

data IOPair val = IOPair {ioPairInputs :: [val], ioPairOutputs :: [val]}
  deriving (Show, Eq, Generic)
  deriving anyclass (NFData)
  deriving
    (ToSym (IOPair conVal), ToCon (IOPair conVal), Mergeable, PPrint)
    via (Default (IOPair val))
