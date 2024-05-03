{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Grisette.Lib.Synth.Reasoning.IOPair (IOPair (..)) where

import GHC.Generics (Generic)
import Grisette (Default (Default), ToCon, ToSym)

data IOPair val = IOPair {ioPairInputs :: [val], ioPairOutputs :: [val]}
  deriving (Show, Eq, Generic)
  deriving
    (ToSym (IOPair conVal), ToCon (IOPair conVal))
    via (Default (IOPair val))
