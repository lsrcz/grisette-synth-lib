module Grisette.Lib.Synth.Reasoning.IOPair (IOPair (..)) where

data IOPair val = IOPair {ioPairInputs :: [val], ioPairOutputs :: [val]}
  deriving (Show, Eq)
