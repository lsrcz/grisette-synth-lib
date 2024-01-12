{-# LANGUAGE ConstraintKinds #-}

module Grisette.Lib.Synth.Operator.VarId (ConcreteVarId) where

import Data.Hashable (Hashable)
import Grisette (GPretty)

type ConcreteVarId varId =
  ( GPretty varId,
    Show varId,
    Eq varId,
    Hashable varId,
    Integral varId
  )
