{-# LANGUAGE ConstraintKinds #-}

module Grisette.Lib.Synth.VarId
  ( ConcreteVarId,
    SymbolicVarId,
    RelatedVarId,
  )
where

import Data.Hashable (Hashable)
import Data.Typeable (Typeable)
import Grisette (GPretty, Mergeable, SOrd, ToCon, ToSym)

type ConcreteVarId varId =
  ( GPretty varId,
    Show varId,
    Eq varId,
    Hashable varId,
    Integral varId,
    Typeable varId,
    Mergeable varId
  )

type SymbolicVarId varId =
  ( Show varId,
    Eq varId,
    Num varId,
    SOrd varId,
    Mergeable varId,
    Typeable varId
  )

type RelatedVarId conVarId symVarId =
  ( ConcreteVarId conVarId,
    SymbolicVarId symVarId,
    ToSym conVarId symVarId,
    ToCon symVarId conVarId
  )
