{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.Program.SumProg (SumProg (..)) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Grisette (Default (Default), EvaluateSym, Mergeable, ToCon, ToSym)
import Grisette.Lib.Synth.Program.Concrete.Program
  ( ProgGPretty (topologicalGPrettyProg),
    ProgToDot (topologicalProgToDot),
  )
import Grisette.Lib.Synth.Program.ProgNaming (ProgNaming (nameProg))
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Program.ProgTyping (ProgTyping (typeProg))

data SumProg l r
  = SumProgL l
  | SumProgR r
  deriving (Eq, Generic)
  deriving anyclass (NFData, Hashable)
  deriving
    ( EvaluateSym,
      Mergeable,
      ToCon (SumProg sl sr),
      ToSym (SumProg cl cr)
    )
    via (Default (SumProg l r))

instance (Show l, Show r) => Show (SumProg l r) where
  show (SumProgL l) = show l
  show (SumProgR r) = show r

instance
  (ProgSemantics semObj l val ctx, ProgSemantics semObj r val ctx) =>
  ProgSemantics semObj (SumProg l r) val ctx
  where
  runProg semObj (SumProgL l) = runProg semObj l
  runProg semObj (SumProgR r) = runProg semObj r

instance (ProgNaming l, ProgNaming r) => ProgNaming (SumProg l r) where
  nameProg (SumProgL l) = nameProg l
  nameProg (SumProgR r) = nameProg r

instance (ProgTyping l ty, ProgTyping r ty) => ProgTyping (SumProg l r) ty where
  typeProg (SumProgL l) = typeProg l
  typeProg (SumProgR r) = typeProg r

instance (ProgGPretty l, ProgGPretty r) => ProgGPretty (SumProg l r) where
  topologicalGPrettyProg (SumProgL l) = topologicalGPrettyProg l
  topologicalGPrettyProg (SumProgR r) = topologicalGPrettyProg r

instance (ProgToDot l, ProgToDot r) => ProgToDot (SumProg l r) where
  topologicalProgToDot (SumProgL l) = topologicalProgToDot l
  topologicalProgToDot (SumProgR r) = topologicalProgToDot r
