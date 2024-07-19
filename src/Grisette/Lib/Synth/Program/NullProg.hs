{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.Program.NullProg (NullProg) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Grisette (Default (Default), EvalSym, Mergeable, ToCon, ToSym)
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Program.Concrete.Program
  ( ProgPPrint (topologicalPFormatProg),
    ProgToDot (topologicalProgToDot),
  )
import Grisette.Lib.Synth.Program.ProgNaming (ProgNaming (nameProg))
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Program.ProgTyping (ProgTyping (typeProg))

data NullProg
  deriving (Generic)
  deriving anyclass (NFData, Hashable)
  deriving
    (EvalSym, Mergeable, ToCon NullProg, ToSym NullProg)
    via (Default NullProg)

instance Show NullProg where
  show _ = error "Impossible"

instance Eq NullProg where
  _ == _ = error "Impossible"

instance (MonadContext ctx) => ProgSemantics semObj NullProg val ctx where
  runProg _ _ = error "Impossible"

instance ProgNaming NullProg where
  nameProg _ = error "Impossible"

instance ProgTyping NullProg ty where
  typeProg _ = error "Impossible"

instance ProgPPrint NullProg where
  topologicalPFormatProg _ _ = error "Impossible"

instance ProgToDot NullProg where
  topologicalProgToDot _ _ = error "Impossible"
