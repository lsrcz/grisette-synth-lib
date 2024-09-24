{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.TypeSignature (TypeSignature (..)) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvalSym,
    ExtractSym,
    Mergeable,
    SymEq,
    SymOrd,
  )

data TypeSignature ty = TypeSignature {argTypes :: [ty], resTypes :: [ty]}
  deriving (Show, Eq, Generic)
  deriving anyclass (NFData, Hashable)
  deriving
    ( SymEq,
      SymOrd,
      EvalSym,
      ExtractSym,
      Mergeable
    )
    via (Default (TypeSignature ty))
