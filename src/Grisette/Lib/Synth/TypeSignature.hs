{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.TypeSignature (TypeSignature (..)) where

import Control.DeepSeq (NFData)
import qualified Data.Binary as Binary
import Data.Bytes.Serial (Serial, deserialize, serialize)
import Data.Hashable (Hashable)
import qualified Data.Serialize as Cereal
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvalSym,
    ExtractSym,
    Mergeable,
    SymEq,
    SymOrd,
    ToCon,
    ToSym,
  )

data TypeSignature ty = TypeSignature {argTypes :: [ty], resTypes :: [ty]}
  deriving (Show, Eq, Generic)
  deriving anyclass (NFData, Hashable, Serial)
  deriving
    ( SymEq,
      SymOrd,
      EvalSym,
      ExtractSym,
      Mergeable,
      ToCon (TypeSignature sty),
      ToSym (TypeSignature cty)
    )
    via (Default (TypeSignature ty))

instance (Serial ty) => Cereal.Serialize (TypeSignature ty) where
  put = serialize
  get = deserialize

instance (Serial ty) => Binary.Binary (TypeSignature ty) where
  put = serialize
  get = deserialize
