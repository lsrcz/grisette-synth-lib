{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Program.ComponentSketch.OpTyping
  ( OpTypingSimple (..),
    OpTypingByNumInputs (..),
    OpTypingByInputTypes (..),
  )
where

import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.TypeSignature (TypeSignature)

class OpTypingSimple semObj op ty | semObj op -> ty where
  typeOpSimple :: (MonadContext ctx) => semObj -> op -> ctx (TypeSignature ty)

class OpTypingByNumInputs semObj op ty | semObj op -> ty where
  typeOpByNumInputs ::
    (MonadContext ctx) => semObj -> op -> Int -> ctx (TypeSignature ty)

class OpTypingByInputTypes semObj op ty | semObj op -> ty where
  typeOpByInputTypes ::
    (MonadContext ctx) => semObj -> op -> [ty] -> ctx (TypeSignature ty)
