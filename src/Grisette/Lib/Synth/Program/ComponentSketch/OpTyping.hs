{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Program.ComponentSketch.OpTyping
  ( OpTypingSimple (..),
    OpTypingByNumInputs (..),
    OpTyping (..),
  )
where

import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.TypeSignature (TypeSignature)

class OpTypingSimple op ty where
  typeOpSimple :: (MonadContext ctx) => op -> ctx (TypeSignature ty)

class OpTypingByNumInputs op ty where
  typeOpByNumInputs ::
    (MonadContext ctx) => op -> Int -> ctx (TypeSignature ty)

class OpTyping op ty where
  typeOp :: (MonadContext ctx) => op -> [ty] -> ctx (TypeSignature ty)
