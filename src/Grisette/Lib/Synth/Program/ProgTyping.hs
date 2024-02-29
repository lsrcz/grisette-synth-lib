{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.Program.ProgTyping (ProgTyping (..)) where

import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.TypeSignature (TypeSignature)

class ProgTyping prog ty where
  typeProg :: (MonadContext ctx) => prog -> ctx (TypeSignature ty)
