{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.Program.ProgTyping (ProgTyping (..)) where

import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.TypeSignature (TypeSignature)

class ProgTyping semObj prog ty where
  typeProg :: (MonadContext ctx) => semObj -> prog -> ctx (TypeSignature ty)
