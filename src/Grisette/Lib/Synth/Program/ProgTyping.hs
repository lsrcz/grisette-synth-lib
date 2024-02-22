{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.Program.ProgTyping (ProgTyping (..)) where

import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Operator.OpTyping (TypeSignature)

class (MonadContext ctx) => ProgTyping semObj prog ty ctx where
  typeProg :: semObj -> prog -> ctx (TypeSignature ty)
