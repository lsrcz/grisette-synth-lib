{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Grisette.Lib.Synth.Program.SubProg (HasSubProg (..)) where

import Grisette.Lib.Synth.Context (MonadContext)

class (MonadContext ctx) => HasSubProg op subProg ctx | op -> subProg where
  getSubProg :: op -> ctx [subProg]
