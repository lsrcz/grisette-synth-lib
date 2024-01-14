{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (..)) where

import Grisette.Lib.Synth.Context (MonadContext)

class (MonadContext ctx) => ProgSemantics semObj prog val ctx where
  runProg :: semObj -> prog -> [val] -> ctx [val]
