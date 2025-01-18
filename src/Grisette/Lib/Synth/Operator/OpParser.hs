{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Grisette.Lib.Synth.Operator.OpParser (OpParser (..)) where

import Grisette.Lib.Synth.Util.Parser (CharParser)

class OpParser op where
  opParser :: (CharParser e s m) => m op
