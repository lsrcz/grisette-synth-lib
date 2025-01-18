{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Grisette.Lib.Synth.Type.TypeParser (TypeParser (..)) where

import Grisette.Lib.Synth.Util.Parser (CharParser)

class TypeParser op where
  typeParser :: (CharParser e s m) => m op
