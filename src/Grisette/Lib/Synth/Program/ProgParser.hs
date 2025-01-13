{-# LANGUAGE FlexibleContexts #-}

module Grisette.Lib.Synth.Program.ProgParser
  ( ProgParser (..),
    progTableParser,
  )
where

import Control.Applicative (Alternative (many))
import qualified Data.Text as T
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable (SymbolTable))
import Grisette.Lib.Synth.Util.Parser (CharParser)

class ProgParser prog where
  progParser :: (CharParser e s m) => m (T.Text, prog)

progTableParser :: (CharParser e s m, ProgParser prog) => m (SymbolTable prog)
progTableParser = do
  progList <- many progParser
  return $ SymbolTable progList
