{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Grisette.Lib.Synth.Program.SymbolTable
  ( SymbolTable (..),
    lookupSymbol,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Bytes.Serial (Serial)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette (EvalSym, Mergeable, ToCon (toCon))
import Grisette.Lib.Synth.Context (ConcreteContext)

newtype SymbolTable prog = SymbolTable [(T.Text, prog)]
  deriving (Show, Eq, Generic)
  deriving newtype (Mergeable, EvalSym, Semigroup, Monoid)
  deriving anyclass (Serial, NFData)

instance
  (ToCon symProg conProg) =>
  ToCon (SymbolTable symProg) (SymbolTable conProg)
  where
  toCon (SymbolTable table) =
    SymbolTable <$> traverse (\(s, p) -> (s,) <$> toCon p) table

lookupSymbol :: SymbolTable prog -> T.Text -> ConcreteContext prog
lookupSymbol (SymbolTable table) symbol = go table
  where
    go [] = throwError $ "Symbol " <> symbol <> " not found"
    go ((sym, p) : rest) = if sym == symbol then return p else go rest
