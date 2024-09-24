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
    ProgReachableSymbols (..),
    transitivelyReachableSymbols,
    filterByReachableSymbols,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Bifunctor (Bifunctor (second))
import Data.Bytes.Serial (Serial)
import qualified Data.HashSet as HS
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette (EvalSym, Mergeable, ToCon (toCon), ToSym (toSym))
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

instance
  (ToSym conProg symProg) =>
  ToSym (SymbolTable conProg) (SymbolTable symProg)
  where
  toSym (SymbolTable table) =
    SymbolTable $ second toSym <$> table

lookupSymbol :: SymbolTable prog -> T.Text -> ConcreteContext prog
lookupSymbol (SymbolTable table) symbol = go table
  where
    go [] = throwError $ "Symbol " <> symbol <> " not found"
    go ((sym, p) : rest) = if sym == symbol then return p else go rest

class ProgReachableSymbols prog where
  progReachableSymbols :: prog -> HS.HashSet T.Text

transitivelyReachableSymbols ::
  (ProgReachableSymbols prog) =>
  HS.HashSet T.Text ->
  SymbolTable prog ->
  ConcreteContext (HS.HashSet T.Text)
transitivelyReachableSymbols symbols table = go symbols symbols
  where
    go confirmed queue = do
      allNew <- fmap mconcat $ traverse (go1 confirmed) $ HS.toList queue
      go (HS.union confirmed allNew) allNew
    go1 confirmed s = do
      prog <- lookupSymbol table s
      let symbols = progReachableSymbols prog
      return $ HS.difference symbols confirmed

filterByReachableSymbols ::
  (ProgReachableSymbols prog) =>
  HS.HashSet T.Text ->
  SymbolTable prog ->
  ConcreteContext (SymbolTable prog)
filterByReachableSymbols symbols table@(SymbolTable t) = do
  progReachable <- transitivelyReachableSymbols symbols table
  let filtered = filter (\(s, _) -> HS.member s progReachable) t
  return $ SymbolTable filtered
