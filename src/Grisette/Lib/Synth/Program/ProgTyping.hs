{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.Program.ProgTyping
  ( ProgTyping (..),
    ProgTypeTable (..),
    typeSymbolTable,
    lookupType,
    symbolType,
  )
where

import Data.Bifunctor (Bifunctor (second))
import qualified Data.Text as T
import Grisette.Lib.Synth.Context (ConcreteContext)
import Grisette.Lib.Synth.Program.ProgUtil (ProgUtil (ProgTypeType))
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable (SymbolTable))
import Grisette.Lib.Synth.TypeSignature (TypeSignature)

class (ProgUtil prog) => ProgTyping prog where
  typeProg :: prog -> TypeSignature (ProgTypeType prog)

newtype ProgTypeTable ty
  = ProgTypeTable [(T.Text, TypeSignature ty)]
  deriving newtype (Semigroup, Monoid)

typeSymbolTable ::
  (ProgTyping prog) => SymbolTable prog -> ProgTypeTable (ProgTypeType prog)
typeSymbolTable (SymbolTable table) =
  let res = ProgTypeTable $ fmap (second typeProg) table in res

lookupType ::
  ProgTypeTable ty ->
  T.Text ->
  ConcreteContext (TypeSignature ty)
lookupType (ProgTypeTable table) symbol = go table
  where
    go [] = error $ "Symbol " <> T.unpack symbol <> " not found"
    go ((sym, f) : rest) = if sym == symbol then return f else go rest

symbolType ::
  (ProgTyping prog) =>
  SymbolTable prog ->
  T.Text ->
  ConcreteContext (TypeSignature (ProgTypeType prog))
symbolType table = lookupType (typeSymbolTable table)
