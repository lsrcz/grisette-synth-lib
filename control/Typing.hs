{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Typing
  ( Type (..),
    typePlus,
    typeEquals,
    typeMinus,
    typeIntConst,
    typeIf,
  )
where

import Control.Monad (when)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvalSym,
    GenSym (fresh),
    GenSymSimple (simpleFresh),
    Mergeable,
    PPrint (pformat),
    ToCon,
  )
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Program.ProgTyping (ProgTyping (typeProg))
import Grisette.Lib.Synth.TypeSignature
  ( TypeSignature (TypeSignature),
  )
import Value (Value, ValueBuilder (mkBool, mkInt))

data Type = IntType | BoolType
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable)
  deriving (Mergeable, EvalSym, ToCon Type) via (Default Type)

instance PPrint Type where
  pformat IntType = "int"
  pformat BoolType = "bool"

typePlus :: (MonadContext ctx) => ctx (TypeSignature Type)
typePlus = mrgReturn $ TypeSignature [IntType, IntType] [IntType]

typeEquals :: (MonadContext ctx) => ctx (TypeSignature Type)
typeEquals = mrgReturn $ TypeSignature [IntType, IntType] [BoolType]

typeMinus :: (MonadContext ctx) => ctx (TypeSignature Type)
typeMinus = mrgReturn $ TypeSignature [IntType, IntType] [IntType]

typeIntConst :: (MonadContext ctx) => ctx (TypeSignature Type)
typeIntConst = mrgReturn $ TypeSignature [] [IntType]

typeIf ::
  (MonadContext ctx, ProgTyping prog Type) =>
  prog ->
  prog ->
  ctx (TypeSignature Type)
typeIf true false = do
  trueType@(TypeSignature trueArgType trueResType) <- typeProg true
  falseType <- typeProg false
  when (trueType /= falseType) $ mrgThrowError "Unmatched branch types"
  mrgReturn $ TypeSignature (BoolType : trueArgType) trueResType

instance
  ( Mergeable intVal,
    Mergeable boolVal,
    GenSymSimple () intVal,
    GenSymSimple () boolVal
  ) =>
  GenSym Type (Value intVal boolVal)
  where
  fresh IntType = mkInt <$> simpleFresh ()
  fresh BoolType = mkBool <$> simpleFresh ()
