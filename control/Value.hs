{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Value
  ( Value (..),
    ValueBuilder (..),
    ValueExtractor (..),
    SymValue,
  )
where

import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    Mergeable,
    MonadUnion,
    SEq,
    ToSym,
    UnionM,
    liftToMonadUnion,
    mrgReturn,
  )
import Grisette.Lib.Synth.Context (MonadContext (raiseError, result))

data Value intVal boolVal
  = IntValue intVal
  | BoolValue boolVal
  deriving (Show, Eq, Generic)
  deriving
    (Mergeable, SEq, ToSym (Value symIntVal symBoolVal))
    via (Default (Value intVal boolVal))

class (Mergeable val) => ValueBuilder val where
  type IntValType val
  type BoolValType val
  mkInt :: IntValType val -> val
  mkBool :: BoolValType val -> val

class (MonadContext ctx, ValueBuilder val) => ValueExtractor val ctx where
  getInt :: val -> ctx (IntValType val)
  getBool :: val -> ctx (BoolValType val)

instance
  (MonadContext ctx, Mergeable intVal, Mergeable boolVal) =>
  ValueExtractor (Value intVal boolVal) ctx
  where
  getInt (IntValue i) = result i
  getInt _ = raiseError "Not an integer"
  getBool (BoolValue b) = result b
  getBool _ = raiseError "Not a boolean"

instance
  (Mergeable intVal, Mergeable boolVal) =>
  ValueBuilder (Value intVal boolVal)
  where
  type IntValType (Value intVal boolVal) = intVal
  type BoolValType (Value intVal boolVal) = boolVal
  mkInt = IntValue
  mkBool = BoolValue

type SymValue intVal boolVal = UnionM (Value intVal boolVal)

instance
  (Mergeable intVal, Mergeable boolVal) =>
  ValueBuilder (SymValue intVal boolVal)
  where
  type IntValType (SymValue intVal boolVal) = intVal
  type BoolValType (SymValue intVal boolVal) = boolVal
  mkInt = mrgReturn . IntValue
  mkBool = mrgReturn . BoolValue

instance
  (MonadContext ctx, MonadUnion ctx, Mergeable intVal, Mergeable boolVal) =>
  ValueExtractor (SymValue intVal boolVal) ctx
  where
  getInt x = liftToMonadUnion x >>= getInt
  getBool x = liftToMonadUnion x >>= getBool
