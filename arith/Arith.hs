{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Arith
  ( OpCode (..),
    Sem (..),
    OpType (..),
  )
where

import GHC.Generics (Generic)
import Grisette (Default (Default), EvaluateSym, GPretty (gpretty), GenSymSimple (simpleFresh), Mergeable, MonadFresh, SymInteger, ToCon)
import Grisette.Lib.Synth.Context (MonadContext (raiseError, result))
import Grisette.Lib.Synth.Operator.OpPretty
  ( OpPretty (describeArguments, prefixResults),
    OpPrettyError (IncorrectNumberOfArguments, IncorrectNumberOfResults),
  )
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Operator.OpTyping
  ( GenIntermediate (genIntermediate),
    OpTyping (typeOp),
  )
import Grisette.Lib.Synth.Program.Concrete
  ( OpDirectSubProgs (opDirectSubProgs),
    SomePrettyProg,
  )
import Grisette.Lib.Synth.Util.Show (showText)

data OpCode
  = Plus
  | Mul
  | Minus
  | UMinus
  deriving (Show, Generic)
  deriving (EvaluateSym, ToCon OpCode) via (Default OpCode)

instance OpDirectSubProgs OpCode SomePrettyProg where
  opDirectSubProgs _ = []

instance GPretty OpCode where
  gpretty Plus = "plus"
  gpretty Mul = "mul"
  gpretty Minus = "minus"
  gpretty UMinus = "uminus"

data Sem = Sem

instance
  (MonadContext ctx, Num a, Mergeable a) =>
  OpSemantics Sem OpCode a ctx
  where
  applyOp _ Plus [x, y] = result [x + y]
  applyOp _ Mul [x, y] = result [x * y]
  applyOp _ Minus [x, y] = result [x - y]
  applyOp _ UMinus [x] = result [x]
  applyOp _ op _ = raiseError $ "Invalid arguments to operator " <> showText op

instance OpPretty OpCode where
  describeArguments Plus 2 = Right [Just "lhs", Just "rhs"]
  describeArguments Mul 2 = Right [Just "lhs", Just "rhs"]
  describeArguments Minus 2 = Right [Just "lhs", Just "rhs"]
  describeArguments UMinus 1 = Right [Nothing]
  describeArguments op n = Left $ IncorrectNumberOfArguments op n
  prefixResults Plus 2 1 = Right ["r"]
  prefixResults Mul 2 1 = Right ["r"]
  prefixResults Minus 2 1 = Right ["r"]
  prefixResults UMinus 1 1 = Right ["r"]
  prefixResults op n m = Left $ IncorrectNumberOfResults op n m

data OpType = IntegerType
  deriving (Show, Generic)
  deriving (GPretty, Mergeable, EvaluateSym) via (Default OpType)

instance (MonadContext ctx) => OpTyping Sem OpCode OpType ctx where
  typeOp _ Plus 2 = result ([IntegerType, IntegerType], [IntegerType])
  typeOp _ Mul 2 = result ([IntegerType, IntegerType], [IntegerType])
  typeOp _ Minus 2 = result ([IntegerType, IntegerType], [IntegerType])
  typeOp _ UMinus 1 = result ([IntegerType], [IntegerType])
  typeOp _ op _ = raiseError $ "Invalid arguments to operator " <> showText op

instance
  (MonadContext ctx, MonadFresh ctx) =>
  GenIntermediate Sem OpType SymInteger ctx
  where
  genIntermediate _ IntegerType = simpleFresh ()
