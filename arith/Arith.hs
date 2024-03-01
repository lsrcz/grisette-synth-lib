{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Grisette
  ( Default (Default),
    EvaluateSym,
    GPretty (gpretty),
    GenSymSimple (simpleFresh),
    Mergeable,
    MonadFresh,
    MonadUnion,
    SymInteger,
    ToCon,
    mrgReturn,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Operator.OpTyping
  ( OpTyping,
    OpTypingSimple (typeOpSimple),
    SymOpLimits,
  )
import Grisette.Lib.Synth.Program.ComponentSketch
  ( GenIntermediate (genIntermediate),
  )
import Grisette.Lib.Synth.Program.Concrete
  ( OpDirectSubProgs (opDirectSubProgs),
    OpPretty (describeArguments, prefixResults),
    OpPrettyError (IncorrectNumberOfArguments, IncorrectNumberOfResults),
    SomePrettyProg,
  )
import Grisette.Lib.Synth.TypeSignature
  ( TypeSignature (TypeSignature),
  )
import Grisette.Lib.Synth.Util.Show (showText)

-- * Operators

-- | The operators in this example are simple arithmetic operators.
data OpCode
  = Plus
  | Mul
  | Minus
  | UMinus
  deriving (Show, Generic)
  deriving (EvaluateSym, ToCon OpCode) via (Default OpCode)

-- * Semantics and typing.

-- | We use the type information for generating the intermediate values for the
-- synthesizer.

-- | A semantics object. You may use different semantics objects to choose
-- different semantics of the operators. For now, we just define a simple unit
-- object that does not carry any information.
data Sem = Sem

-- | The semantics of the operators are given with the 'OpSemantics' type class.
-- The ctx here is the context for the evaluation. It is a monadic context that
-- supports error handling and may support merging of multiple paths and fresh
-- variable generation.
--
-- Grisette provide several contexts. For example, the 'ConcreteContext' is for
-- running a program concretely, while an 'AngelicContext' is for running a
-- program symbolically, with the introduction of angelic variables. To use the
-- component encoding for synthesis, you need to use the 'AngelicContext'.
instance
  (MonadContext ctx, Num a, Mergeable a) =>
  OpSemantics Sem OpCode a ctx
  where
  applyOp _ Plus [x, y] = mrgReturn [x + y]
  applyOp _ Mul [x, y] = mrgReturn [x * y]
  applyOp _ Minus [x, y] = mrgReturn [x - y]
  applyOp _ UMinus [x] = mrgReturn [x]
  applyOp _ op _ =
    mrgThrowError $ "Invalid number of arguments to operator " <> showText op

-- | Here we only have one type, the integer type. The component encoding
-- (https://ieeexplore.ieee.org/abstract/document/6062089) needs to generate
-- intermediate values for the inputs and outputs of the operators. We use this
-- type info for that generation.
data OpType = IntegerType
  deriving (Show, Eq, Generic)
  deriving (Mergeable, EvaluateSym) via (Default OpType)

-- instance OpTypingSimple OpCode OpType where
instance OpTypingSimple OpCode OpType where
  typeOpSimple Plus = TypeSignature [IntegerType, IntegerType] [IntegerType]
  typeOpSimple Mul = TypeSignature [IntegerType, IntegerType] [IntegerType]
  typeOpSimple Minus = TypeSignature [IntegerType, IntegerType] [IntegerType]
  typeOpSimple UMinus = TypeSignature [IntegerType] [IntegerType]

instance (MonadContext ctx) => OpTyping OpCode OpType ctx

instance SymOpLimits OpCode

-- | Here, for generating `SymInteger`, we just generate a fresh variable using
-- `simpleFresh` provided by Grisette.
instance
  (MonadContext ctx, MonadFresh ctx, MonadUnion ctx) =>
  GenIntermediate Sem OpType SymInteger ctx
  where
  genIntermediate _ IntegerType = simpleFresh ()

-- Pretty printing

-- You may ignore this for now. This is used when your program supports
-- procedure calls.
instance OpDirectSubProgs OpCode SomePrettyProg where
  opDirectSubProgs _ = []

instance GPretty OpType where
  gpretty IntegerType = "int"

instance GPretty OpCode where
  gpretty Plus = "plus"
  gpretty Mul = "mul"
  gpretty Minus = "minus"
  gpretty UMinus = "uminus"

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
