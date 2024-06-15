{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Arith (OpCode (..)) where

import Grisette
  ( Default (Default),
    GPretty (gpretty),
    Mergeable,
    deriveAllGrisetteExcept,
    mrgReturn,
  )
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Operator.OpSemantics
  ( DefaultSem,
    OpSemantics (applyOp),
    pureBinaryOp,
    pureUnaryOp,
  )
import Grisette.Lib.Synth.Operator.OpTyping
  ( DefaultType,
    OpTyping (typeOp),
    binaryDefaultType,
    simpleTyping,
    unaryDefaultType,
  )
import Grisette.Lib.Synth.Program.Concrete
  ( OpPretty (describeArguments),
  )
import Grisette.Lib.Synth.Program.NullProg (NullProg)
import Grisette.Lib.Synth.Program.SubProg
  ( HasAnyPathSubProgs (getAnyPathSubProgs),
    HasSubProgs (getSubProgs),
  )

-- * Operators

-- | The operators in this example are simple arithmetic operators.
data OpCode
  = Plus
  | Mul
  | Minus
  | UMinus

deriveAllGrisetteExcept ''OpCode [''GPretty]

-- * Semantics and typing.

-- | The semantics of the operators are given with the 'OpSemantics' type class.
-- The ctx here is the context for the evaluation. It is a monadic context that
-- supports error handling and may support merging of multiple paths and fresh
-- variable generation.
--
-- Grisette provide several contexts. For example, the 'ConcreteContext' is for
-- running a program concretely, while an 'AngelicContext' is for running a
-- program symbolically, with the introduction of angelic variables. To use the
-- component encoding for synthesis, you need to use the 'AngelicContext'.
--
-- The 'DefaultSem' is a predefined semantics object. If you don't need to
-- parameterize the semantics, you can use this type. It is also used by the
-- component encoding to generate intermediate values. With 'DefaultSem' and
-- 'DefaultType' (see below), the system is able to resolve whether it can
-- generate intermediate values based on a reasonable default behavior.
instance
  (MonadContext ctx, Num a, Mergeable a) =>
  OpSemantics DefaultSem OpCode a ctx
  where
  applyOp _ Plus = pureBinaryOp "Plus" (+)
  applyOp _ Mul = pureBinaryOp "Mul" (*)
  applyOp _ Minus = pureBinaryOp "Minus" (-)
  applyOp _ UMinus = pureUnaryOp "UMinus" negate

-- | The component encoding
-- (https://ieeexplore.ieee.org/abstract/document/6062089) needs to generate
-- intermediate values for the inputs and outputs of the operators. We use type
-- info for that generation.
--
-- Since here we only have one type, the integer type, we can use the
-- 'DefaultType'. It can be used to generate intermediate symbolic integers
-- along with the 'DefaultSem'.
instance (MonadContext ctx) => OpTyping OpCode DefaultType ctx where
  typeOp = simpleTyping $ \op ->
    if
      | op `elem` [Plus, Mul, Minus] -> binaryDefaultType
      | op == UMinus -> unaryDefaultType
      | otherwise -> error "If fall through"

-- Pretty printing
instance GPretty OpCode where
  gpretty Plus = "plus"
  gpretty Mul = "mul"
  gpretty Minus = "minus"
  gpretty UMinus = "uminus"

instance OpPretty OpCode where
  describeArguments Plus = Right [Just "lhs", Just "rhs"]
  describeArguments Mul = Right [Just "lhs", Just "rhs"]
  describeArguments Minus = Right [Just "lhs", Just "rhs"]
  describeArguments UMinus = Right [Nothing]

instance (MonadContext ctx) => HasSubProgs OpCode NullProg ctx where
  getSubProgs _ = mrgReturn []

instance HasAnyPathSubProgs OpCode NullProg where
  getAnyPathSubProgs _ = []
