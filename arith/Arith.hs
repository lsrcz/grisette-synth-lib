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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Arith (OpCode (..)) where

import Control.Monad.Error.Class (MonadError (throwError))
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvalSym,
    Mergeable,
    PPrint (pformat),
    derive,
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
    OpTyping (OpTypeType, typeOp),
    binaryDefaultType,
    simpleTyping,
    unaryDefaultType,
  )
import Grisette.Lib.Synth.Program.Concrete
  ( OpPPrint (describeArguments, prefixResults),
    OpPPrintError (PPrintTypingError),
    PrefixByType (prefixByType),
    allPrefixesByTypes,
  )
import Grisette.Lib.Synth.Program.NullProg (NullProg)
import Grisette.Lib.Synth.TypeSignature (TypeSignature (TypeSignature))

-- * Operators

-- | The operators in this example are simple arithmetic operators.
data OpCode
  = Plus
  | Mul
  | Minus
  | UMinus

derive ''OpCode [''EvalSym, ''Show, ''Generic, ''Mergeable]

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
  applyOp _ _ _ Plus = pureBinaryOp "Plus" (+)
  applyOp _ _ _ Mul = pureBinaryOp "Mul" (*)
  applyOp _ _ _ Minus = pureBinaryOp "Minus" (-)
  applyOp _ _ _ UMinus = pureUnaryOp "UMinus" negate

-- | The component encoding
-- (https://ieeexplore.ieee.org/abstract/document/6062089) needs to generate
-- intermediate values for the inputs and outputs of the operators. We use type
-- info for that generation.
--
-- Since here we only have one type, the integer type, we can use the
-- 'DefaultType'. It can be used to generate intermediate symbolic integers
-- along with the 'DefaultSem'.
instance (MonadContext ctx) => OpTyping OpCode ctx where
  type OpTypeType OpCode = DefaultType
  typeOp _ = simpleTyping $ \case
    Plus -> binaryDefaultType
    Mul -> binaryDefaultType
    Minus -> binaryDefaultType
    UMinus -> unaryDefaultType

-- Pretty printing
instance PPrint OpCode where
  pformat Plus = "plus"
  pformat Mul = "mul"
  pformat Minus = "minus"
  pformat UMinus = "uminus"

instance OpPPrint OpCode where
  describeArguments _ Plus = Right [Just "lhs", Just "rhs"]
  describeArguments _ Mul = Right [Just "lhs", Just "rhs"]
  describeArguments _ Minus = Right [Just "lhs", Just "rhs"]
  describeArguments _ UMinus = Right [Nothing]

-- prefixResults table op = case typeOp table op of
--   Right (TypeSignature _ resTypes) ->
--     return $ prefixByType <$> resTypes
--   Left err -> throwError $ PPrintTypingError op err
