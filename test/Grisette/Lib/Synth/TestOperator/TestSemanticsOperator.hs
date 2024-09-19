{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (..),
    TestSemanticsObj (..),
    TestSemanticsType (..),
    TestSemanticsCost (..),
  )
where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (ArithException)
import Control.Monad (when)
import Control.Monad.Except (runExceptT)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvalSym,
    GenSymSimple (simpleFresh),
    Mergeable,
    MonadUnion,
    SafeDiv (safeDivMod),
    SymInteger,
    ToCon,
    ToSym,
    liftToMonadUnion,
  )
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Operator.OpTyping
  ( OpTyping (typeOp),
    simpleTyping,
  )
import Grisette.Lib.Synth.Program.ComponentSketch
  ( GenIntermediate (genIntermediate),
  )
import Grisette.Lib.Synth.Program.Concrete.Flatten (OpFlatten (opForwardedSubProg))
import Grisette.Lib.Synth.Program.CostModel.PerStmtCostModel (OpCost (opCost))
import Grisette.Lib.Synth.Program.NullProg (NullProg)
import Grisette.Lib.Synth.Program.ProgConstraints (OpSubProgConstraints)
import Grisette.Lib.Synth.Program.SubProg (HasSubProgs (getSubProgs))
import Grisette.Lib.Synth.TypeSignature
  ( TypeSignature (TypeSignature),
  )
import Grisette.Lib.Synth.Util.Show (showText)
import Grisette.Lib.Synth.VarId (ConcreteVarId)

data TestSemanticsOp = Add | DivMod | Inc | Double
  deriving (Show, Generic, Eq)
  deriving anyclass (Hashable)
  deriving
    (Mergeable, ToCon TestSemanticsOp, EvalSym, ToSym TestSemanticsOp)
    via (Default TestSemanticsOp)

instance
  (MonadContext ctx) =>
  OpSubProgConstraints constrObj TestSemanticsOp ctx

data TestSemanticsObj = TestSemanticsObj deriving (Eq)

instance NFData TestSemanticsObj where
  rnf TestSemanticsObj = ()

data TestSemanticsType = IntType
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable)
  deriving
    (Mergeable, EvalSym, ToCon TestSemanticsType, ToSym TestSemanticsType)
    via (Default TestSemanticsType)

instance
  (MonadContext ctx) =>
  OpSemantics TestSemanticsObj TestSemanticsOp Integer ctx
  where
  applyOp _ Add [x, y] = return [x + y]
  applyOp _ Add l =
    mrgThrowError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showText (length l)
        <> " arguments."
  applyOp _ DivMod [x, y] = do
    when (y == 0) $ mrgThrowError "ArithException: divide by zero"
    return [x `div` y, x `mod` y]
  applyOp _ DivMod l =
    mrgThrowError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showText (length l)
        <> " arguments."
  applyOp _ Inc [x] = return [x + 1]
  applyOp _ Inc l =
    mrgThrowError $
      "Incorrect number of arguments for inc, expected 1 arguments, but got "
        <> showText (length l)
        <> " arguments."
  applyOp _ Double [x] = return [x + x]
  applyOp _ Double l =
    mrgThrowError $
      "Incorrect number of arguments for dec, expected 1 arguments, but got "
        <> showText (length l)
        <> " arguments."

instance
  (MonadContext ctx, MonadUnion ctx) =>
  OpSemantics TestSemanticsObj TestSemanticsOp SymInteger ctx
  where
  applyOp _ Add [x, y] = return [x + y]
  applyOp _ Add l =
    mrgThrowError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showText (length l)
        <> " arguments."
  applyOp _ DivMod [x, y] = do
    r <- liftToMonadUnion $ runExceptT $ safeDivMod x y
    case r of
      Left (e :: ArithException) -> mrgThrowError $ "ArithException: " <> showText e
      Right (d, m) -> mrgReturn [d, m]
  applyOp _ DivMod l =
    mrgThrowError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showText (length l)
        <> " arguments."
  applyOp _ Inc [x] = return [x + 1]
  applyOp _ Inc l =
    mrgThrowError $
      "Incorrect number of arguments for inc, expected 1 arguments, but got "
        <> showText (length l)
        <> " arguments."
  applyOp _ Double [x] = return [x + x]
  applyOp _ Double l =
    mrgThrowError $
      "Incorrect number of arguments for dec, expected 1 arguments, but got "
        <> showText (length l)
        <> " arguments."

instance
  (MonadContext ctx) =>
  OpTyping TestSemanticsOp TestSemanticsType ctx
  where
  typeOp = simpleTyping $ \case
    Add -> TypeSignature [IntType, IntType] [IntType]
    DivMod -> TypeSignature [IntType, IntType] [IntType, IntType]
    Inc -> TypeSignature [IntType] [IntType]
    Double -> TypeSignature [IntType] [IntType]

instance GenIntermediate TestSemanticsObj TestSemanticsType SymInteger where
  genIntermediate _ _ = simpleFresh ()

data TestSemanticsCost = TestSemanticsCost

instance
  (MonadContext ctx) =>
  OpCost TestSemanticsCost TestSemanticsOp SymInteger ctx
  where
  opCost _ Add = return 2
  opCost _ Double = return 1
  opCost _ Inc = return 1
  opCost _ DivMod = return 5

instance (MonadContext ctx) => HasSubProgs TestSemanticsOp NullProg ctx where
  getSubProgs _ = return []

instance
  (ConcreteVarId conVarId) =>
  OpFlatten TestSemanticsOp conVarId TestSemanticsType
  where
  opForwardedSubProg _ = return Nothing
