{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
    LogicalOp (false),
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
  ( OpTyping (OpTypeType, typeOp),
    simpleTyping,
  )
import Grisette.Lib.Synth.Program.ComponentSketch
  ( GenIntermediate (genIntermediate),
  )
import Grisette.Lib.Synth.Program.ComponentSketch.SymmetryReduction
  ( OpSymmetryReduction (opCommutativeArgPos, opUnreorderable),
  )
import Grisette.Lib.Synth.Program.Concrete.Flatten
  ( OpFlatten (opForwardedSubProg),
  )
import Grisette.Lib.Synth.Program.CostModel.PerStmtCostModel (OpCost (opCost))
import Grisette.Lib.Synth.TypeSignature
  ( TypeSignature (TypeSignature),
  )
import Grisette.Lib.Synth.Util.Show (showText)

data TestSemanticsOp = Add | DivMod | Inc | Double
  deriving (Show, Generic, Eq)
  deriving anyclass (Hashable)
  deriving
    (Mergeable, ToCon TestSemanticsOp, EvalSym, ToSym TestSemanticsOp)
    via (Default TestSemanticsOp)

instance OpSymmetryReduction TestSemanticsOp where
  opUnreorderable _ _ = false
  opCommutativeArgPos Add = mrgReturn [[0, 1]]
  opCommutativeArgPos _ = mrgReturn []

instance OpFlatten TestSemanticsOp TestSemanticsOp where
  opForwardedSubProg op = return $ Right op

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
  applyOp _ _ Add [x, y] = return [x + y]
  applyOp _ _ Add l =
    mrgThrowError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showText (length l)
        <> " arguments."
  applyOp _ _ DivMod [x, y] = do
    when (y == 0) $ mrgThrowError "ArithException: divide by zero"
    return [x `div` y, x `mod` y]
  applyOp _ _ DivMod l =
    mrgThrowError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showText (length l)
        <> " arguments."
  applyOp _ _ Inc [x] = return [x + 1]
  applyOp _ _ Inc l =
    mrgThrowError $
      "Incorrect number of arguments for inc, expected 1 arguments, but got "
        <> showText (length l)
        <> " arguments."
  applyOp _ _ Double [x] = return [x + x]
  applyOp _ _ Double l =
    mrgThrowError $
      "Incorrect number of arguments for dec, expected 1 arguments, but got "
        <> showText (length l)
        <> " arguments."

instance
  (MonadContext ctx, MonadUnion ctx) =>
  OpSemantics TestSemanticsObj TestSemanticsOp SymInteger ctx
  where
  applyOp _ _ Add [x, y] = return [x + y]
  applyOp _ _ Add l =
    mrgThrowError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showText (length l)
        <> " arguments."
  applyOp _ _ DivMod [x, y] = do
    r <- liftToMonadUnion $ runExceptT $ safeDivMod x y
    case r of
      Left (e :: ArithException) -> mrgThrowError $ "ArithException: " <> showText e
      Right (d, m) -> mrgReturn [d, m]
  applyOp _ _ DivMod l =
    mrgThrowError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showText (length l)
        <> " arguments."
  applyOp _ _ Inc [x] = return [x + 1]
  applyOp _ _ Inc l =
    mrgThrowError $
      "Incorrect number of arguments for inc, expected 1 arguments, but got "
        <> showText (length l)
        <> " arguments."
  applyOp _ _ Double [x] = return [x + x]
  applyOp _ _ Double l =
    mrgThrowError $
      "Incorrect number of arguments for dec, expected 1 arguments, but got "
        <> showText (length l)
        <> " arguments."

instance (MonadContext ctx) => OpTyping TestSemanticsOp ctx where
  type OpTypeType TestSemanticsOp = TestSemanticsType
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
  opCost _ _ Add = return 2
  opCost _ _ Double = return 1
  opCost _ _ Inc = return 1
  opCost _ _ DivMod = return 5
