{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (..),
    TestSemanticsObj (..),
    TestSemanticsType (..),
  )
where

import Control.Exception (ArithException)
import Control.Monad (when)
import Control.Monad.Except (runExceptT)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvaluateSym,
    GenSymSimple (simpleFresh),
    Mergeable,
    MonadFresh,
    MonadUnion,
    SafeDivision (safeDivMod),
    SymInteger,
    ToCon,
    liftToMonadUnion,
  )
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Program.ComponentSketch
  ( GenIntermediate (genIntermediate),
    OpTypingByInputTypes,
    OpTypingByNumInputs (typeOpByNumInputs),
    OpTypingSimple (typeOpSimple),
  )
import Grisette.Lib.Synth.Program.ComponentSketch.OpTyping (OpTypingByInputTypes (typeOpByInputTypes))
import Grisette.Lib.Synth.TypeSignature
  ( TypeSignature (TypeSignature),
  )
import Grisette.Lib.Synth.Util.Show (showText)

data TestSemanticsOp = Add | DivMod | Inc | Double
  deriving (Show, Generic, Eq)
  deriving anyclass (Hashable)
  deriving
    (Mergeable, ToCon TestSemanticsOp, EvaluateSym)
    via (Default TestSemanticsOp)

data TestSemanticsObj = TestSemanticsObj

data TestSemanticsType = IntType
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable)
  deriving (Mergeable, EvaluateSym) via (Default TestSemanticsType)

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
  OpTypingSimple
    TestSemanticsObj
    TestSemanticsOp
    TestSemanticsType
  where
  typeOpSimple _ Add =
    mrgReturn $ TypeSignature [IntType, IntType] [IntType]
  typeOpSimple _ DivMod =
    mrgReturn $ TypeSignature [IntType, IntType] [IntType, IntType]
  typeOpSimple _ Inc = mrgReturn $ TypeSignature [IntType] [IntType]
  typeOpSimple _ Double = mrgReturn $ TypeSignature [IntType] [IntType]

instance
  OpTypingByNumInputs
    TestSemanticsObj
    TestSemanticsOp
    TestSemanticsType
  where
  typeOpByNumInputs _ Add 2 =
    mrgReturn $ TypeSignature [IntType, IntType] [IntType]
  typeOpByNumInputs _ Add l =
    mrgThrowError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showText l
        <> " arguments."
  typeOpByNumInputs _ DivMod 2 =
    mrgReturn $ TypeSignature [IntType, IntType] [IntType, IntType]
  typeOpByNumInputs _ DivMod l =
    mrgThrowError $
      "Incorrect number of arguments for divmod, expected 2 arguments, but got "
        <> showText l
        <> " arguments."
  typeOpByNumInputs _ Inc 1 = mrgReturn $ TypeSignature [IntType] [IntType]
  typeOpByNumInputs _ Inc l =
    mrgThrowError $
      "Incorrect number of arguments for inc, expected 1 arguments, but got "
        <> showText l
        <> " arguments."
  typeOpByNumInputs _ Double 1 = mrgReturn $ TypeSignature [IntType] [IntType]
  typeOpByNumInputs _ Double l =
    mrgThrowError $
      "Incorrect number of arguments for dec, expected 1 arguments, but got "
        <> showText l
        <> " arguments."

instance
  OpTypingByInputTypes
    TestSemanticsObj
    TestSemanticsOp
    TestSemanticsType
  where
  typeOpByInputTypes _ Add [IntType, IntType] =
    mrgReturn $ TypeSignature [IntType, IntType] [IntType]
  typeOpByInputTypes _ Add ty =
    mrgThrowError $
      "The operator add cannot be applied to types "
        <> showText ty
        <> ", expected [IntType, IntType]."
  typeOpByInputTypes _ DivMod [IntType, IntType] =
    mrgReturn $ TypeSignature [IntType, IntType] [IntType, IntType]
  typeOpByInputTypes _ DivMod ty =
    mrgThrowError $
      "The operator divmod cannot be applied to types "
        <> showText ty
        <> ", expected [IntType, IntType]."
  typeOpByInputTypes _ Inc [IntType] =
    mrgReturn $ TypeSignature [IntType] [IntType]
  typeOpByInputTypes _ Inc ty =
    mrgThrowError $
      "The operator inc cannot be applied to types "
        <> showText ty
        <> ", expected [IntType]."
  typeOpByInputTypes _ Double [IntType] =
    mrgReturn $ TypeSignature [IntType] [IntType]
  typeOpByInputTypes _ Double ty =
    mrgThrowError $
      "The operator double cannot be applied to types "
        <> showText ty
        <> ", expected [IntType]."

instance
  (MonadContext ctx, MonadFresh ctx) =>
  GenIntermediate TestSemanticsObj TestSemanticsType SymInteger ctx
  where
  genIntermediate _ IntType = simpleFresh ()
