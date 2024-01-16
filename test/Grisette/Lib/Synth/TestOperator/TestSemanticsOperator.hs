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

import Control.Monad (when)
import Control.Monad.Except (runExceptT)
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    GenSymSimple (simpleFresh),
    Mergeable,
    MonadFresh,
    MonadUnion,
    SafeDivision (safeDivMod),
    SymInteger,
    ToCon,
    liftToMonadUnion,
  )
import Grisette.Lib.Synth.Context (MonadContext (raiseError, result))
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Operator.OpTyping
  ( GenIntermediate (genIntermediate),
    OpTyping (typeOp),
  )
import Grisette.Lib.Synth.Util.Show (showText)

data TestSemanticsOp = Add | DivMod | Inc | Double
  deriving (Show, Generic, Eq)
  deriving (Mergeable, ToCon TestSemanticsOp) via (Default TestSemanticsOp)

data TestSemanticsObj = TestSemanticsObj

data TestSemanticsType = IntType
  deriving (Show, Eq, Generic)
  deriving (Mergeable) via (Default TestSemanticsType)

instance
  (MonadContext ctx) =>
  OpSemantics TestSemanticsObj TestSemanticsOp Integer ctx
  where
  applyOp _ Add [x, y] = return [x + y]
  applyOp _ Add l =
    raiseError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showText (length l)
        <> " arguments."
  applyOp _ DivMod [x, y] = do
    when (y == 0) $ raiseError "ArithException: divide by zero"
    return [x `div` y, x `mod` y]
  applyOp _ DivMod l =
    raiseError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showText (length l)
        <> " arguments."
  applyOp _ Inc [x] = return [x + 1]
  applyOp _ Inc l =
    raiseError $
      "Incorrect number of arguments for inc, expected 1 arguments, but got "
        <> showText (length l)
        <> " arguments."
  applyOp _ Double [x] = return [x + x]
  applyOp _ Double l =
    raiseError $
      "Incorrect number of arguments for dec, expected 1 arguments, but got "
        <> showText (length l)
        <> " arguments."

instance
  (MonadContext ctx, MonadUnion ctx) =>
  OpSemantics TestSemanticsObj TestSemanticsOp SymInteger ctx
  where
  applyOp _ Add [x, y] = return [x + y]
  applyOp _ Add l =
    raiseError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showText (length l)
        <> " arguments."
  applyOp _ DivMod [x, y] = do
    r <- liftToMonadUnion $ runExceptT $ safeDivMod x y
    case r of
      Left e -> raiseError $ "ArithException: " <> showText e
      Right (d, m) -> result [d, m]
  applyOp _ DivMod l =
    raiseError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showText (length l)
        <> " arguments."
  applyOp _ Inc [x] = return [x + 1]
  applyOp _ Inc l =
    raiseError $
      "Incorrect number of arguments for inc, expected 1 arguments, but got "
        <> showText (length l)
        <> " arguments."
  applyOp _ Double [x] = return [x + x]
  applyOp _ Double l =
    raiseError $
      "Incorrect number of arguments for dec, expected 1 arguments, but got "
        <> showText (length l)
        <> " arguments."

instance
  (MonadContext ctx) =>
  OpTyping TestSemanticsObj TestSemanticsOp TestSemanticsType ctx
  where
  typeOp _ Add 2 = result ([IntType, IntType], [IntType])
  typeOp _ Add l =
    raiseError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showText l
        <> " arguments."
  typeOp _ DivMod 2 = result ([IntType, IntType], [IntType, IntType])
  typeOp _ DivMod l =
    raiseError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showText l
        <> " arguments."
  typeOp _ Inc 1 = result ([IntType], [IntType])
  typeOp _ Inc l =
    raiseError $
      "Incorrect number of arguments for inc, expected 1 arguments, but got "
        <> showText l
        <> " arguments."
  typeOp _ Double 1 = result ([IntType], [IntType])
  typeOp _ Double l =
    raiseError $
      "Incorrect number of arguments for dec, expected 1 arguments, but got "
        <> showText l
        <> " arguments."

instance
  (MonadContext ctx, MonadFresh ctx) =>
  GenIntermediate TestSemanticsObj TestSemanticsType SymInteger ctx
  where
  genIntermediate _ IntType = simpleFresh ()
