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
    Mergeable,
    MonadUnion,
    SafeDivision (safeDivMod),
    SymInteger,
    liftToMonadUnion,
  )
import Grisette.Lib.Synth.Context (MonadContext (raiseError, result))
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Util.Show (showText)

data TestSemanticsOp = Add | DivMod
  deriving (Show, Generic, Eq)
  deriving (Mergeable) via (Default TestSemanticsOp)

data TestSemanticsObj = TestSemanticsObj

data TestSemanticsType = IntType

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
