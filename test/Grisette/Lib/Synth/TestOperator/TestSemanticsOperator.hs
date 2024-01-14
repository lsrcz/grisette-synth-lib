{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (..),
    TestSemanticsObj (..),
    TestSemanticsType (..),
  )
where

import Control.Monad (when)
import GHC.Generics (Generic)
import Grisette (Default (Default), Mergeable)
import Grisette.Lib.Synth.Context (MonadContext (raiseError))
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Util.Show (showText)

data TestSemanticsOp = Add | DivMod
  deriving (Show, Generic, Eq)
  deriving (Mergeable) via (Default TestSemanticsOp)

data TestSemanticsObj = TestSemanticsObj

data TestSemanticsType = IntType

instance
  (MonadContext ctx) =>
  OpSemantics TestSemanticsObj TestSemanticsOp Int ctx
  where
  applyOp _ Add [x, y] = return [x + y]
  applyOp _ Add l =
    raiseError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showText (length l)
        <> " arguments."
  applyOp _ DivMod [x, y] = do
    when (y == 0) $ raiseError "Division by zero"
    return [x `div` y, x `mod` y]
  applyOp _ DivMod l =
    raiseError $
      "Incorrect number of arguments for add, expected 2 arguments, but got "
        <> showText (length l)
        <> " arguments."
