{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.TestOperator.TestPrettyOperator
  ( TestPrettyOp (..),
    TestPrettySem (..),
    TestPrettyType (..),
  )
where

import GHC.Generics (Generic)
import Grisette (Default (Default), GPretty, Mergeable)
import Grisette.Core.Data.Class.GPretty (GPretty (gpretty))
import Grisette.Lib.Synth.Operator.OpPretty
  ( OpPretty (describeArguments, prefixResults),
    OpPrettyError (IncorrectNumberOfArguments, IncorrectNumberOfResults),
  )

data TestPrettySem = PrettySem

data TestPrettyOp = PrettyOp1 | PrettyOp2
  deriving (Show, Generic, Eq)
  deriving (Mergeable) via (Default TestPrettyOp)

instance GPretty TestPrettyOp where
  gpretty PrettyOp1 = "op1"
  gpretty PrettyOp2 = "op2"

instance OpPretty TestPrettySem TestPrettyOp where
  describeArguments _ PrettyOp1 1 = Right [Just "op1"]
  describeArguments _ PrettyOp1 n =
    Left $ IncorrectNumberOfArguments PrettyOp1 n
  describeArguments _ PrettyOp2 0 = Right []
  describeArguments _ PrettyOp2 1 = Right [Nothing]
  describeArguments _ PrettyOp2 2 = Right [Just "op2'2'0'arg", Nothing]
  describeArguments _ PrettyOp2 n =
    Left $ IncorrectNumberOfArguments PrettyOp2 n
  prefixResults _ PrettyOp1 1 1 = Right ["op1res"]
  prefixResults _ PrettyOp1 numArguments numResults =
    Left $ IncorrectNumberOfResults PrettyOp1 numArguments numResults
  prefixResults _ PrettyOp2 0 0 = Right []
  prefixResults _ PrettyOp2 1 1 = Right ["op2'1'res"]
  prefixResults _ PrettyOp2 2 2 = Right ["op2'2'0'res", "op2'2'1'res"]
  prefixResults _ PrettyOp2 numArguments numResults =
    Left $ IncorrectNumberOfResults PrettyOp2 numArguments numResults

data TestPrettyType = PrettyType1 | PrettyType2
  deriving (Show, Generic, Eq)
  deriving (Mergeable, GPretty) via (Default TestPrettyType)
