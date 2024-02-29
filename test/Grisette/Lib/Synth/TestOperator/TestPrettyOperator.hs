{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.TestOperator.TestPrettyOperator
  ( TestPrettyOp (..),
    TestPrettyExtOp (..),
    TestPrettyType (..),
  )
where

import GHC.Generics (Generic)
import Grisette (Default (Default), GPretty, Mergeable)
import Grisette.Core.Data.Class.GPretty (GPretty (gpretty))
import Grisette.Lib.Synth.Program.Concrete
  ( OpDirectSubProgs (opDirectSubProgs),
    OpPretty (describeArguments, prefixResults),
    OpPrettyError (IncorrectNumberOfArguments, IncorrectNumberOfResults),
    Prog (progArgList, progName, progResList),
    SomePrettyProg (SomePrettyProg),
  )

data TestPrettyExtOp = TestPrettyExtOp
  deriving (Show, Generic, Eq)
  deriving (Mergeable) via (Default TestPrettyExtOp)

instance GPretty TestPrettyExtOp where
  gpretty TestPrettyExtOp = "ext"

instance OpPretty TestPrettyExtOp where
  describeArguments TestPrettyExtOp n = Right $ replicate n Nothing
  prefixResults TestPrettyExtOp n m
    | n == m = Right $ replicate n "o"
    | otherwise = Left $ IncorrectNumberOfResults TestPrettyExtOp n m

data TestPrettyOp
  = PrettyOp1
  | PrettyOp2
  | PrettyInvokeOp (Prog TestPrettyOp Int TestPrettyType)
  | PrettyInvokeExtOp (Prog TestPrettyExtOp Int TestPrettyType)
  deriving (Show, Generic, Eq)
  deriving (Mergeable) via (Default TestPrettyOp)

instance GPretty TestPrettyOp where
  gpretty PrettyOp1 = "op1"
  gpretty PrettyOp2 = "op2"
  gpretty (PrettyInvokeOp prog) = "invoke(" <> gpretty (progName prog) <> ")"
  gpretty (PrettyInvokeExtOp prog) =
    "invoke_ext(" <> gpretty (progName prog) <> ")"

instance OpPretty TestPrettyOp where
  describeArguments PrettyOp1 1 = Right [Just "op1"]
  describeArguments PrettyOp1 n =
    Left $ IncorrectNumberOfArguments PrettyOp1 n
  describeArguments PrettyOp2 0 = Right []
  describeArguments PrettyOp2 1 = Right [Nothing]
  describeArguments PrettyOp2 2 = Right [Just "op2'2'0'arg", Nothing]
  describeArguments PrettyOp2 n =
    Left $ IncorrectNumberOfArguments PrettyOp2 n
  describeArguments op@(PrettyInvokeOp prog) n
    | n == length (progArgList prog) = Right $ replicate n Nothing
    | otherwise = Left $ IncorrectNumberOfArguments op n
  describeArguments op@(PrettyInvokeExtOp prog) n
    | n == length (progArgList prog) = Right $ replicate n Nothing
    | otherwise = Left $ IncorrectNumberOfArguments op n
  prefixResults PrettyOp1 1 1 = Right ["op1res"]
  prefixResults PrettyOp1 numArguments numResults =
    Left $ IncorrectNumberOfResults PrettyOp1 numArguments numResults
  prefixResults PrettyOp2 0 0 = Right []
  prefixResults PrettyOp2 1 1 = Right ["op2'1'res"]
  prefixResults PrettyOp2 2 2 = Right ["op2'2'0'res", "op2'2'1'res"]
  prefixResults PrettyOp2 numArguments numResults =
    Left $ IncorrectNumberOfResults PrettyOp2 numArguments numResults
  prefixResults op@(PrettyInvokeOp prog) numArguments numResults
    | numArguments == length (progArgList prog)
        && numResults == length (progResList prog) =
        Right (replicate numResults "o")
    | otherwise =
        Left $ IncorrectNumberOfResults op numArguments numResults
  prefixResults op@(PrettyInvokeExtOp prog) numArguments numResults
    | numArguments == length (progArgList prog)
        && numResults == length (progResList prog) =
        Right (replicate numResults "o")
    | otherwise =
        Left $ IncorrectNumberOfResults op numArguments numResults

data TestPrettyType = PrettyType1 | PrettyType2
  deriving (Show, Generic, Eq)
  deriving (Mergeable, GPretty) via (Default TestPrettyType)

instance OpDirectSubProgs TestPrettyExtOp SomePrettyProg where
  opDirectSubProgs _ = []

instance OpDirectSubProgs TestPrettyOp SomePrettyProg where
  opDirectSubProgs (PrettyInvokeOp prog) = [SomePrettyProg prog]
  opDirectSubProgs (PrettyInvokeExtOp prog) = [SomePrettyProg prog]
  opDirectSubProgs _ = []
