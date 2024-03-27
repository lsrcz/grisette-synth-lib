{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Grisette.Lib.Synth.Context (ConcreteContext)
import Grisette.Lib.Synth.Operator.OpTyping (OpTyping (typeOp))
import Grisette.Lib.Synth.Program.Concrete
  ( OpPretty (describeArguments, prefixResults, topologicalGPrettySubProg),
    PrefixByType (prefixByType),
    Prog (progArgList, progName, progResList),
    ProgArg (progArgType),
    ProgRes (progResType),
    allPrefixesByTypes,
    topologicalGPrettyProg,
    topologicalProgToDot,
  )
import Grisette.Lib.Synth.Program.Concrete.OpToDot
  ( OpToDot (topologicalSubProgToDot),
  )
import Grisette.Lib.Synth.TypeSignature (TypeSignature (TypeSignature))

data TestPrettyExtOp = TestPrettyExtOp
  deriving (Show, Generic, Eq)
  deriving (Mergeable) via (Default TestPrettyExtOp)

instance GPretty TestPrettyExtOp where
  gpretty TestPrettyExtOp = "ext"

instance OpPretty TestPrettyExtOp where
  describeArguments TestPrettyExtOp = Right [Nothing]

instance OpTyping TestPrettyExtOp TestPrettyType ConcreteContext where
  typeOp TestPrettyExtOp =
    return $ TypeSignature [PrettyType1] [PrettyType1, PrettyType2]

instance OpToDot TestPrettyExtOp where
  topologicalSubProgToDot _ = id

data TestPrettyOp
  = PrettyOp0
  | PrettyOp1
  | PrettyOp2
  | PrettyInvokeOp (Prog TestPrettyOp Int TestPrettyType)
  | PrettyInvokeExtOp (Prog TestPrettyExtOp Int TestPrettyType)
  deriving (Show, Generic, Eq)
  deriving (Mergeable) via (Default TestPrettyOp)

instance GPretty TestPrettyOp where
  gpretty PrettyOp0 = "op0"
  gpretty PrettyOp1 = "op1"
  gpretty PrettyOp2 = "op2"
  gpretty (PrettyInvokeOp prog) = "invoke(" <> gpretty (progName prog) <> ")"
  gpretty (PrettyInvokeExtOp prog) =
    "invoke_ext(" <> gpretty (progName prog) <> ")"

instance OpPretty TestPrettyOp where
  describeArguments PrettyOp0 = Right []
  describeArguments PrettyOp1 = Right [Just "op1"]
  describeArguments PrettyOp2 = Right [Just "op2'2'0'arg", Nothing]
  describeArguments (PrettyInvokeOp prog) =
    Right $ replicate (length $ progArgList prog) Nothing
  describeArguments (PrettyInvokeExtOp prog) =
    Right $ replicate (length $ progArgList prog) Nothing
  prefixResults PrettyOp2 = return ["op2_", "op2'_"]
  prefixResults op = allPrefixesByTypes op
  topologicalGPrettySubProg (PrettyInvokeOp prog) = topologicalGPrettyProg prog
  topologicalGPrettySubProg (PrettyInvokeExtOp prog) =
    topologicalGPrettyProg prog
  topologicalGPrettySubProg _ = id

instance OpToDot TestPrettyOp where
  topologicalSubProgToDot (PrettyInvokeOp prog) = topologicalProgToDot prog
  topologicalSubProgToDot (PrettyInvokeExtOp prog) =
    topologicalProgToDot prog
  topologicalSubProgToDot _ = id

data TestPrettyType = PrettyType1 | PrettyType2
  deriving (Show, Generic, Eq)
  deriving (Mergeable, GPretty) via (Default TestPrettyType)

instance PrefixByType TestPrettyType where
  prefixByType PrettyType1 = "t1_"
  prefixByType PrettyType2 = "t2_"

instance OpTyping TestPrettyOp TestPrettyType ConcreteContext where
  typeOp PrettyOp0 = return $ TypeSignature [] [PrettyType1]
  typeOp PrettyOp1 = return $ TypeSignature [PrettyType1] [PrettyType1]
  typeOp PrettyOp2 =
    return $ TypeSignature [PrettyType1, PrettyType2] [PrettyType2, PrettyType1]
  typeOp (PrettyInvokeOp prog) =
    return $
      TypeSignature
        (progArgType <$> progArgList prog)
        (progResType <$> progResList prog)
  typeOp (PrettyInvokeExtOp prog) =
    return $
      TypeSignature
        (progArgType <$> progArgList prog)
        (progResType <$> progResList prog)
