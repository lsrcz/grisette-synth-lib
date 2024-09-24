{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Grisette.Lib.Synth.TestOperator.TestPrettyOperator
  ( TestPrettyOp (..),
    TestPrettyExtOp (..),
    TestPrettyType (..),
  )
where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette (Default (Default), Mergeable, PPrint (pformat))
import Grisette.Lib.Synth.Context (ConcreteContext)
import Grisette.Lib.Synth.Operator.OpTyping (OpTyping (OpTypeType, typeOp))
import Grisette.Lib.Synth.Program.Concrete
  ( OpPPrint (describeArguments, prefixResults),
    PrefixByType (prefixByType),
    allPrefixesByTypes,
  )
import Grisette.Lib.Synth.Program.Concrete.Flatten
  ( OpFlatten (opForwardedSubProg),
  )
import Grisette.Lib.Synth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes),
  )

data TestPrettyExtOp = TestPrettyExtOp
  deriving (Show, Generic, Eq)
  deriving (Mergeable) via (Default TestPrettyExtOp)

instance PPrint TestPrettyExtOp where
  pformat TestPrettyExtOp = "ext"

instance OpPPrint TestPrettyExtOp where
  describeArguments TestPrettyExtOp = Right [Nothing]

instance OpTyping TestPrettyExtOp ConcreteContext where
  type OpTypeType TestPrettyExtOp = TestPrettyType
  typeOp TestPrettyExtOp =
    return $ TypeSignature [PrettyType1] [PrettyType1, PrettyType2]

data TestPrettyOp
  = PrettyOp0
  | PrettyOp1
  | PrettyOp2
  | PrettyOp2NoDescNoPrefix
  | PrettyInvokeOp (TypeSignature TestPrettyType) T.Text
  | PrettyInvokeExtOp (TypeSignature TestPrettyType) T.Text
  deriving (Show, Generic, Eq)
  deriving (Mergeable) via (Default TestPrettyOp)

instance OpFlatten TestPrettyOp TestPrettyOp where
  opForwardedSubProg (PrettyInvokeOp _ prog) = return $ Left prog
  opForwardedSubProg (PrettyInvokeExtOp _ prog) = return $ Left prog
  opForwardedSubProg r = return $ Right r

instance PPrint TestPrettyOp where
  pformat PrettyOp0 = "op0"
  pformat PrettyOp1 = "op1"
  pformat PrettyOp2 = "op2"
  pformat PrettyOp2NoDescNoPrefix = "op2NoDescNoPrefix"
  pformat (PrettyInvokeOp _ prog) = "invoke(" <> pformat prog <> ")"
  pformat (PrettyInvokeExtOp _ prog) =
    "invoke_ext(" <> pformat prog <> ")"

instance OpPPrint TestPrettyOp where
  describeArguments PrettyOp0 = Right []
  describeArguments PrettyOp1 = Right [Just "op1"]
  describeArguments PrettyOp2 = Right [Just "op2'2'0'arg", Nothing]
  describeArguments PrettyOp2NoDescNoPrefix = Right []
  describeArguments (PrettyInvokeOp sig _) = return $ Nothing <$ argTypes sig
  describeArguments (PrettyInvokeExtOp sig _) =
    return $ Nothing <$ argTypes sig
  prefixResults PrettyOp2 = return ["op2_", "op2'_"]
  prefixResults PrettyOp2NoDescNoPrefix = return []
  prefixResults op = allPrefixesByTypes op

data TestPrettyType = PrettyType1 | PrettyType2
  deriving (Show, Generic, Eq)
  deriving (Mergeable, PPrint) via (Default TestPrettyType)

instance PrefixByType TestPrettyType where
  prefixByType PrettyType1 = "t1_"
  prefixByType PrettyType2 = "t2_"

instance OpTyping TestPrettyOp ConcreteContext where
  type OpTypeType TestPrettyOp = TestPrettyType
  typeOp PrettyOp0 = return $ TypeSignature [] [PrettyType1]
  typeOp PrettyOp1 = return $ TypeSignature [PrettyType1] [PrettyType1]
  typeOp PrettyOp2 =
    return $ TypeSignature [PrettyType1, PrettyType2] [PrettyType2, PrettyType1]
  typeOp PrettyOp2NoDescNoPrefix =
    return $ TypeSignature [PrettyType1, PrettyType2] [PrettyType2, PrettyType1]
  typeOp (PrettyInvokeOp sig _) = return sig
  typeOp (PrettyInvokeExtOp sig _) = return sig
