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

import Control.Monad.Error.Class (MonadError (throwError))
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette (Default (Default), Mergeable, PPrint (pformat))
import Grisette.Lib.Synth.Context (ConcreteContext)
import Grisette.Lib.Synth.Operator.OpTyping (OpTyping (OpTypeType, typeOp))
import Grisette.Lib.Synth.Program.Concrete
  ( OpPPrint (describeArguments, prefixResults),
    OpPPrintError (PPrintTypingError),
    PrefixByType (prefixByType),
    allPrefixesByTypes,
  )
import Grisette.Lib.Synth.Program.Concrete.Flatten (OpFlatten (opForwardedSubProg))
import Grisette.Lib.Synth.Program.ProgTyping (lookupType)
import Grisette.Lib.Synth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes),
  )

data TestPrettyExtOp = TestPrettyExtOp
  deriving (Show, Generic, Eq)
  deriving (Mergeable) via (Default TestPrettyExtOp)

instance PPrint TestPrettyExtOp where
  pformat TestPrettyExtOp = "ext"

instance OpPPrint TestPrettyExtOp where
  describeArguments _ TestPrettyExtOp = Right [Nothing]

instance OpTyping TestPrettyExtOp ConcreteContext where
  type OpTypeType TestPrettyExtOp = TestPrettyType
  typeOp _ TestPrettyExtOp =
    return $ TypeSignature [PrettyType1] [PrettyType1, PrettyType2]

data TestPrettyOp
  = PrettyOp0
  | PrettyOp1
  | PrettyOp2
  | PrettyOp2NoDescNoPrefix
  | PrettyInvokeOp T.Text
  | PrettyInvokeExtOp T.Text
  deriving (Show, Generic, Eq)
  deriving (Mergeable) via (Default TestPrettyOp)

instance OpFlatten TestPrettyOp TestPrettyOp where
  opForwardedSubProg (PrettyInvokeOp prog) = return $ Left prog
  opForwardedSubProg (PrettyInvokeExtOp prog) = return $ Left prog
  opForwardedSubProg r = return $ Right r

instance PPrint TestPrettyOp where
  pformat PrettyOp0 = "op0"
  pformat PrettyOp1 = "op1"
  pformat PrettyOp2 = "op2"
  pformat PrettyOp2NoDescNoPrefix = "op2NoDescNoPrefix"
  pformat (PrettyInvokeOp prog) = "invoke(" <> pformat prog <> ")"
  pformat (PrettyInvokeExtOp prog) =
    "invoke_ext(" <> pformat prog <> ")"

instance OpPPrint TestPrettyOp where
  describeArguments _ PrettyOp0 = Right []
  describeArguments _ PrettyOp1 = Right [Just "op1"]
  describeArguments _ PrettyOp2 = Right [Just "op2'2'0'arg", Nothing]
  describeArguments _ PrettyOp2NoDescNoPrefix = Right []
  describeArguments table op@(PrettyInvokeOp prog) = do
    tys <-
      either (throwError . PPrintTypingError op) (return . argTypes) $
        lookupType table prog
    Right $ Nothing <$ tys
  describeArguments table op@(PrettyInvokeExtOp prog) = do
    tys <-
      either (throwError . PPrintTypingError op) (return . argTypes) $
        lookupType table prog
    Right $ Nothing <$ tys
  prefixResults _ PrettyOp2 = return ["op2_", "op2'_"]
  prefixResults _ PrettyOp2NoDescNoPrefix = return []
  prefixResults table op = allPrefixesByTypes table op

data TestPrettyType = PrettyType1 | PrettyType2
  deriving (Show, Generic, Eq)
  deriving (Mergeable, PPrint) via (Default TestPrettyType)

instance PrefixByType TestPrettyType where
  prefixByType PrettyType1 = "t1_"
  prefixByType PrettyType2 = "t2_"

instance OpTyping TestPrettyOp ConcreteContext where
  type OpTypeType TestPrettyOp = TestPrettyType
  typeOp _ PrettyOp0 = return $ TypeSignature [] [PrettyType1]
  typeOp _ PrettyOp1 = return $ TypeSignature [PrettyType1] [PrettyType1]
  typeOp _ PrettyOp2 =
    return $ TypeSignature [PrettyType1, PrettyType2] [PrettyType2, PrettyType1]
  typeOp _ PrettyOp2NoDescNoPrefix =
    return $ TypeSignature [PrettyType1, PrettyType2] [PrettyType2, PrettyType1]
  typeOp table (PrettyInvokeOp prog) = lookupType table prog
  typeOp table (PrettyInvokeExtOp prog) = lookupType table prog
