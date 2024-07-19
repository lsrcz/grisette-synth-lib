{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Grisette (Default (Default), Mergeable, PPrint (pformat), mrgReturn)
import Grisette.Lib.Synth.Context (ConcreteContext, MonadContext)
import Grisette.Lib.Synth.Operator.OpTyping (OpTyping (typeOp))
import Grisette.Lib.Synth.Program.Concrete
  ( OpPPrint (describeArguments, prefixResults),
    PrefixByType (prefixByType),
    Prog (progArgList, progName, progResList),
    ProgArg (progArgType),
    ProgRes (progResType),
    allPrefixesByTypes,
  )
import Grisette.Lib.Synth.Program.Concrete.Flatten
  ( OpFlatten (opForwardedSubProg),
  )
import Grisette.Lib.Synth.Program.NullProg (NullProg)
import Grisette.Lib.Synth.Program.SubProg
  ( HasAnyPathSubProgs (getAnyPathSubProgs),
    HasSubProgs (getSubProgs),
  )
import Grisette.Lib.Synth.Program.SumProg (SumProg (SumProgL, SumProgR))
import Grisette.Lib.Synth.TypeSignature (TypeSignature (TypeSignature))

data TestPrettyExtOp = TestPrettyExtOp
  deriving (Show, Generic, Eq)
  deriving (Mergeable) via (Default TestPrettyExtOp)

instance (MonadContext ctx) => HasSubProgs TestPrettyExtOp NullProg ctx where
  getSubProgs _ = mrgReturn []

instance HasAnyPathSubProgs TestPrettyExtOp NullProg where
  getAnyPathSubProgs _ = []

instance PPrint TestPrettyExtOp where
  pformat TestPrettyExtOp = "ext"

instance OpPPrint TestPrettyExtOp where
  describeArguments TestPrettyExtOp = Right [Nothing]

instance OpTyping TestPrettyExtOp TestPrettyType ConcreteContext where
  typeOp TestPrettyExtOp =
    return $ TypeSignature [PrettyType1] [PrettyType1, PrettyType2]

data TestPrettyOp
  = PrettyOp0
  | PrettyOp1
  | PrettyOp2
  | PrettyOp2NoDescNoPrefix
  | PrettyInvokeOp (Prog TestPrettyOp Int TestPrettyType)
  | PrettyInvokeExtOp (Prog TestPrettyExtOp Int TestPrettyType)
  deriving (Show, Generic, Eq)
  deriving (Mergeable) via (Default TestPrettyOp)

instance
  (MonadContext ctx) =>
  HasSubProgs
    TestPrettyOp
    ( SumProg
        (Prog TestPrettyOp Int TestPrettyType)
        (Prog TestPrettyExtOp Int TestPrettyType)
    )
    ctx
  where
  getSubProgs (PrettyInvokeOp prog) = mrgReturn [SumProgL prog]
  getSubProgs (PrettyInvokeExtOp prog) = mrgReturn [SumProgR prog]
  getSubProgs _ = mrgReturn []

instance
  HasAnyPathSubProgs
    TestPrettyOp
    ( SumProg
        (Prog TestPrettyOp Int TestPrettyType)
        (Prog TestPrettyExtOp Int TestPrettyType)
    )
  where
  getAnyPathSubProgs (PrettyInvokeOp prog) = [SumProgL prog]
  getAnyPathSubProgs (PrettyInvokeExtOp prog) = [SumProgR prog]
  getAnyPathSubProgs _ = []

instance OpFlatten TestPrettyOp Int TestPrettyType where
  opForwardedSubProg (PrettyInvokeOp prog) = return $ Just prog
  opForwardedSubProg _ = return Nothing

instance PPrint TestPrettyOp where
  pformat PrettyOp0 = "op0"
  pformat PrettyOp1 = "op1"
  pformat PrettyOp2 = "op2"
  pformat PrettyOp2NoDescNoPrefix = "op2NoDescNoPrefix"
  pformat (PrettyInvokeOp prog) = "invoke(" <> pformat (progName prog) <> ")"
  pformat (PrettyInvokeExtOp prog) =
    "invoke_ext(" <> pformat (progName prog) <> ")"

instance OpPPrint TestPrettyOp where
  describeArguments PrettyOp0 = Right []
  describeArguments PrettyOp1 = Right [Just "op1"]
  describeArguments PrettyOp2 = Right [Just "op2'2'0'arg", Nothing]
  describeArguments PrettyOp2NoDescNoPrefix = Right []
  describeArguments (PrettyInvokeOp prog) =
    Right $ replicate (length $ progArgList prog) Nothing
  describeArguments (PrettyInvokeExtOp prog) =
    Right $ replicate (length $ progArgList prog) Nothing
  prefixResults PrettyOp2 = return ["op2_", "op2'_"]
  prefixResults PrettyOp2NoDescNoPrefix = return []
  prefixResults op = allPrefixesByTypes op

data TestPrettyType = PrettyType1 | PrettyType2
  deriving (Show, Generic, Eq)
  deriving (Mergeable, PPrint) via (Default TestPrettyType)

instance PrefixByType TestPrettyType where
  prefixByType PrettyType1 = "t1_"
  prefixByType PrettyType2 = "t2_"

instance OpTyping TestPrettyOp TestPrettyType ConcreteContext where
  typeOp PrettyOp0 = return $ TypeSignature [] [PrettyType1]
  typeOp PrettyOp1 = return $ TypeSignature [PrettyType1] [PrettyType1]
  typeOp PrettyOp2 =
    return $ TypeSignature [PrettyType1, PrettyType2] [PrettyType2, PrettyType1]
  typeOp PrettyOp2NoDescNoPrefix =
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
