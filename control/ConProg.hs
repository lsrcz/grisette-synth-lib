{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ConProg (Op (..), Prog) where

import Data.Hashable (Hashable)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette (PPrint (pformat))
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Operator.OpSemantics (DefaultSem, OpSemantics (applyOp))
import Grisette.Lib.Synth.Operator.OpTyping
  ( OpTyping (OpTypeType, typeOp),
  )
import Grisette.Lib.Synth.Program.Concrete
  ( OpPPrint (describeArguments),
    PrefixByType (prefixByType),
  )
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
-- applyIf,

import Grisette.Lib.Synth.Program.ProgTyping (lookupType)
import Grisette.Lib.Synth.TypeSignature (TypeSignature (TypeSignature))
import Grisette.Lib.Synth.Util.Pretty (parenCommaList)
import Semantics
  ( HasSemantics,
    applyEquals,
    applyIf,
    applyIntConst,
    applyMinus,
    applyPlus,
  )
import Typing (Type, typeEquals, typeIf, typeIntConst, typeMinus, typePlus)
import Value (Value)

data Op intVal
  = Plus
  | Equals
  | Minus
  | IntConst intVal
  | If T.Text T.Text
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable)

type Prog varId intVal = Concrete.Prog (Op intVal) varId Type

instance (PPrint intVal) => PPrint (Op intVal) where
  pformat Plus = "plus"
  pformat Equals = "equals"
  pformat Minus = "Minus"
  pformat (IntConst n) = "const" <> parenCommaList [pformat n]
  pformat (If true false) =
    "if" <> parenCommaList (pformat <$> [true, false])

instance
  (PPrint intVal, Show intVal) =>
  OpPPrint (Op intVal)
  where
  describeArguments _ Plus = return $ replicate 2 Nothing
  describeArguments _ Equals = return $ replicate 2 Nothing
  describeArguments _ Minus = return $ replicate 2 Nothing
  describeArguments _ (IntConst _) = return []
  describeArguments table (If true _) =
    case lookupType table true of
      Left err -> error $ T.unpack err
      Right (TypeSignature arg _) -> return $ Just "cond" : (Nothing <$ arg)

instance PrefixByType Type where
  prefixByType _ = "r"

instance (MonadContext ctx) => OpTyping (Op intVal) ctx where
  type OpTypeType (Op intVal) = Type
  typeOp _ Plus = typePlus
  typeOp _ Equals = typeEquals
  typeOp _ Minus = typeMinus
  typeOp _ IntConst {} = typeIntConst
  typeOp table (If true false) = typeIf table true false

instance
  ( HasSemantics (Value intVal boolVal) ctx
  ) =>
  OpSemantics DefaultSem (Op intVal) (Value intVal boolVal) ctx
  where
  applyOp _ _ _ Plus = applyPlus
  applyOp _ _ _ Equals = applyEquals
  applyOp _ _ _ Minus = applyMinus
  applyOp _ _ _ (IntConst c) = applyIntConst c
  applyOp _ table _ (If true false) = applyIf table true false
