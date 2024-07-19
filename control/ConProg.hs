{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module ConProg (Op (..), Prog) where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Grisette (PPrint (pformat), mrgReturn)
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Operator.OpSemantics (DefaultSem, OpSemantics (applyOp))
import Grisette.Lib.Synth.Operator.OpTyping
  ( OpTyping (typeOp),
  )
import Grisette.Lib.Synth.Program.Concrete
  ( OpPPrint (describeArguments),
    PrefixByType (prefixByType),
  )
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.ProgNaming (ProgNaming (nameProg))
import Grisette.Lib.Synth.Program.SubProg
  ( HasAnyPathSubProgs (getAnyPathSubProgs),
    HasSubProgs (getSubProgs),
  )
import Grisette.Lib.Synth.Util.Pretty (parenCommaList)
import Grisette.Lib.Synth.VarId (ConcreteVarId)
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

data Op varId intVal
  = Plus
  | Equals
  | Minus
  | IntConst intVal
  | If (Prog varId intVal) (Prog varId intVal)
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable)

instance
  (MonadContext ctx) =>
  HasSubProgs (Op varId intVal) (Prog varId intVal) ctx
  where
  getSubProgs (If true false) = mrgReturn [true, false]
  getSubProgs _ = mrgReturn []

instance HasAnyPathSubProgs (Op varId intVal) (Prog varId intVal) where
  getAnyPathSubProgs (If true false) = [true, false]
  getAnyPathSubProgs _ = []

type Prog varId intVal = Concrete.Prog (Op varId intVal) varId Type

instance (PPrint intVal) => PPrint (Op varId intVal) where
  pformat Plus = "plus"
  pformat Equals = "equals"
  pformat Minus = "Minus"
  pformat (IntConst n) = "const" <> parenCommaList [pformat n]
  pformat (If true false) =
    "if" <> parenCommaList (pformat <$> [nameProg true, nameProg false])

instance
  (PPrint intVal, ConcreteVarId varId, Show intVal) =>
  OpPPrint (Op varId intVal)
  where
  describeArguments Plus = return $ replicate 2 Nothing
  describeArguments Equals = return $ replicate 2 Nothing
  describeArguments Minus = return $ replicate 2 Nothing
  describeArguments (IntConst _) = return []
  describeArguments (If true _) =
    return $ Just "cond" : (Nothing <$ Concrete.progArgList true)

instance PrefixByType Type where
  prefixByType _ = "r"

instance (MonadContext ctx) => OpTyping (Op varId intVal) Type ctx where
  typeOp Plus = typePlus
  typeOp Equals = typeEquals
  typeOp Minus = typeMinus
  typeOp IntConst {} = typeIntConst
  typeOp (If true false) = typeIf true false

instance
  ( HasSemantics (Value intVal boolVal) ctx,
    ConcreteVarId varId
  ) =>
  OpSemantics DefaultSem (Op varId intVal) (Value intVal boolVal) ctx
  where
  applyOp _ Plus = applyPlus
  applyOp _ Equals = applyEquals
  applyOp _ Minus = applyMinus
  applyOp _ (IntConst c) = applyIntConst c
  applyOp sem (If true false) = applyIf sem true false
