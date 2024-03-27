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
import Grisette
  ( GPretty (gpretty),
  )
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Operator.OpTyping
  ( OpTyping (typeOp),
  )
import Grisette.Lib.Synth.Program.Concrete
  ( OpPretty (describeArguments),
    PrefixByType (prefixByType),
    topologicalGPrettyProg,
    topologicalProgToDot,
  )
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.Concrete.OpToDot (OpToDot (topologicalOpToDotSubProg))
import Grisette.Lib.Synth.Program.ProgNaming (ProgNaming (nameProg))
import Grisette.Lib.Synth.Util.Pretty (parenCommaList)
import Grisette.Lib.Synth.VarId (ConcreteVarId)
import Semantics
  ( HasSemantics,
    Sem,
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

type Prog varId intVal = Concrete.Prog (Op varId intVal) varId Type

instance (GPretty intVal) => GPretty (Op varId intVal) where
  gpretty Plus = "plus"
  gpretty Equals = "equals"
  gpretty Minus = "Minus"
  gpretty (IntConst n) = "const" <> parenCommaList [gpretty n]
  gpretty (If true false) =
    "if" <> parenCommaList (gpretty <$> [nameProg true, nameProg false])

instance
  (GPretty intVal, ConcreteVarId varId, Show intVal) =>
  OpPretty (Op varId intVal)
  where
  describeArguments Plus = return $ replicate 2 Nothing
  describeArguments Equals = return $ replicate 2 Nothing
  describeArguments Minus = return $ replicate 2 Nothing
  describeArguments (IntConst _) = return []
  describeArguments (If true _) =
    return $ Just "cond" : (Nothing <$ Concrete.progArgList true)
  topologicalGPrettySubProg (If true false) =
    topologicalGPrettyProg true . topologicalGPrettyProg false
  topologicalGPrettySubProg _ = id

instance
  (GPretty intVal, ConcreteVarId varId, Show intVal) =>
  OpToDot (Op varId intVal)
  where
  topologicalOpToDotSubProg (If true false) =
    topologicalProgToDot true . topologicalProgToDot false
  topologicalOpToDotSubProg _ = id

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
  OpSemantics Sem (Op varId intVal) (Value intVal boolVal) ctx
  where
  applyOp _ Plus = applyPlus
  applyOp _ Equals = applyEquals
  applyOp _ Minus = applyMinus
  applyOp _ (IntConst c) = applyIntConst c
  applyOp sem (If true false) = applyIf sem true false
