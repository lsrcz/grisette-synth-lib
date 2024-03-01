{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Sketch (Op (..), Prog) where

import qualified ConProg as Concrete
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvaluateSym,
    GenSymSimple,
    MonadFresh,
    MonadUnion,
    ToCon,
  )
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Operator.OpTyping
  ( OpTyping (typeOp),
    SymOpLimits (symOpMaximumArgNum, symOpMaximumResNum),
    SymOpTyping,
  )
import qualified Grisette.Lib.Synth.Program.ComponentSketch as Component
import Grisette.Lib.Synth.TypeSignature (TypeSignature (TypeSignature))
import Grisette.Lib.Synth.VarId (RelatedVarId, SymbolicVarId)
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
import Value (SymValue)

data Op varId intVal
  = Plus
  | Equals
  | Minus
  | IntConst intVal
  | If (Prog varId intVal) (Prog varId intVal)
  deriving (Show, Generic)
  deriving (EvaluateSym) via (Default (Op varId intVal))

deriving via
  (Default (Concrete.Op conVarId conIntVal))
  instance
    (RelatedVarId conVarId symVarId, ToCon symIntVal conIntVal) =>
    ToCon (Op symVarId symIntVal) (Concrete.Op conVarId conIntVal)

type Prog varId intVal = Component.Prog (Op varId intVal) varId Type

instance
  (MonadContext ctx) =>
  OpTyping (Op varId intVal) Type ctx
  where
  typeOp Plus = typePlus
  typeOp Equals = typeEquals
  typeOp Minus = typeMinus
  typeOp IntConst {} = typeIntConst
  typeOp (If true false) = typeIf true false

instance
  (MonadContext ctx, MonadUnion ctx) =>
  SymOpTyping (Op varId intVal) Type ctx

instance SymOpLimits (Op varId intVal) where
  symOpMaximumArgNum op = case typeOp op of
    Left err -> error $ "symOpMaximumArgNum: " ++ show err
    Right (TypeSignature argTypes _) -> length argTypes
  symOpMaximumResNum op = case typeOp op of
    Left err -> error $ "symOpMaximumResNum: " ++ show err
    Right (TypeSignature _ resTypes) -> length resTypes

instance
  ( HasSemantics (SymValue intVal boolVal) ctx,
    MonadUnion ctx,
    MonadFresh ctx,
    SymbolicVarId varId,
    GenSymSimple () intVal,
    GenSymSimple () boolVal
  ) =>
  OpSemantics Sem (Op varId intVal) (SymValue intVal boolVal) ctx
  where
  applyOp _ Plus = applyPlus
  applyOp _ Equals = applyEquals
  applyOp _ Minus = applyMinus
  applyOp _ (IntConst c) = applyIntConst c
  applyOp sem (If true false) = applyIf sem true false
