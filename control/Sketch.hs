{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Sketch (Op (..), Prog) where

import qualified ConProg as Concrete
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvalSym,
    GenSymSimple,
    LogicalOp (false),
    Mergeable,
    ToCon,
    mrgReturn,
  )
import Grisette.Lib.Synth.Context (MonadAngelicContext, MonadContext)
import Grisette.Lib.Synth.Operator.OpSemantics
  ( DefaultSem,
    OpSemantics (applyOp),
  )
import Grisette.Lib.Synth.Operator.OpTyping
  ( OpTyping (OpTypeType, typeOp),
  )
import qualified Grisette.Lib.Synth.Program.ComponentSketch as Component
import Grisette.Lib.Synth.Program.ComponentSketch.SymmetryReduction
  ( OpSymmetryReduction (opUnreorderable),
  )
import Grisette.Lib.Synth.TypeSignature (TypeSignature)
import Semantics
  ( HasSemantics,
    applyEquals,
    applyIf,
    applyIntConst,
    applyMinus,
    applyPlus,
  )
import Typing (Type, typeEquals, typeIf, typeIntConst, typeMinus, typePlus)
import Value (SymValue)

data Op intVal
  = Plus
  | Equals
  | Minus
  | IntConst intVal
  | If (TypeSignature Type) T.Text T.Text
  deriving (Show, Generic)
  deriving (EvalSym, Mergeable) via (Default (Op intVal))

deriving via
  (Default (Concrete.Op conIntVal))
  instance
    ( ToCon symIntVal conIntVal,
      Mergeable symIntVal
    ) =>
    ToCon (Op symIntVal) (Concrete.Op conIntVal)

type Prog varId intVal = Component.Prog (Op intVal) varId Type

instance
  (MonadContext ctx) =>
  OpTyping (Op intVal) ctx
  where
  type OpTypeType (Op intVal) = Type
  typeOp Plus = typePlus
  typeOp Equals = typeEquals
  typeOp Minus = typeMinus
  typeOp IntConst {} = typeIntConst
  typeOp (If sig _ _) = mrgReturn $ typeIf sig

instance
  ( HasSemantics (SymValue intVal boolVal) ctx,
    MonadAngelicContext ctx,
    GenSymSimple () intVal,
    GenSymSimple () boolVal
  ) =>
  OpSemantics DefaultSem (Op intVal) (SymValue intVal boolVal) ctx
  where
  applyOp _ _ Plus = applyPlus
  applyOp _ _ Equals = applyEquals
  applyOp _ _ Minus = applyMinus
  applyOp _ _ (IntConst c) = applyIntConst c
  applyOp _ table (If _ true false) = applyIf table true false

instance OpSymmetryReduction (Op intVal) where
  opUnreorderable _ _ = false
  opCommutativeArgPos Plus = mrgReturn [[0, 1]]
  opCommutativeArgPos Equals = mrgReturn [[0, 1]]
  opCommutativeArgPos _ = mrgReturn []
