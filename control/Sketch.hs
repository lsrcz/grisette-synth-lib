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
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Program.ComponentSketch
  ( OpTypingSimple (typeOpSimple),
  )
import qualified Grisette.Lib.Synth.Program.ComponentSketch as Component
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

instance OpTypingSimple Sem (Op varId intVal) Type where
  typeOpSimple _ Plus = typePlus
  typeOpSimple _ Equals = typeEquals
  typeOpSimple _ Minus = typeMinus
  typeOpSimple _ IntConst {} = typeIntConst
  typeOpSimple sem (If true false) = typeIf sem true false

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
