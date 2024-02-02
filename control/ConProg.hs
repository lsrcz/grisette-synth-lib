{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module ConProg (Op (..), Prog) where

import GHC.Generics (Generic)
import Grisette
  ( GPretty (gpretty),
  )
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Operator.OpPretty
  ( OpPretty (describeArguments, prefixResults),
    OpPrettyError (IncorrectNumberOfArguments),
  )
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Operator.OpTyping (OpTyping (typeOp))
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
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
  deriving (Show, Generic)

type Prog varId intVal = Concrete.Prog (Op varId intVal) varId Type

instance (GPretty intVal) => GPretty (Op varId intVal) where
  gpretty Plus = "plus"
  gpretty Equals = "equals"
  gpretty Minus = "Minus"
  gpretty (IntConst n) = "const" <> parenCommaList [gpretty n]
  gpretty (If true false) =
    "if" <> parenCommaList (gpretty <$> [nameProg true, nameProg false])

instance (GPretty intVal) => OpPretty (Op varId intVal) where
  describeArguments Plus 2 = return $ replicate 2 Nothing
  describeArguments Equals 2 = return $ replicate 2 Nothing
  describeArguments Minus 2 = return $ replicate 2 Nothing
  describeArguments (IntConst _) 0 = return []
  describeArguments (If true _) n
    | n == 1 + length (Concrete.progArgList true) =
        return $ Just "cond" : (Nothing <$ Concrete.progArgList true)
  describeArguments op n = Left $ IncorrectNumberOfArguments op n
  prefixResults _ _ r = return $ replicate r "r"

instance
  ( ConcreteVarId varId,
    GPretty intVal,
    Show intVal
  ) =>
  Concrete.OpDirectSubProgs (Op varId intVal) Concrete.SomePrettyProg
  where
  opDirectSubProgs (If true false) =
    [Concrete.SomePrettyProg true, Concrete.SomePrettyProg false]
  opDirectSubProgs _ = []

instance (MonadContext ctx) => OpTyping Sem (Op varId intVal) Type ctx where
  typeOp _ Plus = typePlus
  typeOp _ Equals = typeEquals
  typeOp _ Minus = typeMinus
  typeOp _ IntConst {} = typeIntConst
  typeOp sem (If true false) = typeIf sem true false

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
