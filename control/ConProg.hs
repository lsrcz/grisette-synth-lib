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
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Operator.OpTyping
  ( OpTypingSimple (typeOpSimple),
  )
import Grisette.Lib.Synth.Program.Concrete
  ( OpPretty (describeArguments, prefixResults),
    OpPrettyError (IncorrectNumberOfArguments),
  )
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

instance OpTypingSimple (Op varId intVal) Type where
  typeOpSimple Plus = typePlus
  typeOpSimple Equals = typeEquals
  typeOpSimple Minus = typeMinus
  typeOpSimple IntConst {} = typeIntConst
  typeOpSimple (If true false) = typeIf true false

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
