{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Typing
  ( Type (..),
    typePlus,
    typeEquals,
    typeMinus,
    typeIntConst,
    typeIf,
  )
where

import Control.Monad (when)
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvaluateSym,
    GPretty (gpretty),
    GenSymSimple (simpleFresh),
    Mergeable,
    MonadFresh,
    ToCon,
  )
import Grisette.Lib.Synth.Context (MonadContext (raiseError, result))
import Grisette.Lib.Synth.Operator.OpTyping (GenIntermediate (genIntermediate))
import Grisette.Lib.Synth.Program.ProgTyping (ProgTyping (typeProg))
import Semantics (Sem)
import Value (SymValue, ValueBuilder (mkBool, mkInt))

data Type = IntType | BoolType
  deriving (Show, Eq, Generic)
  deriving (Mergeable, EvaluateSym, ToCon Type) via (Default Type)

instance GPretty Type where
  gpretty IntType = "int"
  gpretty BoolType = "bool"

typePlus :: (MonadContext ctx) => Int -> ctx ([Type], [Type])
typePlus 2 = result ([IntType, IntType], [IntType])
typePlus _ = raiseError "Incorrect number of arguments"

typeEquals :: (MonadContext ctx) => Int -> ctx ([Type], [Type])
typeEquals 2 = result ([IntType, IntType], [BoolType])
typeEquals _ = raiseError "Incorrect number of arguments"

typeMinus :: (MonadContext ctx) => Int -> ctx ([Type], [Type])
typeMinus 2 = result ([IntType, IntType], [IntType])
typeMinus _ = raiseError "Incorrect number of arguments"

typeIntConst :: (MonadContext ctx) => Int -> ctx ([Type], [Type])
typeIntConst 0 = result ([], [IntType])
typeIntConst _ = raiseError "Incorrect number of arguments"

typeIf ::
  (MonadContext ctx, ProgTyping sem prog Type ctx) =>
  sem ->
  prog ->
  prog ->
  Int ->
  ctx ([Type], [Type])
typeIf sem true false n = do
  trueType@(trueArgType, trueResType) <- typeProg sem true
  falseType@(falseArgType, _) <- typeProg sem false
  when (length trueArgType /= n - 1 || length falseArgType /= n - 1) $
    raiseError "Incorrect number of arguments"
  when (trueType /= falseType) $ raiseError "Unmatch branch types"
  result (BoolType : trueArgType, trueResType)

instance
  ( MonadFresh ctx,
    MonadContext ctx,
    Mergeable intVal,
    Mergeable boolVal,
    GenSymSimple () intVal,
    GenSymSimple () boolVal
  ) =>
  GenIntermediate Sem Type (SymValue intVal boolVal) ctx
  where
  genIntermediate _ IntType = mkInt <$> simpleFresh ()
  genIntermediate _ BoolType = mkBool <$> simpleFresh ()
