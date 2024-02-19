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
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Synth.Context (MonadContext)
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
typePlus 2 = mrgReturn ([IntType, IntType], [IntType])
typePlus _ = mrgThrowError "Incorrect number of arguments"

typeEquals :: (MonadContext ctx) => Int -> ctx ([Type], [Type])
typeEquals 2 = mrgReturn ([IntType, IntType], [BoolType])
typeEquals _ = mrgThrowError "Incorrect number of arguments"

typeMinus :: (MonadContext ctx) => Int -> ctx ([Type], [Type])
typeMinus 2 = mrgReturn ([IntType, IntType], [IntType])
typeMinus _ = mrgThrowError "Incorrect number of arguments"

typeIntConst :: (MonadContext ctx) => Int -> ctx ([Type], [Type])
typeIntConst 0 = mrgReturn ([], [IntType])
typeIntConst _ = mrgThrowError "Incorrect number of arguments"

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
    mrgThrowError "Incorrect number of arguments"
  when (trueType /= falseType) $ mrgThrowError "Unmatch branch types"
  mrgReturn (BoolType : trueArgType, trueResType)

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
