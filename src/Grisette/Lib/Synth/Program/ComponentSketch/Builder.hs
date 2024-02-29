{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Program.ComponentSketch.Builder
  ( MkStmt (..),
    MkTypedOpSimple (..),
    MkTypedOpByNumInputs (..),
    MkTypedOpByInputTypes (..),
    freshStmtSimple,
    freshStmtByNumInputs,
    freshStmtByInputTypes,
    MkProg (..),
    MkFreshProg (..),
  )
where

import qualified Data.Text as T
import Grisette
  ( Fresh,
    GenSymSimple (simpleFresh),
    SymBool,
  )
import Grisette.Lib.Synth.Program.ComponentSketch.OpTyping
  ( OpTypingByInputTypes (typeOpByInputTypes),
    OpTypingByNumInputs (typeOpByNumInputs),
    OpTypingSimple (typeOpSimple),
  )
import Grisette.Lib.Synth.Program.ComponentSketch.Program
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
    TypedOp (TypedOp),
  )
import Grisette.Lib.Synth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import Grisette.Lib.Synth.Util.Show (showText)

class MkTypedOpSimple typedOp semObj op ty | typedOp -> op ty where
  mkTypedOpSimple :: semObj -> op -> typedOp

instance
  (OpTypingSimple semObj op ty) =>
  MkTypedOpSimple (TypedOp op ty) semObj op ty
  where
  mkTypedOpSimple semObj op =
    case typeOpSimple semObj op of
      Left err -> error $ "mkTypedOpSimple: " <> T.unpack err
      Right signature -> TypedOp op signature

instance
  (OpTypingSimple semObj op ty) =>
  MkTypedOpSimple (Fresh (TypedOp op ty)) semObj (Fresh op) ty
  where
  mkTypedOpSimple semObj freshOp = do
    mkTypedOpSimple semObj <$> freshOp

class MkTypedOpByNumInputs typedOp semObj op ty | typedOp -> op ty where
  mkTypedOpByNumInputs :: semObj -> op -> Int -> typedOp

instance
  (OpTypingByNumInputs semObj op ty) =>
  MkTypedOpByNumInputs (TypedOp op ty) semObj op ty
  where
  mkTypedOpByNumInputs semObj op numInput =
    case typeOpByNumInputs semObj op numInput of
      Left err -> error $ "mkTypedOpByNumInput: " <> T.unpack err
      Right signature -> TypedOp op signature

instance
  (OpTypingByNumInputs semObj op ty) =>
  MkTypedOpByNumInputs (Fresh (TypedOp op ty)) semObj (Fresh op) ty
  where
  mkTypedOpByNumInputs semObj op numInput =
    mkTypedOpByNumInputs semObj <$> op <*> return numInput

class MkTypedOpByInputTypes typedOp semObj op ty | typedOp -> op ty where
  mkTypedOpByInputTypes :: semObj -> op -> [ty] -> typedOp

instance
  (OpTypingByInputTypes semObj op ty) =>
  MkTypedOpByInputTypes (TypedOp op ty) semObj op ty
  where
  mkTypedOpByInputTypes semObj op inputTypes =
    case typeOpByInputTypes semObj op inputTypes of
      Left err -> error $ "mkTypedOpByNumInput: " <> T.unpack err
      Right signature -> TypedOp op signature

instance
  (OpTypingByInputTypes semObj op ty) =>
  MkTypedOpByInputTypes (Fresh (TypedOp op ty)) semObj (Fresh op) ty
  where
  mkTypedOpByInputTypes semObj op inputTypes =
    mkTypedOpByInputTypes semObj <$> op <*> return inputTypes

class MkStmt stmt typedOp symVarId bool | stmt -> typedOp symVarId bool where
  mkStmt :: typedOp -> [symVarId] -> [symVarId] -> bool -> stmt

instance MkStmt (Stmt op symVarId ty) (TypedOp op ty) symVarId SymBool where
  mkStmt = Stmt

instance
  MkStmt
    (Fresh (Stmt op symVarId ty))
    (Fresh (TypedOp op ty))
    (Fresh symVarId)
    (Fresh SymBool)
  where
  mkStmt typedOp freshArgIds freshResIds freshDisabled =
    Stmt
      <$> typedOp
      <*> sequence freshArgIds
      <*> sequence freshResIds
      <*> freshDisabled

freshStmtSimple ::
  (OpTypingSimple semObj op ty, GenSymSimple () symVarId) =>
  semObj ->
  Fresh op ->
  Fresh (Stmt op symVarId ty)
freshStmtSimple semObj freshOp = do
  typedOp@(TypedOp _ TypeSignature {..}) <- mkTypedOpSimple semObj freshOp
  mkStmt
    (return typedOp)
    (simpleFresh () <$ argTypes)
    (simpleFresh () <$ resTypes)
    (simpleFresh ())

freshStmtByNumInputs ::
  (OpTypingByNumInputs semObj op ty, GenSymSimple () symVarId) =>
  semObj ->
  Fresh op ->
  Int ->
  Fresh (Stmt op symVarId ty)
freshStmtByNumInputs semObj freshOp numInput = do
  typedOp@(TypedOp _ TypeSignature {..}) <-
    mkTypedOpByNumInputs semObj freshOp numInput
  mkStmt
    (return typedOp)
    (simpleFresh () <$ argTypes)
    (simpleFresh () <$ resTypes)
    (simpleFresh ())

freshStmtByInputTypes ::
  (OpTypingByInputTypes semObj op ty, GenSymSimple () symVarId) =>
  semObj ->
  Fresh op ->
  [ty] ->
  Fresh (Stmt op symVarId ty)
freshStmtByInputTypes semObj freshOp inputTypes = do
  typedOp@(TypedOp _ TypeSignature {..}) <-
    mkTypedOpByInputTypes semObj freshOp inputTypes
  mkStmt
    (return typedOp)
    (simpleFresh () <$ argTypes)
    (simpleFresh () <$ resTypes)
    (simpleFresh ())

class MkProg prog stmt op symVarId ty | prog -> stmt op symVarId ty where
  mkProg ::
    T.Text ->
    [(T.Text, ty)] ->
    [stmt] ->
    [(symVarId, ty)] ->
    prog

instance MkProg (Prog op symVarId ty) (Stmt op symVarId ty) op symVarId ty where
  mkProg name args stmts rets =
    Prog name (uncurry ProgArg <$> args) stmts (uncurry ProgRes <$> rets)

instance
  MkProg
    (Fresh (Prog op symVarId ty))
    (Fresh (Stmt op symVarId ty))
    (Fresh op)
    (Fresh symVarId)
    ty
  where
  mkProg name args stmts rets =
    Prog name (uncurry ProgArg <$> args)
      <$> sequence stmts
      <*> traverse
        (\(freshVarId, ty) -> ProgRes <$> freshVarId <*> return ty)
        rets

class MkFreshProg prog stmt op ty | prog -> stmt op ty where
  mkFreshProg :: T.Text -> [ty] -> [Fresh stmt] -> [ty] -> Fresh prog

instance
  (GenSymSimple () symVarId) =>
  MkFreshProg (Prog op symVarId ty) (Stmt op symVarId ty) op ty
  where
  mkFreshProg name argTypes freshStmts retTypes =
    Prog
      name
      [ProgArg ("arg" <> showText n) ty | (n, ty) <- zip [0 ..] argTypes]
      <$> sequence freshStmts
      <*> sequence [ProgRes <$> simpleFresh () <*> return ty | ty <- retTypes]
