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
    MkTypedOp (..),
    freshStmtSimple,
    freshStmtByNumInputs,
    freshStmt,
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
  ( OpTyping (typeOp),
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

class MkTypedOpSimple typedOp op ty | typedOp -> op ty where
  mkTypedOpSimple :: op -> typedOp

instance
  (OpTypingSimple op ty) =>
  MkTypedOpSimple (TypedOp op ty) op ty
  where
  mkTypedOpSimple op =
    case typeOpSimple op of
      Left err -> error $ "mkTypedOpSimple: " <> T.unpack err
      Right signature -> TypedOp op signature

instance
  (OpTypingSimple op ty) =>
  MkTypedOpSimple (Fresh (TypedOp op ty)) (Fresh op) ty
  where
  mkTypedOpSimple freshOp = do
    mkTypedOpSimple <$> freshOp

class MkTypedOpByNumInputs typedOp op ty | typedOp -> op ty where
  mkTypedOpByNumInputs :: op -> Int -> typedOp

instance
  (OpTypingByNumInputs op ty) =>
  MkTypedOpByNumInputs (TypedOp op ty) op ty
  where
  mkTypedOpByNumInputs op numInput =
    case typeOpByNumInputs op numInput of
      Left err -> error $ "mkTypedOpByNumInput: " <> T.unpack err
      Right signature -> TypedOp op signature

instance
  (OpTypingByNumInputs op ty) =>
  MkTypedOpByNumInputs (Fresh (TypedOp op ty)) (Fresh op) ty
  where
  mkTypedOpByNumInputs op numInput =
    mkTypedOpByNumInputs <$> op <*> return numInput

class MkTypedOp typedOp op ty | typedOp -> op ty where
  mkTypedOp :: op -> [ty] -> typedOp

instance
  (OpTyping op ty) =>
  MkTypedOp (TypedOp op ty) op ty
  where
  mkTypedOp op inputTypes =
    case typeOp op inputTypes of
      Left err -> error $ "mkTypedOpByNumInput: " <> T.unpack err
      Right signature -> TypedOp op signature

instance
  (OpTyping op ty) =>
  MkTypedOp (Fresh (TypedOp op ty)) (Fresh op) ty
  where
  mkTypedOp op inputTypes =
    mkTypedOp <$> op <*> return inputTypes

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
  (OpTypingSimple op ty, GenSymSimple () symVarId) =>
  Fresh op ->
  Fresh (Stmt op symVarId ty)
freshStmtSimple freshOp = do
  typedOp@(TypedOp _ TypeSignature {..}) <- mkTypedOpSimple freshOp
  mkStmt
    (return typedOp)
    (simpleFresh () <$ argTypes)
    (simpleFresh () <$ resTypes)
    (simpleFresh ())

freshStmtByNumInputs ::
  (OpTypingByNumInputs op ty, GenSymSimple () symVarId) =>
  Fresh op ->
  Int ->
  Fresh (Stmt op symVarId ty)
freshStmtByNumInputs freshOp numInput = do
  typedOp@(TypedOp _ TypeSignature {..}) <-
    mkTypedOpByNumInputs freshOp numInput
  mkStmt
    (return typedOp)
    (simpleFresh () <$ argTypes)
    (simpleFresh () <$ resTypes)
    (simpleFresh ())

freshStmt ::
  (OpTyping op ty, GenSymSimple () symVarId) =>
  Fresh op ->
  [ty] ->
  Fresh (Stmt op symVarId ty)
freshStmt freshOp inputTypes = do
  typedOp@(TypedOp _ TypeSignature {..}) <-
    mkTypedOp freshOp inputTypes
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
