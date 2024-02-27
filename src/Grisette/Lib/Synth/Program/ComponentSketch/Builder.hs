{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Program.ComponentSketch.Builder
  ( MkStmt (..),
    MkFreshStmt (..),
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
import Grisette.Lib.Synth.Program.ComponentSketch.Program
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import Grisette.Lib.Synth.Util.Show (showText)

class MkStmt stmt op symVarId bool | stmt -> op symVarId bool where
  mkStmt :: op -> [symVarId] -> [symVarId] -> bool -> stmt

class MkFreshStmt stmt op | stmt -> op where
  mkFreshStmt :: Fresh op -> Int -> Int -> Fresh stmt

instance MkStmt (Stmt op symVarId) op symVarId SymBool where
  mkStmt = Stmt

instance
  MkStmt
    (Fresh (Stmt op symVarId))
    (Fresh op)
    (Fresh symVarId)
    (Fresh SymBool)
  where
  mkStmt op freshArgIds freshResIds freshDisabled =
    Stmt
      <$> op
      <*> sequence freshArgIds
      <*> sequence freshResIds
      <*> freshDisabled

instance
  (GenSymSimple () symVarId) =>
  MkFreshStmt (Stmt op symVarId) op
  where
  mkFreshStmt op argCount resCount = do
    mkStmt
      op
      (replicate argCount $ simpleFresh ())
      (replicate resCount $ simpleFresh ())
      (simpleFresh ())

class MkProg prog stmt op symVarId ty | prog -> stmt op symVarId ty where
  mkProg ::
    T.Text ->
    [(ty, T.Text)] ->
    [stmt] ->
    [(ty, symVarId)] ->
    prog

instance MkProg (Prog op symVarId ty) (Stmt op symVarId) op symVarId ty where
  mkProg name args stmts rets =
    Prog name (uncurry ProgArg <$> args) stmts (uncurry ProgRes <$> rets)

instance
  MkProg
    (Fresh (Prog op symVarId ty))
    (Fresh (Stmt op symVarId))
    (Fresh op)
    (Fresh symVarId)
    ty
  where
  mkProg name args stmts rets =
    Prog name (uncurry ProgArg <$> args)
      <$> sequence stmts
      <*> traverse (\(ty, freshVarId) -> ProgRes ty <$> freshVarId) rets

class MkFreshProg prog stmt op ty | prog -> stmt op ty where
  mkFreshProg :: T.Text -> [ty] -> [Fresh stmt] -> [ty] -> Fresh prog

instance
  (GenSymSimple () symVarId) =>
  MkFreshProg (Prog op symVarId ty) (Stmt op symVarId) op ty
  where
  mkFreshProg name argTypes freshStmts retTypes =
    Prog
      name
      [ProgArg ty ("arg" <> showText n) | (n, ty) <- zip [0 ..] argTypes]
      <$> sequence freshStmts
      <*> sequence [ProgRes ty <$> simpleFresh () | ty <- retTypes]
