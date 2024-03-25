{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Program.ComponentSketch.Builder
  ( simpleFreshStmt,
    freshStmt,
    MkProg (..),
    MkFreshProg (..),
    fromConcrete,
  )
where

import Control.Monad (replicateM)
import qualified Data.Text as T
import Grisette
  ( Fresh,
    GenSymSimple (simpleFresh),
    Mergeable,
    ToSym (toSym),
    chooseFresh,
  )
import Grisette.Lib.Synth.Operator.OpTyping
  ( SymOpLimits (symOpMaximumArgNum, symOpMaximumResNum),
  )
import Grisette.Lib.Synth.Program.ComponentSketch.Program
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Util.Show (showText)

simpleFreshStmt ::
  (SymOpLimits op, Mergeable op, GenSymSimple () symVarId) =>
  op ->
  Fresh (Stmt op symVarId ty)
simpleFreshStmt op = freshStmt (return [op])

freshStmt ::
  (SymOpLimits op, Mergeable op, GenSymSimple () symVarId) =>
  Fresh [op] ->
  Fresh (Stmt op symVarId ty)
freshStmt freshOps = do
  ops <- freshOps
  chosenOps <- chooseFresh ops
  let maxArgNum = maximum $ symOpMaximumArgNum <$> ops
  argIds <- replicateM maxArgNum (simpleFresh ())
  argNum <- simpleFresh ()
  let maxResNum = maximum $ symOpMaximumResNum <$> ops
  resIds <- replicateM maxResNum (simpleFresh ())
  resNum <- simpleFresh ()
  disabled <- simpleFresh ()
  return $ Stmt chosenOps argIds argNum resIds resNum disabled

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

fromConcrete ::
  forall symOp symVarId symTy conOp conVarId conTy.
  ( ToSym conTy symTy,
    ToSym conOp symOp,
    SymOpLimits symOp,
    Mergeable symOp,
    GenSymSimple () symVarId
  ) =>
  Concrete.Prog conOp conVarId conTy ->
  Fresh (Prog symOp symVarId symTy)
fromConcrete (Concrete.Prog name argList stmtList resList) = do
  stmts <- freshStmts
  Prog name args stmts <$> freshResults
  where
    args =
      (\(Concrete.ProgArg name _ ty) -> ProgArg name (toSym ty)) <$> argList
    freshStmts =
      traverse
        (\(Concrete.Stmt op _ _) -> freshStmt (return [toSym op :: symOp]))
        stmtList
    freshResults =
      traverse
        ( \(Concrete.ProgRes _ ty) -> do
            freshId <- simpleFresh ()
            return $ ProgRes freshId (toSym ty)
        )
        resList
