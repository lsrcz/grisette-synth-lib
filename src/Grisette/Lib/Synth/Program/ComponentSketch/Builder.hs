{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Program.ComponentSketch.Builder
  ( StmtExtraConstraint (..),
    simpleFreshStmt,
    simpleFreshStmt',
    simpleFreshStmts,
    simpleFreshStmts',
    freshStmt,
    freshStmt',
    freshStmts,
    freshStmts',
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
  )
import Grisette.Lib.Synth.Operator.OpTyping
  ( SymOpLimits (symOpMaximumArgNum, symOpMaximumResNum),
  )
import Grisette.Lib.Synth.Program.ComponentSketch.Program
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt, stmtMustBeAfter, stmtResIds),
  )
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Util.Show (showText)

newtype StmtExtraConstraint op symVarId = StmtExtraConstraint
  { stmtMustBeAfterStmts :: [Stmt op symVarId]
  }

simpleFreshStmt' ::
  (SymOpLimits op, GenSymSimple () symVarId) =>
  op ->
  StmtExtraConstraint op symVarId ->
  Fresh [Stmt op symVarId]
simpleFreshStmt' op (StmtExtraConstraint mustBeAfterStmts) = do
  let maxArgNum = symOpMaximumArgNum op
  argIds <- replicateM maxArgNum (simpleFresh ())
  argNum <- simpleFresh ()
  let maxResNum = symOpMaximumResNum op
  resIds <- replicateM maxResNum (simpleFresh ())
  resNum <- simpleFresh ()
  disabled <- simpleFresh ()
  return
    [ Stmt op argIds argNum resIds resNum disabled $
        concatMap stmtResIds mustBeAfterStmts
    ]

augmentMustBeAfterForStmts :: [symVarId] -> [Stmt op symVarId] -> [Stmt op symVarId]
augmentMustBeAfterForStmts _ [] = []
augmentMustBeAfterForStmts resIds (stmt : stmts) =
  stmt {stmtMustBeAfter = stmtMustBeAfter stmt ++ resIds}
    : augmentMustBeAfterForStmts (resIds ++ stmtResIds stmt) stmts

simpleFreshStmts' ::
  (SymOpLimits op, GenSymSimple () symVarId) =>
  Int ->
  op ->
  StmtExtraConstraint op symVarId ->
  Fresh [Stmt op symVarId]
simpleFreshStmts' num op extraConstraint = do
  stmts <- concat <$> replicateM num (simpleFreshStmt' op extraConstraint)
  return $ augmentMustBeAfterForStmts [] stmts

simpleFreshStmt ::
  (SymOpLimits op, GenSymSimple () symVarId) =>
  op ->
  Fresh [Stmt op symVarId]
simpleFreshStmt op = simpleFreshStmt' op (StmtExtraConstraint [])

simpleFreshStmts ::
  (SymOpLimits op, GenSymSimple () symVarId) =>
  Int ->
  op ->
  Fresh [Stmt op symVarId]
simpleFreshStmts num op = simpleFreshStmts' num op (StmtExtraConstraint [])

freshStmt' ::
  (SymOpLimits op, Mergeable op, GenSymSimple () symVarId) =>
  Fresh op ->
  StmtExtraConstraint op symVarId ->
  Fresh [Stmt op symVarId]
freshStmt' freshOp extraConstraint = do
  op <- freshOp
  simpleFreshStmt' op extraConstraint

freshStmts' ::
  (SymOpLimits op, Mergeable op, GenSymSimple () symVarId) =>
  Int ->
  Fresh op ->
  StmtExtraConstraint op symVarId ->
  Fresh [Stmt op symVarId]
freshStmts' num freshOp extraConstraint = do
  stmts <- concat <$> replicateM num (freshStmt' freshOp extraConstraint)
  return $ augmentMustBeAfterForStmts [] stmts

freshStmt ::
  (SymOpLimits op, Mergeable op, GenSymSimple () symVarId) =>
  Fresh op ->
  Fresh [Stmt op symVarId]
freshStmt freshOp = freshStmt' freshOp (StmtExtraConstraint [])

freshStmts ::
  (SymOpLimits op, Mergeable op, GenSymSimple () symVarId) =>
  Int ->
  Fresh op ->
  Fresh [Stmt op symVarId]
freshStmts num freshOp = freshStmts' num freshOp (StmtExtraConstraint [])

class MkProg prog stmts op symVarId ty | prog -> stmts op symVarId ty where
  mkProg ::
    T.Text ->
    [(T.Text, ty)] ->
    stmts ->
    [(symVarId, ty)] ->
    prog

instance MkProg (Prog op symVarId ty) [Stmt op symVarId] op symVarId ty where
  mkProg name args stmts rets =
    Prog name (uncurry ProgArg <$> args) stmts (uncurry ProgRes <$> rets)

instance
  MkProg
    (Fresh (Prog op symVarId ty))
    [Fresh [Stmt op symVarId]]
    (Fresh op)
    (Fresh symVarId)
    ty
  where
  mkProg name args stmts rets =
    Prog name (uncurry ProgArg <$> args)
      <$> (concat <$> sequence stmts)
      <*> traverse
        (\(freshVarId, ty) -> ProgRes <$> freshVarId <*> return ty)
        rets

class MkFreshProg prog stmt op ty | prog -> stmt op ty where
  mkFreshProg :: T.Text -> [ty] -> [Fresh [stmt]] -> [ty] -> Fresh prog

instance
  (GenSymSimple () symVarId) =>
  MkFreshProg (Prog op symVarId ty) (Stmt op symVarId) op ty
  where
  mkFreshProg name argTypes freshStmts retTypes =
    Prog
      name
      [ProgArg ("arg" <> showText n) ty | (n, ty) <- zip [0 ..] argTypes]
      <$> (concat <$> sequence freshStmts)
      <*> sequence [ProgRes <$> simpleFresh () <*> return ty | ty <- retTypes]

fromConcrete ::
  forall symOp symVarId symTy conOp conVarId conTy.
  ( ToSym conTy symTy,
    ToSym conOp symOp,
    SymOpLimits symOp,
    GenSymSimple () symVarId
  ) =>
  Concrete.Prog conOp conVarId conTy ->
  Fresh (Prog symOp symVarId symTy)
fromConcrete (Concrete.Prog name argList stmtList resList) = do
  stmts <- concat <$> allFreshStmts
  Prog name args stmts <$> freshResults
  where
    args =
      (\(Concrete.ProgArg name _ ty) -> ProgArg name (toSym ty)) <$> argList
    allFreshStmts =
      traverse
        (\(Concrete.Stmt op _ _) -> simpleFreshStmt (toSym op :: symOp))
        stmtList
    freshResults =
      traverse
        ( \(Concrete.ProgRes _ ty) -> do
            freshId <- simpleFresh ()
            return $ ProgRes freshId (toSym ty)
        )
        resList
