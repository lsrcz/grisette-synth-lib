{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

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
    mkSimpleFreshProg,
    fromConcrete,
    mkSketch,
    mkSimpleSketch,
  )
where

import Control.Monad (replicateM)
import qualified Data.Text as T
import Grisette
  ( Fresh,
    GenSymSimple (simpleFresh),
    LogicalOp ((.||)),
    Mergeable,
    Solvable (con),
    ToSym (toSym),
    identifier,
    runFresh,
  )
import Grisette.Lib.Synth.Context (SymbolicContext)
import Grisette.Lib.Synth.Operator.OpTyping
  ( OpTyping (OpTypeType),
    symOpMaximumArgNum,
    symOpMaximumResNum,
  )
import Grisette.Lib.Synth.Program.ComponentSketch.Program
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt, stmtDisabled, stmtMustBeAfter, stmtResIds),
  )
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.ProgTyping (ProgTyping, typeSymbolTable)
import Grisette.Lib.Synth.Program.ProgUtil (ProgUtil (ProgTypeType))
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable)
import Grisette.Lib.Synth.Util.Show (showText)

newtype StmtExtraConstraint op symVarId = StmtExtraConstraint
  { stmtMustBeAfterStmts :: [Stmt op symVarId]
  }

simpleFreshStmt' ::
  ( OpTyping op SymbolicContext,
    GenSymSimple () symVarId,
    ProgTyping prog,
    ProgTypeType prog ~ OpTypeType op
  ) =>
  SymbolTable prog ->
  op ->
  StmtExtraConstraint op symVarId ->
  Fresh [Stmt op symVarId]
simpleFreshStmt' table op (StmtExtraConstraint mustBeAfterStmts) = do
  let tyTable = typeSymbolTable table
  let maxArgNum = symOpMaximumArgNum tyTable op
  argIds <- replicateM maxArgNum (simpleFresh ())
  argNum <- simpleFresh ()
  let maxResNum = symOpMaximumResNum tyTable op
  resIds <- replicateM maxResNum (simpleFresh ())
  resNum <- simpleFresh ()
  disabled <- simpleFresh ()
  return
    [ Stmt op argIds argNum resIds resNum disabled $
        concatMap stmtResIds mustBeAfterStmts
    ]

augmentMustBeAfterForStmts ::
  [Stmt op symVarId] -> [Stmt op symVarId]
augmentMustBeAfterForStmts = go []
  where
    go _ [] = []
    go resIds (stmt : stmts) =
      stmt {stmtMustBeAfter = stmtMustBeAfter stmt ++ resIds}
        : go (resIds ++ stmtResIds stmt) stmts

augmentDisabled ::
  [Stmt op symVarId] -> [Stmt op symVarId]
augmentDisabled = go (con False)
  where
    go _ [] = []
    go disabled (stmt : stmts) =
      let newDisabled = stmtDisabled stmt .|| disabled
       in stmt {stmtDisabled = newDisabled} : go newDisabled stmts

simpleFreshStmts' ::
  ( OpTyping op SymbolicContext,
    GenSymSimple () symVarId,
    ProgTyping prog,
    ProgTypeType prog ~ OpTypeType op
  ) =>
  SymbolTable prog ->
  Int ->
  op ->
  StmtExtraConstraint op symVarId ->
  Fresh [Stmt op symVarId]
simpleFreshStmts' table num op extraConstraint = do
  stmts <- concat <$> replicateM num (simpleFreshStmt' table op extraConstraint)
  return $ augmentDisabled $ augmentMustBeAfterForStmts stmts

simpleFreshStmt ::
  ( OpTyping op SymbolicContext,
    GenSymSimple () symVarId,
    ProgTyping prog,
    ProgTypeType prog ~ OpTypeType op
  ) =>
  SymbolTable prog ->
  op ->
  Fresh [Stmt op symVarId]
simpleFreshStmt table op = simpleFreshStmt' table op (StmtExtraConstraint [])

simpleFreshStmts ::
  ( OpTyping op SymbolicContext,
    GenSymSimple () symVarId,
    ProgTyping prog,
    ProgTypeType prog ~ OpTypeType op
  ) =>
  SymbolTable prog ->
  Int ->
  op ->
  Fresh [Stmt op symVarId]
simpleFreshStmts table num op =
  simpleFreshStmts' table num op (StmtExtraConstraint [])

freshStmt' ::
  ( OpTyping op SymbolicContext,
    Mergeable op,
    GenSymSimple () symVarId,
    ProgTyping prog,
    ProgTypeType prog ~ OpTypeType op
  ) =>
  SymbolTable prog ->
  Fresh op ->
  StmtExtraConstraint op symVarId ->
  Fresh [Stmt op symVarId]
freshStmt' table freshOp extraConstraint = do
  op <- freshOp
  simpleFreshStmt' table op extraConstraint

freshStmts' ::
  ( OpTyping op SymbolicContext,
    Mergeable op,
    GenSymSimple () symVarId,
    ProgTyping prog,
    ProgTypeType prog ~ OpTypeType op
  ) =>
  SymbolTable prog ->
  Int ->
  Fresh op ->
  StmtExtraConstraint op symVarId ->
  Fresh [Stmt op symVarId]
freshStmts' table num freshOp extraConstraint = do
  stmts <- concat <$> replicateM num (freshStmt' table freshOp extraConstraint)
  return $ augmentDisabled $ augmentMustBeAfterForStmts stmts

freshStmt ::
  ( OpTyping op SymbolicContext,
    Mergeable op,
    GenSymSimple () symVarId,
    ProgTyping prog,
    ProgTypeType prog ~ OpTypeType op
  ) =>
  SymbolTable prog ->
  Fresh op ->
  Fresh [Stmt op symVarId]
freshStmt table freshOp = freshStmt' table freshOp (StmtExtraConstraint [])

freshStmts ::
  ( OpTyping op SymbolicContext,
    Mergeable op,
    GenSymSimple () symVarId,
    ProgTyping prog,
    ProgTypeType prog ~ OpTypeType op
  ) =>
  SymbolTable prog ->
  Int ->
  Fresh op ->
  Fresh [Stmt op symVarId]
freshStmts table num freshOp =
  freshStmts' table num freshOp (StmtExtraConstraint [])

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
    (Prog name (uncurry ProgArg <$> args) . concat <$> sequence stmts)
      <*> traverse
        (\(freshVarId, ty) -> ProgRes <$> freshVarId <*> return ty)
        rets

class MkFreshProg prog stmt op ty | prog -> stmt op ty where
  mkFreshProg :: T.Text -> [ty] -> [Fresh [stmt]] -> [ty] -> Fresh prog

mkSimpleFreshProg ::
  forall prog0 prog op ty symVarId.
  ( MkFreshProg prog (Stmt op symVarId) op ty,
    OpTyping op SymbolicContext,
    GenSymSimple () symVarId,
    ProgTyping prog0,
    ProgTypeType prog0 ~ OpTypeType op,
    ProgUtil prog0
  ) =>
  SymbolTable prog0 ->
  T.Text ->
  [ty] ->
  [op] ->
  [ty] ->
  Fresh prog
mkSimpleFreshProg table name argTypes ops =
  mkFreshProg
    name
    argTypes
    (simpleFreshStmt table <$> ops :: [Fresh [Stmt op symVarId]])

instance
  (GenSymSimple () symVarId) =>
  MkFreshProg (Prog op symVarId ty) (Stmt op symVarId) op ty
  where
  mkFreshProg name argTypes freshStmts retTypes =
    ( ( Prog
          name
          [ProgArg ("arg" <> showText n) ty | (n, ty) <- zip [0 ..] argTypes]
      )
        . concat
        <$> sequence freshStmts
    )
      <*> sequence [ProgRes <$> simpleFresh () <*> return ty | ty <- retTypes]

fromConcrete ::
  forall prog symOp symVarId symTy conOp conVarId conTy.
  ( ToSym conTy symTy,
    ToSym conOp symOp,
    OpTyping symOp SymbolicContext,
    GenSymSimple () symVarId,
    ProgTyping prog,
    ProgTypeType prog ~ OpTypeType symOp
  ) =>
  SymbolTable prog ->
  Concrete.Prog conOp conVarId conTy ->
  Fresh (Prog symOp symVarId symTy)
fromConcrete table (Concrete.Prog name argList stmtList resList) = do
  stmts <- concat <$> allFreshStmts
  Prog name args stmts <$> freshResults
  where
    args =
      (\(Concrete.ProgArg name _ ty) -> ProgArg name (toSym ty)) <$> argList
    allFreshStmts =
      traverse
        (\(Concrete.Stmt op _ _) -> simpleFreshStmt table (toSym op :: symOp))
        stmtList
    freshResults =
      traverse
        ( \(Concrete.ProgRes _ ty) -> do
            freshId <- simpleFresh ()
            return $ ProgRes freshId (toSym ty)
        )
        resList

mkSketch ::
  forall prog stmt op ty.
  (MkFreshProg prog stmt op ty) =>
  T.Text ->
  [ty] ->
  [Fresh [stmt]] ->
  [ty] ->
  prog
mkSketch name argTypes stmts retTypes =
  flip runFresh (identifier name) $
    mkFreshProg name argTypes stmts retTypes

mkSimpleSketch ::
  forall prog0 prog op ty symVarId.
  ( MkFreshProg prog (Stmt op symVarId) op ty,
    OpTyping op SymbolicContext,
    GenSymSimple () symVarId,
    ProgTyping prog0,
    ProgTypeType prog0 ~ OpTypeType op
  ) =>
  SymbolTable prog0 ->
  T.Text ->
  [ty] ->
  [op] ->
  [ty] ->
  prog
mkSimpleSketch table name argTypes ops retTypes =
  flip runFresh (identifier name) $
    mkSimpleFreshProg table name argTypes ops retTypes
