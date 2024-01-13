{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.Concrete
  ( Stmt (..),
    ProgArg (..),
    ProgRes (..),
    Prog (..),
    ProgPrettyError (..),
    prettyStmt,
    prettyProg,
    OpDirectSubProgs (..),
    SomeConstrainedProg (..),
    topologicalSortSubProg,
    SomePrettyProg (..),
  )
where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State (MonadState (get, put), StateT, evalStateT)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map.Ordered as OM
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette (Default (Default), GPretty (gpretty), Mergeable)
import Grisette.Lib.Synth.Operator.OpPretty
  ( OpPretty,
    OpPrettyError,
    VarIdMap,
    prettyArguments,
    prettyResults,
  )
import Grisette.Lib.Synth.Util.Pretty
  ( Doc,
    concatWith,
    hardline,
    nest,
    parenCommaList,
    parenCommaListIfNotSingle,
    (<+>),
  )
import Grisette.Lib.Synth.VarId (ConcreteVarId)

data Stmt op varId = Stmt
  { stmtOp :: op,
    stmtArgIds :: [varId],
    stmtResIds :: [varId]
  }
  deriving (Show, Eq, Generic)
  deriving (Mergeable) via (Default (Stmt op varId))

data ProgArg varId ty = ProgArg
  { progArgType :: ty,
    progArgName :: T.Text,
    progArgId :: varId
  }
  deriving (Show, Eq, Generic)
  deriving (Mergeable) via (Default (ProgArg varId ty))

data ProgRes varId ty = ProgRes
  { progResType :: ty,
    progResId :: varId
  }
  deriving (Show, Eq, Generic)
  deriving (Mergeable) via (Default (ProgRes varId ty))

data Prog op varId ty = Prog
  { progName :: T.Text,
    progArgList :: [ProgArg varId ty],
    progStmtList :: [Stmt op varId],
    progResList :: [ProgRes varId ty]
  }
  deriving (Show, Eq, Generic)
  deriving (Mergeable) via (Default (Prog op varId ty))

data ProgPrettyError op varId
  = StmtPrettyError (Stmt op varId) Int (OpPrettyError op varId)
  | ResultUndefined Int varId
  deriving (Show, Eq, Generic)
  deriving (Mergeable) via (Default (ProgPrettyError op varId))

instance
  (GPretty op, Show op, ConcreteVarId varId) =>
  GPretty (ProgPrettyError op varId)
  where
  gpretty (StmtPrettyError stmt index err) =
    nest
      2
      ( "Error in statement "
          <> gpretty index
          <> ": "
          <> hardline
          <> gpretty err
          <> "."
      )
      <> hardline
      <> nest
        2
        ( "Raw statement: "
            <> hardline
            <> gpretty (show stmt)
        )
  gpretty (ResultUndefined index varId) =
    "Error in result "
      <> gpretty index
      <> ": the variable "
      <> gpretty (toInteger varId)
      <> " is undefined."

prettyStmt ::
  (ConcreteVarId varId, OpPretty op, GPretty op) =>
  Int ->
  Stmt op varId ->
  StateT (VarIdMap varId) (Either (ProgPrettyError op varId)) (Doc ann)
prettyStmt index stmt@(Stmt op argIds resIds) = do
  map <- get
  argPretty <- case prettyArguments op argIds map of
    Left err -> throwError $ StmtPrettyError stmt index err
    Right argPretty -> pure argPretty
  let opPretty = gpretty op
  (newMap, resPretty) <- case prettyResults op (length argIds) resIds map of
    Left err -> throwError $ StmtPrettyError stmt index err
    Right resPretty -> pure resPretty
  put newMap
  return $ resPretty <> " = " <> opPretty <> argPretty

prettyProg ::
  (ConcreteVarId varId, OpPretty op, GPretty op, GPretty ty) =>
  Prog op varId ty ->
  Either (ProgPrettyError op varId) (Doc ann)
prettyProg (Prog name argList stmtList resList) = do
  let initMap =
        HM.fromList $ map (\arg -> (progArgId arg, progArgName arg)) argList
  flip evalStateT initMap $ do
    stmtsPretty <- traverse (uncurry prettyStmt) (zip [0 ..] stmtList)
    let firstLine =
          nest (-2) $
            "def "
              <> gpretty name
              <> parenCommaList
                ( map
                    ( \arg ->
                        gpretty (progArgName arg)
                          <> ": "
                          <> gpretty (progArgType arg)
                    )
                    argList
                )
              <> ":"
    allMap <- get
    let lookupVarId (idx, varId) =
          maybe (throwError $ ResultUndefined idx varId) return $
            HM.lookup varId allMap
    retNames <- traverse lookupVarId (zip [0 ..] $ progResId <$> resList)
    let ret = "return" <+> parenCommaListIfNotSingle (gpretty <$> retNames)
    return . nest 2 . concatWith (\x y -> x <> hardline <> y) $
      concat [[firstLine], stmtsPretty, [ret]]

data SomePrettyProg where
  SomePrettyProg ::
    ( ConcreteVarId varId,
      OpPretty op,
      Show op,
      GPretty op,
      GPretty ty,
      Show ty,
      OpDirectSubProgs op SomePrettyProg
    ) =>
    Prog op varId ty ->
    SomePrettyProg

instance GPretty SomePrettyProg where
  gpretty (SomePrettyProg prog) = do
    let progDoc = prettyProg prog
    case progDoc of
      Left err ->
        nest
          2
          ( "Error while pretty-printing program "
              <> gpretty (progName prog)
              <> hardline
              <> gpretty err
          )
          <> hardline
          <> nest 2 ("Raw program: " <> hardline <> gpretty (show prog))
      Right doc -> doc

class OpDirectSubProgs op someConstrainedProg where
  opDirectSubProgs :: op -> [someConstrainedProg]

class SomeConstrainedProg someConstrainedProg where
  someProgName :: someConstrainedProg -> T.Text
  someDirectSubProgs :: someConstrainedProg -> [someConstrainedProg]

instance SomeConstrainedProg SomePrettyProg where
  someProgName (SomePrettyProg prog) = progName prog
  someDirectSubProgs (SomePrettyProg (Prog _ _ stmts _)) =
    opDirectSubProgs . stmtOp =<< stmts

topologicalSortSubProgStep ::
  (SomeConstrainedProg someConstrainedProg) =>
  OM.OMap T.Text someConstrainedProg ->
  someConstrainedProg ->
  OM.OMap T.Text someConstrainedProg
topologicalSortSubProgStep map someConstrainedProg
  | OM.member (someProgName someConstrainedProg) map = map
topologicalSortSubProgStep map someConstrainedProg =
  allSub OM.>| (someProgName someConstrainedProg, someConstrainedProg)
  where
    allSub =
      foldl topologicalSortSubProgStep map $
        someDirectSubProgs someConstrainedProg

topologicalSortSubProg ::
  (SomeConstrainedProg someConstrainedProg) =>
  someConstrainedProg ->
  [someConstrainedProg]
topologicalSortSubProg prog =
  snd <$> OM.assocs (topologicalSortSubProgStep OM.empty prog)

instance
  ( ConcreteVarId varId,
    OpPretty op,
    Show op,
    GPretty op,
    GPretty ty,
    Show ty,
    OpDirectSubProgs op SomePrettyProg
  ) =>
  GPretty (Prog op varId ty)
  where
  gpretty prog =
    concatWith (\l r -> l <> hardline <> r) $ gpretty <$> allProgs
    where
      allProgs = topologicalSortSubProg (SomePrettyProg prog)
