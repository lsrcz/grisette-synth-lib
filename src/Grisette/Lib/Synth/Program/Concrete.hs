{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.Concrete
  ( Stmt (..),
    ProgArg (..),
    ProgRes (..),
    Prog (..),
    ProgPrettyError (..),
    prettyStmt,
    prettyProg,
  )
where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State (MonadState (get, put), StateT, evalStateT)
import qualified Data.HashMap.Lazy as HM
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

prettyStmt ::
  (ConcreteVarId varId, OpPretty sem op, GPretty op) =>
  sem ->
  Int ->
  Stmt op varId ->
  StateT (VarIdMap varId) (Either (ProgPrettyError op varId)) (Doc ann)
prettyStmt sem index stmt@(Stmt op argIds resIds) = do
  map <- get
  argPretty <- case prettyArguments sem op argIds map of
    Left err -> throwError $ StmtPrettyError stmt index err
    Right argPretty -> pure argPretty
  let opPretty = gpretty op
  (newMap, resPretty) <- case prettyResults sem op (length argIds) resIds map of
    Left err -> throwError $ StmtPrettyError stmt index err
    Right resPretty -> pure resPretty
  put newMap
  return $ resPretty <> " = " <> opPretty <> argPretty

prettyProg ::
  (ConcreteVarId varId, OpPretty sem op, GPretty op, GPretty ty) =>
  sem ->
  Prog op varId ty ->
  Either (ProgPrettyError op varId) (Doc ann)
prettyProg sem (Prog name argList stmtList resList) = do
  let initMap =
        HM.fromList $ map (\arg -> (progArgId arg, progArgName arg)) argList
  flip evalStateT initMap $ do
    stmtsPretty <- traverse (uncurry $ prettyStmt sem) (zip [0 ..] stmtList)
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
