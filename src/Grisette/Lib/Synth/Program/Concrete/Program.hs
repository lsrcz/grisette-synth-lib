{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :    Grisette.Lib.Synth.Program.Concrete
-- Copyright   :    (c) Sirui Lu 2024
-- License     :    BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :    siruilu@cs.washington.edu
-- Stability   :    experimental
-- Portability :    GHC only
--
-- Do not use plain concrete programs on operators that may generate multiple
-- results that cannot be merged into a single result, or there can be
-- significant performance issues.
--
-- Use the `ProgMayMultiPath` wrapper to allow multiple results. However, note
-- that it is generally not a good idea to use non-simply-mergeable types as
-- results, as it can lead to exponential blowup in the number of results.
module Grisette.Lib.Synth.Program.Concrete.Program
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

import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT,
    evalStateT,
  )
import Data.Foldable (traverse_)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import qualified Data.Map.Ordered as OM
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    GPretty (gpretty),
    Mergeable (rootStrategy),
    MergingStrategy (NoStrategy),
    tryMerge,
  )
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Operator.OpPretty
  ( OpPretty,
    OpPrettyError,
    VarIdMap,
    prettyArguments,
    prettyResults,
  )
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Program.ProgNaming (ProgNaming (nameProg))
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Program.ProgTyping (ProgTyping (typeProg))
import Grisette.Lib.Synth.TypeSignature
  ( TypeSignature (TypeSignature),
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
import Grisette.Lib.Synth.Util.Show (showText)
import Grisette.Lib.Synth.VarId (ConcreteVarId)

data Stmt op varId = Stmt
  { stmtOp :: op,
    stmtArgIds :: [varId],
    stmtResIds :: [varId]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable)

instance Mergeable (Stmt op varId) where
  rootStrategy = NoStrategy

data ProgArg varId ty = ProgArg
  { progArgName :: T.Text,
    progArgId :: varId,
    progArgType :: ty
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable)

data ProgRes varId ty = ProgRes
  { progResId :: varId,
    progResType :: ty
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable)

data Prog op varId ty = Prog
  { progName :: T.Text,
    progArgList :: [ProgArg varId ty],
    progStmtList :: [Stmt op varId],
    progResList :: [ProgRes varId ty]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable)

instance Mergeable (Prog op varId ty) where
  rootStrategy = NoStrategy

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

type Env varId val = HM.HashMap varId val

addVal ::
  (ConcreteVarId varId, MonadContext ctx) =>
  varId ->
  val ->
  StateT (Env varId val) ctx ()
addVal varId val = do
  env <- get
  when (HM.member varId env) . throwError $
    "Variable " <> showText varId <> " is already defined."
  put $ HM.insert varId val env

lookupVal ::
  (ConcreteVarId varId, MonadContext ctx) =>
  varId ->
  StateT (Env varId val) ctx val
lookupVal varId = do
  env <- get
  case HM.lookup varId env of
    Nothing -> throwError $ "Variable " <> showText varId <> " is undefined."
    Just val -> return val

instance
  ( OpSemantics semObj op val ctx,
    ConcreteVarId varId,
    Mergeable val
  ) =>
  ProgSemantics semObj (Prog op varId ty) val ctx
  where
  runProg sem (Prog _ arg stmts ret) inputs = tryMerge $ do
    when (length inputs /= length arg) . throwError $
      "Expected "
        <> showText (length arg)
        <> " arguments, but got "
        <> showText (length inputs)
        <> " arguments."
    let initialEnv = HM.fromList $ zip (progArgId <$> arg) inputs
    let runStmt (Stmt op argIds resIds) = do
          args <- traverse lookupVal argIds
          res <- lift $ applyOp sem op args
          when (length res /= length resIds) . throwError $
            "Incorrect number of results."
          traverse_ (uncurry addVal) $ zip resIds res
    flip evalStateT initialEnv $ do
      traverse_ runStmt stmts
      traverse (lookupVal . progResId) ret

instance (Mergeable ty) => ProgTyping semObj (Prog op varId ty) ty where
  typeProg _ prog =
    mrgReturn $
      TypeSignature
        (progArgType <$> progArgList prog)
        (progResType <$> progResList prog)

instance ProgNaming (Prog op varId ty) where
  nameProg = progName
