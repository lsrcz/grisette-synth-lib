{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Grisette.Lib.Synth.Program.ProgConstraints
  ( ProgConstraints (..),
    WithConstraints (..),
    addPathLocalIdent,
    runProgWithConstraints,
    OpSubProgConstraints (..),
    pattern ConstraintHierarchy,
    setConstraintKind,
    progNameArgLocalIdent,
    progArgLocalIdent,
    progNameStmtLocalIdent,
    progStmtLocalIdent,
    progNameStmtSubProgLocalIdent,
    progStmtSubProgLocalIdent,
    progNameResLocalIdent,
    progResLocalIdent,
  )
where

import Control.DeepSeq (NFData)
import qualified Data.Binary as Binary
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Hashable (Hashable)
import qualified Data.Serialize as Cereal
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( Identifier (Identifier),
    Mergeable,
    MonadFresh (localIdentifier),
    MonadUnion,
    SExpr (Atom, List),
    TryMerge,
    Union,
    liftUnion,
    mrgReturn,
    tryMerge,
  )
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Program.ProgNaming (ProgNaming (nameProg))
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Util.Show (showText)

class (MonadContext ctx) => ProgConstraints constObj prog ctx where
  constrainProg :: constObj -> prog -> ctx ()

instance (MonadContext ctx) => ProgConstraints () prog ctx where
  constrainProg _ _ = mrgReturn ()

instance
  ( ProgConstraints constObj1 prog ctx,
    ProgConstraints constObj2 prog ctx
  ) =>
  ProgConstraints (constObj1, constObj2) prog ctx
  where
  constrainProg (obj1, obj2) prog = do
    constrainProg obj1 prog
    constrainProg obj2 prog

instance
  ( ProgConstraints constObj1 prog ctx,
    ProgConstraints constObj2 prog ctx,
    ProgConstraints constObj3 prog ctx
  ) =>
  ProgConstraints (constObj1, constObj2, constObj3) prog ctx
  where
  constrainProg (obj1, obj2, obj3) = constrainProg (obj1, (obj2, obj3))

instance
  ( ProgConstraints constObj1 prog ctx,
    ProgConstraints constObj2 prog ctx,
    ProgConstraints constObj3 prog ctx,
    ProgConstraints constObj4 prog ctx
  ) =>
  ProgConstraints (constObj1, constObj2, constObj3, constObj4) prog ctx
  where
  constrainProg (obj1, obj2, obj3, obj4) =
    constrainProg (obj1, (obj2, obj3, obj4))

data WithConstraints semObj constObj = WithConstraints semObj constObj
  deriving (Eq, Generic)
  deriving anyclass (Serial, NFData, Hashable)

instance
  (Serial semObj, Serial constObj) =>
  Cereal.Serialize (WithConstraints semObj constObj)
  where
  put = serialize
  get = deserialize

instance
  (Serial semObj, Serial constObj) =>
  Binary.Binary (WithConstraints semObj constObj)
  where
  put = serialize
  get = deserialize

runProgWithConstraints ::
  (ProgConstraints constObj prog ctx, ProgSemantics semObj prog val ctx) =>
  WithConstraints semObj constObj ->
  prog ->
  [val] ->
  ctx [val]
runProgWithConstraints (WithConstraints semObj constObj) prog inputs = do
  constrainProg constObj prog
  runProg semObj prog inputs

class (MonadContext ctx) => OpSubProgConstraints constObj op ctx where
  constrainOpSubProg :: constObj -> op -> ctx ()
  constrainOpSubProg _ _ = mrgReturn ()

instance
  (MonadUnion ctx, OpSubProgConstraints constObj op ctx, Mergeable op) =>
  OpSubProgConstraints constObj (Union op) ctx
  where
  constrainOpSubProg constObj op =
    liftUnion op >>= constrainOpSubProg constObj

atomView :: SExpr -> Maybe T.Text
atomView (Atom x) = Just x
atomView _ = Nothing

viewConstraintHierarchy :: SExpr -> Maybe ([T.Text], T.Text, SExpr)
viewConstraintHierarchy
  (List [Atom "grisette-synth-hierarchy", List paths, Atom kind, info]) = do
    paths <- traverse atomView paths
    Just (paths, kind, info)
viewConstraintHierarchy _ = Nothing

-- ConstraintHierarchy
-- (grisette-synth-hierarchy (listof paths) kind info)
pattern ConstraintHierarchy :: [T.Text] -> T.Text -> SExpr -> SExpr
pattern ConstraintHierarchy paths kind info <-
  (viewConstraintHierarchy -> Just (paths, kind, info))
  where
    ConstraintHierarchy paths kind info =
      List
        [ Atom "grisette-synth-hierarchy",
          List $ Atom <$> paths,
          Atom kind,
          info
        ]

setConstraintKind ::
  (MonadFresh m, TryMerge m, Mergeable a) => T.Text -> m a -> m a
setConstraintKind kind =
  tryMerge
    . localIdentifier
      ( \case
          (Identifier sym (ConstraintHierarchy paths _ info)) ->
            Identifier sym (ConstraintHierarchy paths kind info)
          ident ->
            error $
              "setConstraintKind: Identifier "
                <> show ident
                <> " does not have ConstraintHierarchy info"
      )

addPathLocalIdent ::
  (MonadFresh m, TryMerge m, Mergeable a) => T.Text -> m a -> m a
addPathLocalIdent path =
  tryMerge
    . localIdentifier
      ( \case
          ( Identifier
              sym
              (ConstraintHierarchy paths kind info)
            ) ->
              Identifier sym $ ConstraintHierarchy (path : paths) kind info
          (Identifier sym info) ->
            Identifier sym $ ConstraintHierarchy [path] "" info
      )

progNameArgLocalIdent ::
  (MonadFresh m, TryMerge m, Mergeable a) => T.Text -> m a -> m a
progNameArgLocalIdent progName = addPathLocalIdent (progName <> ":arg")

progArgLocalIdent ::
  (ProgNaming prog, MonadFresh m, TryMerge m, Mergeable a) =>
  prog ->
  m a ->
  m a
progArgLocalIdent = progNameArgLocalIdent . nameProg

progNameStmtLocalIdent ::
  (MonadFresh m, TryMerge m, Mergeable a) => T.Text -> Int -> m a -> m a
progNameStmtLocalIdent progName stmtIndex =
  addPathLocalIdent (progName <> ":stmt" <> showText stmtIndex)

progStmtLocalIdent ::
  (ProgNaming prog, MonadFresh m, TryMerge m, Mergeable a) =>
  prog ->
  Int ->
  m a ->
  m a
progStmtLocalIdent = progNameStmtLocalIdent . nameProg

progNameStmtSubProgLocalIdent ::
  (MonadFresh m, TryMerge m, Mergeable a) => T.Text -> Int -> Int -> m a -> m a
progNameStmtSubProgLocalIdent progName stmtIndex subProgIndex =
  addPathLocalIdent $
    progName
      <> ":stmt"
      <> showText stmtIndex
      <> ":sub"
      <> showText subProgIndex

progStmtSubProgLocalIdent ::
  (ProgNaming prog, MonadFresh m, TryMerge m, Mergeable a) =>
  prog ->
  Int ->
  Int ->
  m a ->
  m a
progStmtSubProgLocalIdent = progNameStmtSubProgLocalIdent . nameProg

progNameResLocalIdent ::
  (MonadFresh m, TryMerge m, Mergeable a) => T.Text -> m a -> m a
progNameResLocalIdent progName =
  addPathLocalIdent (progName <> ":res")

progResLocalIdent ::
  (ProgNaming prog, MonadFresh m, TryMerge m, Mergeable a) =>
  prog ->
  m a ->
  m a
progResLocalIdent = progNameResLocalIdent . nameProg
