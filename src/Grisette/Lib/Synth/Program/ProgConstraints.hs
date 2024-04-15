{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Lib.Synth.Program.ProgConstraints
  ( ProgConstraints (..),
    WithConstraints (..),
    addPathLocalIdent,
    runProgWithConstraints,
    OpSubProgConstraints (..),
    ConstraintHierarchy (..),
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

import Control.DeepSeq (NFData (rnf))
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.Text as T
import Data.Typeable
  ( Proxy (Proxy),
    Typeable,
    cast,
    eqT,
    typeRep,
    type (:~:) (Refl),
  )
import Grisette
  ( Identifier (Identifier, IdentifierWithInfo),
    Mergeable,
    MonadFresh (localIdentifier),
    MonadUnion,
    TryMerge,
    UnionM,
    liftUnionM,
    mrgReturn,
    tryMerge,
  )
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Program.ProgNaming (ProgNaming (nameProg))
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Util.Show (showText)
import Language.Haskell.TH.Syntax (Lift (liftTyped))

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
  OpSubProgConstraints constObj (UnionM op) ctx
  where
  constrainOpSubProg constObj op =
    liftUnionM op >>= constrainOpSubProg constObj

data ConstraintHierarchy where
  ConstraintHierarchy ::
    ( Typeable info,
      Ord info,
      Lift info,
      NFData info,
      Show info,
      Hashable info
    ) =>
    { constraintHierarchyPath :: [T.Text],
      constraintKind :: T.Text,
      constraintOriginalInfo :: info
    } ->
    ConstraintHierarchy

instance Eq ConstraintHierarchy where
  ConstraintHierarchy path1 kind1 (info1 :: info1)
    == ConstraintHierarchy path2 kind2 (info2 :: info2) =
      case eqT @info1 @info2 of
        Nothing -> False
        Just Refl -> path1 == path2 && kind1 == kind2 && info1 == info2

instance Ord ConstraintHierarchy where
  ConstraintHierarchy path1 kind1 (info1 :: info1)
    <= ConstraintHierarchy path2 kind2 (info2 :: info2) =
      case eqT @info1 @info2 of
        Nothing ->
          path1 <= path2
            && kind1 <= kind2
            && typeRep (Proxy @info1) <= typeRep (Proxy @info2)
        Just Refl -> path1 <= path2 && kind1 <= kind2 && info1 <= info2

instance Lift ConstraintHierarchy where
  liftTyped (ConstraintHierarchy path kind info) =
    [||ConstraintHierarchy path kind info||]

instance Hashable ConstraintHierarchy where
  hashWithSalt salt (ConstraintHierarchy path kind info) =
    salt
      `hashWithSalt` path
      `hashWithSalt` kind
      `hashWithSalt` info

instance NFData ConstraintHierarchy where
  rnf (ConstraintHierarchy path kind info) =
    rnf path `seq` rnf kind `seq` rnf info

instance Show ConstraintHierarchy where
  show (ConstraintHierarchy path kind info) =
    T.unpack $ T.intercalate "/" $ reverse path <> [kind] <> [showText info]

setConstraintKind ::
  (MonadFresh m, TryMerge m, Mergeable a) => T.Text -> m a -> m a
setConstraintKind kind =
  tryMerge
    . localIdentifier
      ( \case
          ident@(Identifier _) ->
            error $
              "setConstraintKind: Identifier "
                <> show ident
                <> " does not have ConstraintHierarchy info"
          IdentifierWithInfo ident info ->
            case cast info of
              Just (ConstraintHierarchy paths _ info) ->
                IdentifierWithInfo ident (ConstraintHierarchy paths kind info)
              Nothing ->
                error $
                  "setConstraintKind: IdentifierWithInfo "
                    <> show ident
                    <> " does not have ConstraintHierarchy info"
      )

addPathLocalIdent ::
  (MonadFresh m, TryMerge m, Mergeable a) => T.Text -> m a -> m a
addPathLocalIdent path =
  tryMerge
    . localIdentifier
      ( \case
          ident@(Identifier _) ->
            IdentifierWithInfo ident (ConstraintHierarchy [path] "" ())
          IdentifierWithInfo ident info ->
            case cast info of
              Just (ConstraintHierarchy paths kind info) ->
                IdentifierWithInfo
                  ident
                  (ConstraintHierarchy (path : paths) kind info)
              Nothing ->
                IdentifierWithInfo
                  ident
                  (ConstraintHierarchy [path] "" $ Just info)
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
