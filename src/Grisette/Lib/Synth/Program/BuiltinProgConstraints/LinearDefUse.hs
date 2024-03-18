{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Program.BuiltinProgConstraints.LinearDefUse
  ( Def (..),
    DefUnion,
    Use (..),
    UseUnion,
    LinearDefUse (..),
    LinearDefUseExtract (..),
    LinearDefUseName (..),
    LinearDefUseTypePredicate (..),
    LinearDefUseConstraint,
    concreteStmtDef,
    concreteStmtUse,
    componentStmtDefs,
    componentStmtUses,
    componentProgDefs,
    componentProgUses,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Except (MonadError (throwError))
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvaluateSym,
    LogicalOp (symImplies, symNot, (.&&), (.||)),
    Mergeable,
    MonadUnion,
    SEq ((.==)),
    Solvable (con),
    SymBool,
    UnionM,
    liftUnionM,
    mrgIf,
    mrgReturn,
    mrgTraverse,
    mrgTraverse_,
  )
import Grisette.Core.Data.Class.SOrd (SOrd ((.<=), (.>=)))
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Synth.Context (MonadContext)
import qualified Grisette.Lib.Synth.Program.ComponentSketch.Program as Component
import qualified Grisette.Lib.Synth.Program.Concrete.Program as Concrete
import Grisette.Lib.Synth.Program.ProgConstraints
  ( ProgConstraints (constrainProg),
  )
import Grisette.Lib.Synth.VarId (ConcreteVarId, SymbolicVarId)

data Def varId bool = Def
  { defId :: varId,
    defDisabled :: bool
  }
  deriving (Show, Eq, Generic)
  deriving (Mergeable, SEq, EvaluateSym) via (Default (Def varId bool))

type DefUnion varId = UnionM (Maybe (Def varId SymBool))

data Use varId bool = Use
  { effectiveDefId :: varId,
    mayUseId :: varId,
    useDisabled :: bool
  }
  deriving (Show, Eq, Generic)
  deriving (Mergeable, SEq, EvaluateSym) via (Default (Use varId bool))

type UseUnion varId = UnionM (Maybe (Use varId SymBool))

newtype LinearDefUse defUseObj = LinearDefUse defUseObj

class LinearDefUseName defUseObj where
  linearDefUseName :: defUseObj -> T.Text

class (LinearDefUseName defUseObj) => LinearDefUseExtract defUseObj op where
  linearDefUseExtractDefs ::
    defUseObj -> op -> [varId] -> bool -> Maybe (Def varId bool)
  linearDefUseExtractDefs = linearDefUseExtractInvalidatingDefs
  linearDefUseExtractInvalidatingDefs ::
    defUseObj -> op -> [varId] -> bool -> Maybe (Def varId bool)
  linearDefUseExtractInvalidatingDefs = linearDefUseExtractDefs
  linearDefUseExtractUses ::
    defUseObj -> op -> [varId] -> [varId] -> bool -> Maybe (Use varId bool)

class
  (LinearDefUseName defUseObj) =>
  LinearDefUseTypePredicate defUseObj ty
  where
  linearDefUseTypePredicate :: defUseObj -> ty -> Bool

type LinearDefUseConstraint defUseObj op ty =
  (LinearDefUseExtract defUseObj op, LinearDefUseTypePredicate defUseObj ty)

concreteStmtDef ::
  ( ConcreteVarId conVarId,
    LinearDefUseExtract defUseObj op
  ) =>
  defUseObj ->
  Concrete.Stmt op conVarId ->
  Maybe conVarId
concreteStmtDef defUseObj (Concrete.Stmt op _ resIds) =
  defId <$> linearDefUseExtractDefs defUseObj op resIds False

concreteStmtInvalidatingDef ::
  ( ConcreteVarId conVarId,
    LinearDefUseExtract defUseObj op
  ) =>
  defUseObj ->
  Concrete.Stmt op conVarId ->
  Maybe conVarId
concreteStmtInvalidatingDef defUseObj (Concrete.Stmt op _ resIds) =
  defId <$> linearDefUseExtractInvalidatingDefs defUseObj op resIds False

concreteStmtUse ::
  ( ConcreteVarId conVarId,
    LinearDefUseExtract defUseObj op
  ) =>
  defUseObj ->
  Concrete.Stmt op conVarId ->
  Maybe conVarId
concreteStmtUse defUseObj (Concrete.Stmt op argIds resIds) =
  mayUseId <$> linearDefUseExtractUses defUseObj op argIds resIds False

concreteProgOnlyUseNewestDef ::
  forall defUseObj op conVarId ty ctx.
  ( MonadContext ctx,
    ConcreteVarId conVarId,
    LinearDefUseConstraint defUseObj op ty
  ) =>
  defUseObj ->
  Concrete.Prog op conVarId ty ->
  ctx ()
concreteProgOnlyUseNewestDef
  defUseObj
  (Concrete.Prog _ argList stmtList resList) = do
    let argIds =
          Concrete.progArgId
            <$> filter
              (linearDefUseTypePredicate defUseObj . Concrete.progArgType)
              argList
    let resIds =
          Concrete.progResId
            <$> filter
              (linearDefUseTypePredicate defUseObj . Concrete.progResType)
              resList
    (finalInvalidatedId, finalIds) <- goStmts Nothing argIds stmtList
    if not (null finalIds) && isJust finalInvalidatedId
      then
        mrgTraverse_
          ( \resId ->
              if resId `elem` finalIds
                && resId < fromJust finalInvalidatedId
                then
                  mrgThrowError $
                    "Must use the newest definition of "
                      <> linearDefUseName defUseObj
                else mrgReturn ()
          )
          resIds
      else mrgReturn ()
    where
      goStmts ::
        Maybe conVarId ->
        [conVarId] ->
        [Concrete.Stmt op conVarId] ->
        ctx (Maybe conVarId, [conVarId])
      goStmts invalidatedId usedIds [] = mrgReturn (invalidatedId, usedIds)
      goStmts invalidatedId usedIds (stmt : rest) = do
        let def = concreteStmtDef defUseObj stmt
        let invalidatingDef = concreteStmtInvalidatingDef defUseObj stmt
        let use = concreteStmtUse defUseObj stmt
        if isJust use
          && isJust invalidatedId
          && fromJust use `elem` usedIds
          && fromJust use < fromJust invalidatedId
          then
            mrgThrowError $
              "Must use the newest definition of "
                <> linearDefUseName defUseObj
          else mrgReturn ()
        case def of
          Nothing -> goStmts (invalidatingDef <|> invalidatedId) usedIds rest
          Just defId ->
            goStmts
              (invalidatingDef <|> invalidatedId)
              (defId : usedIds)
              rest

instance
  ( MonadContext ctx,
    LinearDefUseConstraint defUseObj op ty,
    ConcreteVarId conVarId,
    Mergeable op,
    Mergeable ty
  ) =>
  ProgConstraints
    (LinearDefUse defUseObj)
    (Concrete.Prog op conVarId ty)
    ctx
  where
  constrainProg (LinearDefUse defUseObj) =
    concreteProgOnlyUseNewestDef defUseObj

componentStmtDefs ::
  ( SymbolicVarId symVarId,
    MonadUnion ctx,
    Mergeable op,
    LinearDefUseExtract defUseObj op
  ) =>
  defUseObj ->
  Component.Stmt op symVarId ty ->
  ctx [DefUnion symVarId]
componentStmtDefs defUseObj (Component.Stmt opUnion _ _ resIds _ disabled) = do
  op <- liftUnionM opUnion
  mrgReturn [mrgReturn $ linearDefUseExtractDefs defUseObj op resIds disabled]

componentStmtInvalidatingDefs ::
  ( SymbolicVarId symVarId,
    MonadUnion ctx,
    Mergeable op,
    LinearDefUseExtract defUseObj op
  ) =>
  defUseObj ->
  Component.Stmt op symVarId ty ->
  ctx [DefUnion symVarId]
componentStmtInvalidatingDefs
  defUseObj
  (Component.Stmt opUnion _ _ resIds _ disabled) = do
    op <- liftUnionM opUnion
    mrgReturn
      [ mrgReturn $
          linearDefUseExtractInvalidatingDefs defUseObj op resIds disabled
      ]

componentStmtUses ::
  ( SymbolicVarId symVarId,
    MonadUnion ctx,
    Mergeable op,
    LinearDefUseExtract defUseObj op
  ) =>
  defUseObj ->
  Component.Stmt op symVarId ty ->
  ctx [UseUnion symVarId]
componentStmtUses
  defUseObj
  (Component.Stmt opUnion argIds _ resIds _ disabled) = do
    op <- liftUnionM opUnion
    mrgReturn
      [mrgReturn $ linearDefUseExtractUses defUseObj op argIds resIds disabled]

componentProgDefs ::
  ( SymbolicVarId symVarId,
    MonadContext ctx,
    MonadUnion ctx,
    Mergeable op,
    LinearDefUseConstraint defUseObj op ty
  ) =>
  defUseObj ->
  Component.Prog op symVarId ty ->
  ctx [DefUnion symVarId]
componentProgDefs
  defUseObj
  (Component.Prog _ argList stmtList _) = do
    stmtDefs <- mrgTraverse (componentStmtDefs defUseObj) stmtList
    mrgReturn $ concat (argDefs : stmtDefs)
    where
      argDefs =
        fmap (\(i, _) -> mrgReturn $ Just $ Def (fromIntegral i) (con False))
          $ filter
            (linearDefUseTypePredicate defUseObj . Component.progArgType . snd)
          $ zip [0 ..] argList

componentProgInvalidatingDefs ::
  ( SymbolicVarId symVarId,
    MonadContext ctx,
    MonadUnion ctx,
    Mergeable op,
    LinearDefUseConstraint defUseObj op ty
  ) =>
  defUseObj ->
  Component.Prog op symVarId ty ->
  ctx [DefUnion symVarId]
componentProgInvalidatingDefs
  defUseObj
  (Component.Prog _ _ stmtList _) = do
    stmtDefs <- mrgTraverse (componentStmtInvalidatingDefs defUseObj) stmtList
    mrgReturn $ concat stmtDefs

componentProgUses ::
  ( SymbolicVarId symVarId,
    MonadContext ctx,
    MonadUnion ctx,
    Mergeable op,
    LinearDefUseConstraint defUseObj op ty
  ) =>
  defUseObj ->
  Component.Prog op symVarId ty ->
  ctx [UseUnion symVarId]
componentProgUses
  defUseObj
  (Component.Prog _ argList stmtList resList) = do
    stmtUses <- mrgTraverse (componentStmtUses defUseObj) stmtList
    mrgReturn $ concat (resUses : stmtUses)
    where
      resEffectiveDefId = fromIntegral $ length argList + length stmtList
      resUses =
        ( \r ->
            mrgReturn $
              Just $
                Use resEffectiveDefId (Component.progResId r) (con False)
        )
          <$> filter
            (linearDefUseTypePredicate defUseObj . Component.progResType)
            resList

componentProgOnlyUseNewestDef ::
  forall defUseObj op symVarId ty ctx.
  ( MonadContext ctx,
    MonadUnion ctx,
    Mergeable op,
    SymbolicVarId symVarId,
    LinearDefUseConstraint defUseObj op ty
  ) =>
  defUseObj ->
  Component.Prog op symVarId ty ->
  ctx ()
componentProgOnlyUseNewestDef defUseObj sketch =
  mrgTraverse_
    ( \useUnion -> extractUnionMaybe useUnion $
        \use -> do
          invalidatingDefIds <- invalidatingDefs
          mrgTraverse_
            (\d -> defUseConstraint use d invalidatingDefIds)
            =<< defs
    )
    =<< uses
  where
    extractUnionMaybe ::
      (Mergeable a) => UnionM (Maybe a) -> (a -> ctx ()) -> ctx ()
    extractUnionMaybe union f = do
      maybeValue <- liftUnionM union
      case maybeValue of
        Nothing -> mrgReturn ()
        Just a -> f a
    invalidatingDefs = componentProgInvalidatingDefs defUseObj sketch
    uses = componentProgUses defUseObj sketch
    defs = componentProgDefs defUseObj sketch
    defUseConstraint ::
      Use symVarId SymBool -> DefUnion symVarId -> [DefUnion symVarId] -> ctx ()
    defUseConstraint
      (Use eDefId useId disabled)
      defUnion
      invalidatingDefIds = extractUnionMaybe defUnion $
        \(Def defId _disabled) ->
          mrgTraverse_
            ( \invalidatingDefUnion -> extractUnionMaybe invalidatingDefUnion $
                \(Def invalidateDefId invalidateDisabled) ->
                  mrgIf
                    ( symImplies
                        ( symNot disabled
                            .&& (useId .== defId)
                            .&& symNot invalidateDisabled
                        )
                        ( (invalidateDefId .<= useId)
                            .|| (invalidateDefId .>= eDefId)
                        )
                    )
                    (return ())
                    ( throwError $
                        "Must use the newest definition of "
                          <> linearDefUseName defUseObj
                    )
            )
            invalidatingDefIds

instance
  ( MonadContext ctx,
    LinearDefUseConstraint defUseObj op ty,
    MonadUnion ctx,
    SymbolicVarId symVarId,
    Mergeable op,
    Mergeable ty
  ) =>
  ProgConstraints
    (LinearDefUse defUseObj)
    (Component.Prog op symVarId ty)
    ctx
  where
  constrainProg (LinearDefUse defUseObj) =
    componentProgOnlyUseNewestDef defUseObj
