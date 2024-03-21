{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Program.BuiltinProgConstraints.Liveliness
  ( Def (..),
    UnionDef,
    Use (..),
    UnionUse,
    ComponentUse (..),
    UnionComponentUse,
    Liveliness (..),
    LivelinessOpResource (..),
    LivelinessName (..),
    LivelinessTypeResource (..),
    Resource (..),
    LivelinessConstraint,
    livelinessOpDefsByType,
    livelinessTypeDefs,
    livelinessOpUsesByType,
    livelinessTypeUses,
    concreteStmtDef,
    concreteStmtUse,
    componentStmtDefs,
    componentStmtInvalidatingDefs,
    componentStmtUses,
    componentProgDefs,
    componentProgInvalidatingDefs,
    componentProgUses,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Maybe (isJust, mapMaybe)
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
    ToSym (toSym),
    UnionM,
    liftUnionM,
    mrgFmap,
    mrgReturn,
    mrgSequence_,
    mrgTraverse,
    mrgTraverse_,
    simpleMerge,
    symOr,
    symUnless,
  )
import Grisette.Core.Data.Class.SOrd (SOrd ((.<=), (.>=)))
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Operator.OpTyping (OpTyping (typeOp))
import qualified Grisette.Lib.Synth.Program.ComponentSketch.Program as Component
import qualified Grisette.Lib.Synth.Program.Concrete.Program as Concrete
import Grisette.Lib.Synth.Program.ProgConstraints
  ( ProgConstraints (constrainProg),
  )
import Grisette.Lib.Synth.TypeSignature (TypeSignature (argTypes, resTypes))
import Grisette.Lib.Synth.VarId (ConcreteVarId, SymbolicVarId)

class (Mergeable res) => Resource res where
  conflict :: res -> res -> SymBool

data Def varId res = Def
  { defId :: varId,
    defResource :: res,
    defDisabled :: SymBool
  }
  deriving (Show, Eq, Generic)
  deriving (Mergeable, Grisette.SEq, EvaluateSym) via (Default (Def varId res))

data Use varId = Use {useId :: varId, useDisabled :: SymBool}
  deriving (Show, Eq, Generic)
  deriving (Mergeable, Grisette.SEq, EvaluateSym) via (Default (Use varId))

newtype Liveliness livelinessObj = Liveliness livelinessObj

class LivelinessName defUseObj where
  livelinessName :: defUseObj -> T.Text

livelinessTypeDefs ::
  ( Mergeable varId,
    LivelinessTypeResource defUseObj res ty,
    MonadContext ctx
  ) =>
  defUseObj ->
  [ty] ->
  [varId] ->
  SymBool ->
  ctx [Def varId res]
livelinessTypeDefs defUseObj tys resIds disabled = do
  mrgReturn
    $ mapMaybe
      ( \(id, ty) ->
          (\res -> Def id res disabled) <$> livelinessTypeResource defUseObj ty
      )
    $ zip resIds tys

livelinessOpDefsByType ::
  ( Mergeable varId,
    OpTyping op ty ctx,
    LivelinessTypeResource defUseObj res ty
  ) =>
  defUseObj ->
  op ->
  [varId] ->
  SymBool ->
  ctx [Def varId res]
livelinessOpDefsByType defUseObj op resIds disabled = do
  ty <- typeOp op
  livelinessTypeDefs defUseObj (resTypes ty) resIds disabled

livelinessTypeUses ::
  ( Mergeable varId,
    LivelinessTypeResource defUseObj res ty,
    MonadContext ctx
  ) =>
  defUseObj ->
  [ty] ->
  [varId] ->
  SymBool ->
  ctx [Use varId]
livelinessTypeUses defUseObj tys argIds disabled = do
  mrgReturn
    $ mapMaybe
      ( \(id, ty) ->
          Use id disabled <$ livelinessTypeResource defUseObj ty
      )
    $ zip argIds tys

livelinessOpUsesByType ::
  ( Mergeable varId,
    OpTyping op ty ctx,
    LivelinessTypeResource defUseObj res ty
  ) =>
  defUseObj ->
  op ->
  [varId] ->
  SymBool ->
  ctx [Use varId]
livelinessOpUsesByType defUseObj op argIds disabled = do
  ty <- typeOp op
  livelinessTypeUses defUseObj (argTypes ty) argIds disabled

class
  (LivelinessName defUseObj, MonadContext ctx, Resource res) =>
  LivelinessOpResource defUseObj op res ctx
    | defUseObj -> res
  where
  livelinessOpDefs ::
    (Mergeable varId) =>
    defUseObj ->
    op ->
    [varId] ->
    SymBool ->
    ctx [Def varId res]
  default livelinessOpDefs ::
    ( Mergeable varId,
      OpTyping op ty ctx,
      LivelinessTypeResource defUseObj res ty
    ) =>
    defUseObj ->
    op ->
    [varId] ->
    SymBool ->
    ctx [Def varId res]
  livelinessOpDefs = livelinessOpDefsByType
  livelinessOpInvalidatingDefs ::
    (Mergeable varId) =>
    defUseObj ->
    op ->
    [varId] ->
    SymBool ->
    ctx [Def varId res]
  livelinessOpInvalidatingDefs = livelinessOpDefs
  livelinessOpUses ::
    (Mergeable varId) =>
    defUseObj ->
    op ->
    [varId] ->
    SymBool ->
    ctx [Use varId]
  default livelinessOpUses ::
    ( Mergeable varId,
      OpTyping op ty ctx,
      LivelinessTypeResource defUseObj res ty
    ) =>
    defUseObj ->
    op ->
    [varId] ->
    SymBool ->
    ctx [Use varId]
  livelinessOpUses = livelinessOpUsesByType

class
  (Resource res) =>
  LivelinessTypeResource defUseObj res ty
    | defUseObj -> res
  where
  livelinessTypeResource :: defUseObj -> ty -> Maybe res

type LivelinessConstraint defUseObj op ty res ctx =
  ( LivelinessOpResource defUseObj op res ctx,
    LivelinessTypeResource defUseObj res ty,
    MonadUnion ctx,
    Mergeable op
  )

type UnionDef varId res = UnionM [Def varId res]

type UnionUse varId = UnionM [Use varId]

concreteStmtDef ::
  (ConcreteVarId conVarId, LivelinessOpResource defUseObj op res ctx) =>
  defUseObj ->
  Concrete.Stmt op conVarId ->
  ctx (UnionDef conVarId res)
concreteStmtDef defUseObj (Concrete.Stmt op _ resIds) =
  mrgFmap mrgReturn $ livelinessOpDefs defUseObj op resIds (con False)

concreteStmtInvalidatingDef ::
  (ConcreteVarId conVarId, LivelinessOpResource defUseObj op res ctx) =>
  defUseObj ->
  Concrete.Stmt op conVarId ->
  ctx (UnionDef conVarId res)
concreteStmtInvalidatingDef defUseObj (Concrete.Stmt op _ resIds) =
  mrgFmap mrgReturn $
    livelinessOpInvalidatingDefs defUseObj op resIds (con False)

concreteStmtUse ::
  (ConcreteVarId conVarId, LivelinessOpResource defUseObj op res ctx) =>
  defUseObj ->
  Concrete.Stmt op conVarId ->
  ctx (UnionUse conVarId)
concreteStmtUse defUseObj (Concrete.Stmt op argIds _) =
  mrgFmap mrgReturn $ livelinessOpUses defUseObj op argIds (con False)

concreteArgDefs ::
  ( ConcreteVarId conVarId,
    LivelinessTypeResource defUseObj res ty,
    Mergeable res
  ) =>
  defUseObj ->
  [Concrete.ProgArg conVarId ty] ->
  UnionDef conVarId res
concreteArgDefs defUseObj =
  mrgReturn
    . mapMaybe
      ( \(Concrete.ProgArg _ argId argType) ->
          (\res -> Def argId res (con False))
            <$> livelinessTypeResource defUseObj argType
      )

concreteResUses ::
  (ConcreteVarId conVarId, LivelinessTypeResource defUseObj res ty) =>
  defUseObj ->
  [Concrete.ProgRes conVarId ty] ->
  UnionUse conVarId
concreteResUses defUseObj =
  mrgReturn
    . mapMaybe
      ( \(Concrete.ProgRes resId resType) ->
          livelinessTypeResource defUseObj resType
            *> Just (Use resId (con False))
      )

concreteProgOnlyUseNewestDef ::
  forall defUseObj op conVarId ty ctx res.
  (ConcreteVarId conVarId, LivelinessConstraint defUseObj op ty res ctx) =>
  Liveliness defUseObj ->
  Concrete.Prog op conVarId ty ->
  ctx ()
concreteProgOnlyUseNewestDef
  (Liveliness defUseObj)
  (Concrete.Prog _ argList stmtList resList) = do
    let argDefs = concreteArgDefs defUseObj argList
    let resUses = concreteResUses defUseObj resList
    availableDefs <- goStmts [(argDefs, toSym False)] stmtList
    cannotUseInvalidated resUses availableDefs
    where
      invalidateList :: [Def conVarId res] -> [Def conVarId res] -> SymBool
      invalidateList invalidatingDefs defs =
        symOr
          [ conflict (defResource invalidatingDef) (defResource def)
            | invalidatingDef <- invalidatingDefs,
              def <- defs
          ]
      invalidateUnion ::
        UnionDef conVarId res -> UnionDef conVarId res -> SymBool
      invalidateUnion unionInvalidatingDef unionDef =
        simpleMerge $ invalidateList <$> unionInvalidatingDef <*> unionDef
      invalidate ::
        UnionDef conVarId res ->
        [(UnionDef conVarId res, SymBool)] ->
        [(UnionDef conVarId res, SymBool)]
      invalidate invalidatingDef =
        fmap
          ( \(def, invalidated) ->
              (def, invalidated .|| invalidateUnion invalidatingDef def)
          )
      cannotUseInvalidatedSingle ::
        Use conVarId -> Def conVarId res -> SymBool -> ctx ()
      cannotUseInvalidatedSingle use def invalidated = do
        symUnless
          ( useDisabled use
              .|| defDisabled def
              .|| (con (useId use == defId def) `symImplies` symNot invalidated)
          )
          $ mrgThrowError
          $ "Cannot use invalidated resource for " <> livelinessName defUseObj
      cannotUseInvalidatedList ::
        [Use conVarId] -> [Def conVarId res] -> SymBool -> ctx ()
      cannotUseInvalidatedList uses defs invalidated =
        mrgSequence_
          [ cannotUseInvalidatedSingle use def invalidated
            | use <- uses,
              def <- defs
          ]
      cannotUseInvalidatedUnion ::
        UnionUse conVarId -> UnionDef conVarId res -> SymBool -> ctx ()
      cannotUseInvalidatedUnion useUnion defUnion invalidated = do
        uses <- liftUnionM useUnion
        defs <- liftUnionM defUnion
        cannotUseInvalidatedList uses defs invalidated
      cannotUseInvalidated ::
        UnionUse conVarId -> [(UnionDef conVarId res, SymBool)] -> ctx ()
      cannotUseInvalidated uses =
        mrgTraverse_ (uncurry (cannotUseInvalidatedUnion uses))
      goStmts ::
        [(UnionDef conVarId res, SymBool)] ->
        [Concrete.Stmt op conVarId] ->
        ctx [(UnionDef conVarId res, SymBool)]
      goStmts allDefs [] = mrgReturn allDefs
      goStmts allDefs (stmt : rest) = do
        uses <- concreteStmtUse defUseObj stmt
        cannotUseInvalidated uses allDefs
        invalidatingDef <- concreteStmtInvalidatingDef defUseObj stmt
        let invalidatedDefs = invalidate invalidatingDef allDefs
        def <- concreteStmtDef defUseObj stmt
        let newDefs = (def, toSym False) : invalidatedDefs
        goStmts newDefs rest

instance
  (LivelinessConstraint defUseObj op ty res ctx, ConcreteVarId conVarId) =>
  ProgConstraints (Liveliness defUseObj) (Concrete.Prog op conVarId ty) ctx
  where
  constrainProg = concreteProgOnlyUseNewestDef

data ComponentUse varId = ComponentUse
  { componentUseId :: varId,
    componentUseEffectiveDefId :: varId,
    componentUseDisabled :: SymBool
  }
  deriving (Show, Eq, Generic)
  deriving
    (Mergeable, Grisette.SEq, EvaluateSym)
    via (Default (ComponentUse varId))

type UnionComponentUse varId = UnionM [ComponentUse varId]

componentStmtDefs ::
  ( SymbolicVarId symVarId,
    MonadUnion ctx,
    Mergeable op,
    Mergeable res,
    LivelinessOpResource defUseObj op res ctx
  ) =>
  defUseObj ->
  Component.Stmt op symVarId ty ->
  ctx (UnionDef symVarId res)
componentStmtDefs defUseObj (Component.Stmt opUnion _ _ resIds _ disabled) = do
  op <- liftUnionM opUnion
  mrgFmap mrgReturn $ livelinessOpDefs defUseObj op resIds disabled

componentStmtInvalidatingDefs ::
  ( SymbolicVarId symVarId,
    MonadUnion ctx,
    Mergeable op,
    Mergeable res,
    LivelinessOpResource defUseObj op res ctx
  ) =>
  defUseObj ->
  Component.Stmt op symVarId ty ->
  ctx (UnionDef symVarId res)
componentStmtInvalidatingDefs
  defUseObj
  (Component.Stmt opUnion _ _ resIds _ disabled) = do
    op <- liftUnionM opUnion
    mrgFmap mrgReturn $
      livelinessOpInvalidatingDefs defUseObj op resIds disabled

componentStmtUses ::
  ( SymbolicVarId symVarId,
    MonadUnion ctx,
    Mergeable op,
    LivelinessOpResource defUseObj op res ctx
  ) =>
  defUseObj ->
  Component.Stmt op symVarId ty ->
  ctx (UnionComponentUse symVarId)
componentStmtUses
  defUseObj
  (Component.Stmt opUnion argIds _ resIds _ disabled) = do
    op <- liftUnionM opUnion
    opUses <- livelinessOpUses defUseObj op argIds disabled
    mrgReturn . mrgReturn $
      (\(Use i useDisabled) -> ComponentUse i (head resIds) useDisabled)
        <$> opUses

componentProgDefs ::
  (SymbolicVarId symVarId, LivelinessConstraint defUseObj op ty res ctx) =>
  defUseObj ->
  Component.Prog op symVarId ty ->
  ctx [UnionDef symVarId res]
componentProgDefs
  defUseObj
  (Component.Prog _ argList stmtList _) = do
    stmtDefs <- mrgTraverse (componentStmtDefs defUseObj) stmtList
    mrgReturn $ argDefs : stmtDefs
    where
      argDefs =
        mrgReturn
          $ mapMaybe
            ( \(i, Component.ProgArg _ ty) -> do
                res <- livelinessTypeResource defUseObj ty
                return $ Def (fromIntegral i) res (con False)
            )
          $ zip [0 ..] argList

componentProgInvalidatingDefs ::
  (SymbolicVarId symVarId, LivelinessConstraint defUseObj op ty res ctx) =>
  defUseObj ->
  Component.Prog op symVarId ty ->
  ctx [UnionDef symVarId res]
componentProgInvalidatingDefs
  defUseObj
  (Component.Prog _ _ stmtList _) =
    mrgTraverse (componentStmtInvalidatingDefs defUseObj) stmtList

componentProgUses ::
  (SymbolicVarId symVarId, LivelinessConstraint defUseObj op ty res ctx) =>
  defUseObj ->
  Component.Prog op symVarId ty ->
  ctx [UnionComponentUse symVarId]
componentProgUses
  defUseObj
  (Component.Prog _ argList stmtList resList) = do
    stmtUses <- mrgTraverse (componentStmtUses defUseObj) stmtList
    mrgReturn $ resUses : stmtUses
    where
      resEffectiveDefId =
        fromIntegral $
          length argList + sum (length . Component.stmtResIds <$> stmtList)
      resUses =
        mrgReturn $
          (\useId -> ComponentUse useId resEffectiveDefId (con False))
            . Component.progResId
            <$> filter
              ( isJust
                  . livelinessTypeResource defUseObj
                  . Component.progResType
              )
              resList

componentProgOnlyUseNewestDef ::
  forall defUseObj op symVarId ty ctx res.
  (SymbolicVarId symVarId, LivelinessConstraint defUseObj op ty res ctx) =>
  defUseObj ->
  Component.Prog op symVarId ty ->
  ctx ()
componentProgOnlyUseNewestDef defUseObj sketch =
  mrgTraverse_
    ( \useUnion -> traverseUnion useUnion $ \use -> do
        invalidatingDefIds <- invalidatingDefs
        mrgTraverse_ (\d -> defUseConstraint use d invalidatingDefIds) =<< defs
    )
    =<< uses
  where
    traverseUnion ::
      (Mergeable a) => UnionM [a] -> (a -> ctx ()) -> ctx ()
    traverseUnion union f = liftUnionM union >>= mrgTraverse_ f
    invalidatingDefs = componentProgInvalidatingDefs defUseObj sketch
    uses = componentProgUses defUseObj sketch
    defs = componentProgDefs defUseObj sketch
    defUseConstraint ::
      ComponentUse symVarId ->
      UnionDef symVarId res ->
      [UnionDef symVarId res] ->
      ctx ()
    defUseConstraint
      (ComponentUse useId eDefId disabled)
      defUnion
      invalidatingDefIds = traverseUnion defUnion $
        \(Def defId res _disabled) ->
          mrgTraverse_
            ( \invalidatingDefUnion ->
                traverseUnion invalidatingDefUnion $
                  \(Def invalidateDefId invalidateRes invalidateDisabled) ->
                    symUnless
                      ( symImplies
                          ( symNot disabled
                              .&& (useId .== defId)
                              .&& symNot invalidateDisabled
                              .&& conflict res invalidateRes
                          )
                          ( (invalidateDefId .<= useId)
                              .|| (invalidateDefId .>= eDefId)
                          )
                      )
                      $ throwError
                      $ "Cannot use invalidated resource for "
                        <> livelinessName defUseObj
            )
            invalidatingDefIds

instance
  (LivelinessConstraint defUseObj op ty res ctx, SymbolicVarId symVarId) =>
  ProgConstraints (Liveliness defUseObj) (Component.Prog op symVarId ty) ctx
  where
  constrainProg (Liveliness defUseObj) = componentProgOnlyUseNewestDef defUseObj
