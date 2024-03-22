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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Program.BuiltinProgConstraints.Liveliness
  ( Def (..),
    UnionDef,
    Use (..),
    UnionUse,
    ComponentUse (..),
    UnionComponentUse,
    Liveliness (..),
    LivelinessConcrete (..),
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
    livelinessConcreteProgInvalidatingDefs,
    livelinessComponentProgInvalidatingDefs,
  )
where

import Control.Arrow (Arrow (second))
import Control.Monad.Except (MonadError (throwError))
import Data.Foldable (foldl')
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvaluateSym,
    LogicalOp (symImplies, symNot, (.&&), (.||)),
    Mergeable (rootStrategy),
    MergingStrategy (SimpleStrategy),
    MonadUnion,
    SEq ((.==)),
    SimpleMergeable,
    Solvable (con),
    SymBool,
    ToSym (toSym),
    UnionM,
    liftUnionM,
    mrgFmap,
    mrgFoldM,
    mrgReturn,
    mrgSequence_,
    mrgTraverse,
    mrgTraverse_,
    simpleMerge,
    symUnless,
  )
import Grisette.Core.Data.Class.SOrd (SOrd ((.<=), (.>=)))
import Grisette.Generics.BoolLike (BoolLike)
import Grisette.Generics.Class.MonadBranching (MonadBranching (mrgIf))
import Grisette.Generics.Class.SimpleMergeable (SimpleMergeable (mrgIte))
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

class (Mergeable res, BoolLike bool) => Resource bool res where
  conflict :: res -> res -> bool

data Def bool varId res = Def
  { defId :: varId,
    defResource :: res,
    defDisabled :: bool
  }
  deriving (Show, Eq, Generic)
  deriving
    (Mergeable, Grisette.SEq, EvaluateSym)
    via (Default (Def bool varId res))

data Use bool varId = Use {useId :: varId, useDisabled :: bool}
  deriving (Show, Eq, Generic)
  deriving (Mergeable, Grisette.SEq, EvaluateSym) via (Default (Use bool varId))

newtype Liveliness livelinessObj = Liveliness livelinessObj

newtype LivelinessConcrete livelinessObj = LivelinessConcrete livelinessObj

class LivelinessName defUseObj where
  livelinessName :: defUseObj -> T.Text

livelinessTypeDefs ::
  ( Mergeable varId,
    LivelinessTypeResource defUseObj res ty,
    MonadContext ctx,
    BoolLike bool
  ) =>
  defUseObj ->
  [ty] ->
  [varId] ->
  bool ->
  ctx [Def bool varId res]
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
    LivelinessTypeResource defUseObj res ty,
    BoolLike bool
  ) =>
  defUseObj ->
  op ->
  [varId] ->
  bool ->
  ctx [Def bool varId res]
livelinessOpDefsByType defUseObj op resIds disabled = do
  ty <- typeOp op
  livelinessTypeDefs defUseObj (resTypes ty) resIds disabled

livelinessTypeUses ::
  ( Mergeable varId,
    LivelinessTypeResource defUseObj res ty,
    MonadContext ctx,
    BoolLike bool
  ) =>
  defUseObj ->
  [ty] ->
  [varId] ->
  bool ->
  ctx [Use bool varId]
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
    LivelinessTypeResource defUseObj res ty,
    BoolLike bool
  ) =>
  defUseObj ->
  op ->
  [varId] ->
  bool ->
  ctx [Use bool varId]
livelinessOpUsesByType defUseObj op argIds disabled = do
  ty <- typeOp op
  livelinessTypeUses defUseObj (argTypes ty) argIds disabled

newtype MergeResourceListWithDisabled bool res
  = MergeResourceListWithDisabled [(res, bool)]
  deriving (Generic)

applyDisabled ::
  (BoolLike bool) =>
  bool ->
  MergeResourceListWithDisabled bool res ->
  MergeResourceListWithDisabled bool res
applyDisabled disabled (MergeResourceListWithDisabled l) =
  MergeResourceListWithDisabled $ second (.|| disabled) <$> l

instance Monoid (MergeResourceListWithDisabled bool res) where
  mempty = MergeResourceListWithDisabled []

instance Semigroup (MergeResourceListWithDisabled bool res) where
  MergeResourceListWithDisabled l1 <> MergeResourceListWithDisabled l2 =
    MergeResourceListWithDisabled $ l1 ++ l2

deriving via
  (Default (MergeResourceListWithDisabled Bool res))
  instance
    (Mergeable res) =>
    Mergeable (MergeResourceListWithDisabled Bool res)

instance
  (Grisette.SimpleMergeable res) =>
  Mergeable (MergeResourceListWithDisabled SymBool res)
  where
  rootStrategy =
    SimpleStrategy $
      \cond
       (MergeResourceListWithDisabled l1)
       (MergeResourceListWithDisabled l2) ->
          MergeResourceListWithDisabled $
            zipWith (mrgIte cond) (pad l2 l1) (pad l1 l2)
    where
      pad ref l
        | length l >= length ref = l
        | otherwise =
            l ++ (second (const $ con True) <$> drop (length l) ref)

mergeResourceListFromDefList ::
  (Resource bool res) =>
  [Def bool varId res] ->
  MergeResourceListWithDisabled bool res
mergeResourceListFromDefList =
  MergeResourceListWithDisabled . fmap (\d -> (defResource d, defDisabled d))

livelinessConcreteProgInvalidatingDefs ::
  forall defUseObj bool op ty res ctx conVarId varId.
  ( LivelinessConstraint defUseObj bool op ty res ctx,
    ConcreteVarId conVarId,
    Mergeable varId,
    Resource bool res,
    Mergeable (MergeResourceListWithDisabled bool res)
  ) =>
  defUseObj ->
  Concrete.Prog op conVarId ty ->
  varId ->
  bool ->
  ctx [Def bool varId res]
livelinessConcreteProgInvalidatingDefs
  defUseObj
  (Concrete.Prog _ _ stmtList _)
  varId
  disabled = do
    MergeResourceListWithDisabled lst <-
      mrgFoldM
        ( \acc stmt -> do
            v <-
              mrgFmap mergeResourceListFromDefList $
                concreteStmtInvalidatingDef defUseObj stmt
            mrgReturn $ acc <> applyDisabled disabled v
        )
        (MergeResourceListWithDisabled [])
        stmtList
    mrgReturn $ uncurry (Def varId) <$> lst

livelinessComponentProgInvalidatingDefs ::
  forall defUseObj op ty res ctx symVarId varId.
  ( LivelinessConstraint defUseObj SymBool op ty res ctx,
    SymbolicVarId symVarId,
    Mergeable varId,
    MonadUnion ctx,
    Resource SymBool res,
    Grisette.SimpleMergeable res
  ) =>
  defUseObj ->
  Component.Prog op symVarId ty ->
  varId ->
  SymBool ->
  ctx [Def SymBool varId res]
livelinessComponentProgInvalidatingDefs
  defUseObj
  (Component.Prog _ _ stmtList _)
  varId
  disabled = do
    MergeResourceListWithDisabled lst <-
      mrgFoldM
        ( \acc stmt -> do
            v <-
              mrgFmap mergeResourceListFromDefList $
                componentStmtInvalidatingDefs defUseObj stmt >>= liftUnionM
            mrgReturn $ acc <> applyDisabled disabled v
        )
        (MergeResourceListWithDisabled [])
        stmtList
    mrgReturn $ uncurry (Def varId) <$> lst

class
  (LivelinessName defUseObj, MonadContext ctx, Resource bool res) =>
  LivelinessOpResource defUseObj bool op res ctx
    | defUseObj -> res
  where
  livelinessOpDefs ::
    (Mergeable varId) =>
    defUseObj ->
    op ->
    [varId] ->
    bool ->
    ctx [Def bool varId res]
  default livelinessOpDefs ::
    ( Mergeable varId,
      OpTyping op ty ctx,
      LivelinessTypeResource defUseObj res ty
    ) =>
    defUseObj ->
    op ->
    [varId] ->
    bool ->
    ctx [Def bool varId res]
  livelinessOpDefs = livelinessOpDefsByType
  livelinessOpInvalidatingDefs ::
    (Mergeable varId) =>
    defUseObj ->
    op ->
    [varId] ->
    bool ->
    ctx [Def bool varId res]
  livelinessOpInvalidatingDefs = livelinessOpDefs
  livelinessOpUses ::
    (Mergeable varId) =>
    defUseObj ->
    op ->
    [varId] ->
    bool ->
    ctx [Use bool varId]
  default livelinessOpUses ::
    ( Mergeable varId,
      OpTyping op ty ctx,
      LivelinessTypeResource defUseObj res ty
    ) =>
    defUseObj ->
    op ->
    [varId] ->
    bool ->
    ctx [Use bool varId]
  livelinessOpUses = livelinessOpUsesByType

class
  (Mergeable res) =>
  LivelinessTypeResource defUseObj res ty
    | defUseObj -> res
  where
  livelinessTypeResource :: defUseObj -> ty -> Maybe res

type LivelinessConstraint defUseObj bool op ty res ctx =
  ( LivelinessOpResource defUseObj bool op res ctx,
    LivelinessTypeResource defUseObj res ty,
    Mergeable op
  )

type UnionDef varId res = UnionM [Def SymBool varId res]

type UnionUse varId = UnionM [Use SymBool varId]

concreteStmtDef ::
  (ConcreteVarId conVarId, LivelinessOpResource defUseObj bool op res ctx) =>
  defUseObj ->
  Concrete.Stmt op conVarId ->
  ctx [Def bool conVarId res]
concreteStmtDef defUseObj (Concrete.Stmt op _ resIds) =
  livelinessOpDefs defUseObj op resIds (toSym False)

concreteStmtUnionDef ::
  (ConcreteVarId conVarId, LivelinessOpResource defUseObj SymBool op res ctx) =>
  defUseObj ->
  Concrete.Stmt op conVarId ->
  ctx (UnionDef conVarId res)
concreteStmtUnionDef defUseObj stmt =
  mrgFmap mrgReturn $ concreteStmtDef defUseObj stmt

concreteStmtInvalidatingDef ::
  (ConcreteVarId conVarId, LivelinessOpResource defUseObj bool op res ctx) =>
  defUseObj ->
  Concrete.Stmt op conVarId ->
  ctx [Def bool conVarId res]
concreteStmtInvalidatingDef defUseObj (Concrete.Stmt op _ resIds) =
  livelinessOpInvalidatingDefs defUseObj op resIds (toSym False)

concreteStmtInvalidatingUnionDef ::
  (ConcreteVarId conVarId, LivelinessOpResource defUseObj SymBool op res ctx) =>
  defUseObj ->
  Concrete.Stmt op conVarId ->
  ctx (UnionDef conVarId res)
concreteStmtInvalidatingUnionDef defUseObj stmt =
  mrgFmap mrgReturn $ concreteStmtInvalidatingDef defUseObj stmt

concreteStmtUse ::
  (ConcreteVarId conVarId, LivelinessOpResource defUseObj bool op res ctx) =>
  defUseObj ->
  Concrete.Stmt op conVarId ->
  ctx [Use bool conVarId]
concreteStmtUse defUseObj (Concrete.Stmt op argIds _) =
  livelinessOpUses defUseObj op argIds (toSym False)

concreteStmtUnionUse ::
  (ConcreteVarId conVarId, LivelinessOpResource defUseObj SymBool op res ctx) =>
  defUseObj ->
  Concrete.Stmt op conVarId ->
  ctx (UnionUse conVarId)
concreteStmtUnionUse defUseObj stmt =
  mrgFmap mrgReturn $ concreteStmtUse defUseObj stmt

concreteArgDefs ::
  ( ConcreteVarId conVarId,
    LivelinessTypeResource defUseObj res ty,
    BoolLike bool,
    Mergeable res
  ) =>
  defUseObj ->
  [Concrete.ProgArg conVarId ty] ->
  [Def bool conVarId res]
concreteArgDefs defUseObj =
  mapMaybe
    ( \(Concrete.ProgArg _ argId argType) ->
        (\res -> Def argId res (toSym False))
          <$> livelinessTypeResource defUseObj argType
    )

concreteArgUnionDefs ::
  ( ConcreteVarId conVarId,
    LivelinessTypeResource defUseObj res ty,
    Mergeable res
  ) =>
  defUseObj ->
  [Concrete.ProgArg conVarId ty] ->
  UnionDef conVarId res
concreteArgUnionDefs defUseObj = mrgReturn . concreteArgDefs defUseObj

concreteResUses ::
  ( ConcreteVarId conVarId,
    LivelinessTypeResource defUseObj res ty,
    BoolLike bool
  ) =>
  defUseObj ->
  [Concrete.ProgRes conVarId ty] ->
  [Use bool conVarId]
concreteResUses defUseObj =
  mapMaybe
    ( \(Concrete.ProgRes resId resType) ->
        livelinessTypeResource defUseObj resType
          *> Just (Use resId (toSym False))
    )

concreteResUnionUses ::
  (ConcreteVarId conVarId, LivelinessTypeResource defUseObj res ty) =>
  defUseObj ->
  [Concrete.ProgRes conVarId ty] ->
  UnionUse conVarId
concreteResUnionUses defUseObj =
  mrgReturn . concreteResUses defUseObj

invalidateList ::
  (Resource bool res) =>
  [Def bool conVarId res] ->
  [Def bool conVarId res] ->
  bool
invalidateList invalidatingDefs defs =
  foldl'
    (.||)
    (toSym False)
    [ conflict (defResource invalidatingDef) (defResource def)
      | invalidatingDef <- invalidatingDefs,
        def <- defs
    ]

cannotUseInvalidatedSingle ::
  ( LivelinessName defUseObj,
    Resource bool res,
    MonadBranching bool ctx,
    ConcreteVarId conVarId,
    MonadContext ctx
  ) =>
  defUseObj ->
  Use bool conVarId ->
  Def bool conVarId res ->
  bool ->
  ctx ()
cannotUseInvalidatedSingle defUseObj use def invalidated = do
  mrgIf
    ( useDisabled use
        .|| defDisabled def
        .|| ( toSym (useId use == defId def)
                `symImplies` symNot invalidated
            )
    )
    (return ())
    $ mrgThrowError
    $ "Cannot use invalidated resource for " <> livelinessName defUseObj

cannotUseInvalidatedList ::
  ( LivelinessName defUseObj,
    Resource bool res,
    MonadBranching bool ctx,
    ConcreteVarId conVarId,
    MonadContext ctx
  ) =>
  defUseObj ->
  [Use bool conVarId] ->
  [Def bool conVarId res] ->
  bool ->
  ctx ()
cannotUseInvalidatedList defUseObj uses defs invalidated =
  mrgSequence_
    [ cannotUseInvalidatedSingle defUseObj use def invalidated
      | use <- uses,
        def <- defs
    ]

instance
  ( LivelinessConstraint defUseObj Bool op ty res ctx,
    ConcreteVarId conVarId
  ) =>
  ProgConstraints
    (LivelinessConcrete defUseObj)
    (Concrete.Prog op conVarId ty)
    ctx
  where
  constrainProg
    (LivelinessConcrete defUseObj)
    (Concrete.Prog _ argList stmtList resList) = do
      let argDefs = concreteArgDefs defUseObj argList
      let resUses = concreteResUses defUseObj resList
      availableDefs <- goStmts [(argDefs, toSym False)] stmtList
      cannotUseInvalidated resUses availableDefs
      where
        invalidate ::
          (Resource bool res) =>
          [Def bool conVarId res] ->
          [([Def bool conVarId res], bool)] ->
          [([Def bool conVarId res], bool)]
        invalidate invalidatingDef =
          fmap
            ( \(def, invalidated) ->
                (def, invalidated .|| invalidateList invalidatingDef def)
            )
        cannotUseInvalidated ::
          [Use Bool conVarId] -> [([Def Bool conVarId res], Bool)] -> ctx ()
        cannotUseInvalidated uses =
          mrgTraverse_ (uncurry (cannotUseInvalidatedList defUseObj uses))
        goStmts ::
          [([Def Bool conVarId res], Bool)] ->
          [Concrete.Stmt op conVarId] ->
          ctx [([Def Bool conVarId res], Bool)]
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
  ( LivelinessConstraint defUseObj SymBool op ty res ctx,
    ConcreteVarId conVarId,
    MonadUnion ctx
  ) =>
  ProgConstraints (Liveliness defUseObj) (Concrete.Prog op conVarId ty) ctx
  where
  constrainProg
    (Liveliness defUseObj)
    (Concrete.Prog _ argList stmtList resList) = do
      let argDefs = concreteArgUnionDefs defUseObj argList
      let resUses = concreteResUnionUses defUseObj resList
      availableDefs <- goStmts [(argDefs, toSym False)] stmtList
      cannotUseInvalidated resUses availableDefs
      where
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
        cannotUseInvalidatedUnion ::
          UnionUse conVarId -> UnionDef conVarId res -> SymBool -> ctx ()
        cannotUseInvalidatedUnion useUnion defUnion invalidated = do
          uses <- liftUnionM useUnion
          defs <- liftUnionM defUnion
          cannotUseInvalidatedList defUseObj uses defs invalidated
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
          uses <- concreteStmtUnionUse defUseObj stmt
          cannotUseInvalidated uses allDefs
          invalidatingDef <- concreteStmtInvalidatingUnionDef defUseObj stmt
          let invalidatedDefs = invalidate invalidatingDef allDefs
          def <- concreteStmtUnionDef defUseObj stmt
          let newDefs = (def, toSym False) : invalidatedDefs
          goStmts newDefs rest

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
    LivelinessOpResource defUseObj SymBool op res ctx
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
    LivelinessOpResource defUseObj SymBool op res ctx
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
    LivelinessOpResource defUseObj SymBool op res ctx
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
  ( SymbolicVarId symVarId,
    LivelinessConstraint defUseObj SymBool op ty res ctx,
    MonadUnion ctx
  ) =>
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
  ( SymbolicVarId symVarId,
    LivelinessConstraint defUseObj SymBool op ty res ctx,
    MonadUnion ctx
  ) =>
  defUseObj ->
  Component.Prog op symVarId ty ->
  ctx [UnionDef symVarId res]
componentProgInvalidatingDefs
  defUseObj
  (Component.Prog _ _ stmtList _) =
    mrgTraverse (componentStmtInvalidatingDefs defUseObj) stmtList

componentProgUses ::
  ( SymbolicVarId symVarId,
    LivelinessConstraint defUseObj SymBool op ty res ctx,
    MonadUnion ctx
  ) =>
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
  ( SymbolicVarId symVarId,
    LivelinessConstraint defUseObj SymBool op ty res ctx,
    MonadUnion ctx
  ) =>
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
  ( LivelinessConstraint defUseObj SymBool op ty res ctx,
    SymbolicVarId symVarId,
    MonadUnion ctx
  ) =>
  ProgConstraints (Liveliness defUseObj) (Component.Prog op symVarId ty) ctx
  where
  constrainProg (Liveliness defUseObj) = componentProgOnlyUseNewestDef defUseObj
