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
{-# LANGUAGE TupleSections #-}
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
    EfficientlyMergeableResource,
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
import Control.Monad ((>=>))
import Control.Monad.Except (MonadError (throwError))
import Data.Foldable (foldl')
import Data.Maybe (isJust, mapMaybe)
import Data.Proxy (Proxy (Proxy))
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

type EfficientlyMergeableResource bool res =
  Mergeable (MergeResourceListWithDisabled bool res)

class
  ( Mergeable res,
    BoolLike bool,
    EfficientlyMergeableResource bool res
  ) =>
  Resource bool res
  where
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

newtype Liveliness bool livelinessObj = Liveliness livelinessObj

class LivelinessName livelinessObj where
  livelinessName :: livelinessObj -> T.Text

livelinessTypeDefs ::
  ( Mergeable varId,
    LivelinessTypeResource livelinessObj res ty,
    MonadContext ctx,
    BoolLike bool
  ) =>
  livelinessObj ->
  [ty] ->
  [varId] ->
  bool ->
  ctx [Def bool varId res]
livelinessTypeDefs livelinessObj tys resIds disabled = do
  mrgReturn
    $ mapMaybe
      ( \(id, ty) ->
          (\res -> Def id res disabled)
            <$> livelinessTypeDefResource livelinessObj ty
      )
    $ zip resIds tys

livelinessOpDefsByType ::
  ( Mergeable varId,
    OpTyping op ty ctx,
    LivelinessTypeResource livelinessObj res ty,
    BoolLike bool
  ) =>
  livelinessObj ->
  op ->
  [varId] ->
  bool ->
  ctx [Def bool varId res]
livelinessOpDefsByType livelinessObj op resIds disabled = do
  ty <- typeOp op
  livelinessTypeDefs livelinessObj (resTypes ty) resIds disabled

livelinessTypeUses ::
  ( Mergeable varId,
    LivelinessTypeResource livelinessObj res ty,
    MonadContext ctx,
    BoolLike bool
  ) =>
  livelinessObj ->
  [ty] ->
  [varId] ->
  bool ->
  ctx [Use bool varId]
livelinessTypeUses livelinessObj tys argIds disabled = do
  mrgReturn
    $ mapMaybe
      ( \(id, ty) ->
          if livelinessTypeValidUse livelinessObj ty
            then Just $ Use id disabled
            else Nothing
      )
    $ zip argIds tys

livelinessOpUsesByType ::
  ( Mergeable varId,
    OpTyping op ty ctx,
    LivelinessTypeResource livelinessObj res ty,
    BoolLike bool
  ) =>
  livelinessObj ->
  op ->
  [varId] ->
  bool ->
  ctx [Use bool varId]
livelinessOpUsesByType livelinessObj op argIds disabled = do
  ty <- typeOp op
  livelinessTypeUses livelinessObj (argTypes ty) argIds disabled

mergeResourceListFromDefList ::
  (Resource bool res) =>
  [Def bool varId res] ->
  MergeResourceListWithDisabled bool res
mergeResourceListFromDefList =
  MergeResourceListWithDisabled . fmap (\d -> (defResource d, defDisabled d))

livelinessConcreteProgInvalidatingDefs ::
  forall livelinessObj bool op ty res ctx conVarId varId.
  ( LivelinessConstraint livelinessObj bool op ty res ctx,
    ConcreteVarId conVarId,
    Mergeable varId,
    Resource bool res,
    Mergeable (MergeResourceListWithDisabled bool res)
  ) =>
  livelinessObj ->
  Concrete.Prog op conVarId ty ->
  varId ->
  bool ->
  ctx [Def bool varId res]
livelinessConcreteProgInvalidatingDefs
  livelinessObj
  (Concrete.Prog _ _ stmtList _)
  varId
  disabled = do
    MergeResourceListWithDisabled lst <-
      mrgFoldM
        ( \acc stmt -> do
            v <-
              mrgFmap mergeResourceListFromDefList $
                concreteStmtInvalidatingDef livelinessObj stmt
            mrgReturn $ acc <> applyDisabled disabled v
        )
        (MergeResourceListWithDisabled [])
        stmtList
    mrgReturn $ uncurry (Def varId) <$> lst

--
livelinessComponentProgInvalidatingDefs ::
  forall livelinessObj op ty res ctx symVarId varId.
  ( LivelinessConstraint livelinessObj SymBool op ty res ctx,
    SymbolicVarId symVarId,
    Mergeable varId,
    MonadUnion ctx,
    Resource SymBool res,
    Grisette.SimpleMergeable res
  ) =>
  livelinessObj ->
  Component.Prog op symVarId ty ->
  varId ->
  SymBool ->
  ctx [Def SymBool varId res]
livelinessComponentProgInvalidatingDefs
  livelinessObj
  (Component.Prog _ _ stmtList _)
  varId
  disabled = do
    MergeResourceListWithDisabled lst <-
      mrgFoldM
        ( \acc stmt -> do
            v <-
              mrgFmap mergeResourceListFromDefList $
                componentStmtInvalidatingDefs livelinessObj stmt >>= liftUnionM
            mrgReturn $ acc <> applyDisabled disabled v
        )
        (MergeResourceListWithDisabled [])
        stmtList
    mrgReturn $ uncurry (Def varId) <$> lst

class
  (LivelinessName livelinessObj, MonadContext ctx, Resource bool res) =>
  LivelinessOpResource livelinessObj bool op res ctx
    | livelinessObj -> res
  where
  livelinessOpDefs ::
    (Mergeable varId) =>
    livelinessObj ->
    op ->
    [varId] ->
    bool ->
    ctx [Def bool varId res]
  default livelinessOpDefs ::
    ( Mergeable varId,
      OpTyping op ty ctx,
      LivelinessTypeResource livelinessObj res ty
    ) =>
    livelinessObj ->
    op ->
    [varId] ->
    bool ->
    ctx [Def bool varId res]
  livelinessOpDefs = livelinessOpDefsByType
  livelinessOpInvalidatingDefs ::
    (Mergeable varId) =>
    livelinessObj ->
    op ->
    [varId] ->
    bool ->
    ctx [Def bool varId res]
  livelinessOpInvalidatingDefs = livelinessOpDefs
  livelinessOpUses ::
    (Mergeable varId) =>
    livelinessObj ->
    op ->
    [varId] ->
    bool ->
    ctx [Use bool varId]
  default livelinessOpUses ::
    ( Mergeable varId,
      OpTyping op ty ctx,
      LivelinessTypeResource livelinessObj res ty
    ) =>
    livelinessObj ->
    op ->
    [varId] ->
    bool ->
    ctx [Use bool varId]
  livelinessOpUses = livelinessOpUsesByType
  livelinessSubProgConstraint :: livelinessObj -> proxy bool -> op -> ctx ()
  livelinessSubProgConstraint _ _ _ = mrgReturn ()

class
  (Mergeable res) =>
  LivelinessTypeResource livelinessObj res ty
    | livelinessObj -> res
  where
  livelinessTypeDefResource :: livelinessObj -> ty -> Maybe res
  livelinessTypeValidUse :: livelinessObj -> ty -> Bool
  livelinessTypeValidUse livelinessObj ty =
    isJust $ livelinessTypeDefResource livelinessObj ty

type LivelinessConstraint livelinessObj bool op ty res ctx =
  ( LivelinessOpResource livelinessObj bool op res ctx,
    LivelinessTypeResource livelinessObj res ty,
    Mergeable op
  )

type UnionDef varId res = UnionM [Def SymBool varId res]

type UnionUse varId = UnionM [Use SymBool varId]

concreteStmtDef ::
  ( ConcreteVarId conVarId,
    LivelinessOpResource livelinessObj bool op res ctx
  ) =>
  livelinessObj ->
  Concrete.Stmt op conVarId ->
  ctx [Def bool conVarId res]
concreteStmtDef livelinessObj (Concrete.Stmt op _ resIds) =
  livelinessOpDefs livelinessObj op resIds (toSym False)

concreteStmtUnionDef ::
  ( ConcreteVarId conVarId,
    LivelinessOpResource livelinessObj SymBool op res ctx
  ) =>
  livelinessObj ->
  Concrete.Stmt op conVarId ->
  ctx (UnionDef conVarId res)
concreteStmtUnionDef livelinessObj stmt =
  mrgFmap mrgReturn $ concreteStmtDef livelinessObj stmt

concreteStmtInvalidatingDef ::
  ( ConcreteVarId conVarId,
    LivelinessOpResource livelinessObj bool op res ctx
  ) =>
  livelinessObj ->
  Concrete.Stmt op conVarId ->
  ctx [Def bool conVarId res]
concreteStmtInvalidatingDef livelinessObj (Concrete.Stmt op _ resIds) =
  livelinessOpInvalidatingDefs livelinessObj op resIds (toSym False)

concreteStmtInvalidatingUnionDef ::
  ( ConcreteVarId conVarId,
    LivelinessOpResource livelinessObj SymBool op res ctx
  ) =>
  livelinessObj ->
  Concrete.Stmt op conVarId ->
  ctx (UnionDef conVarId res)
concreteStmtInvalidatingUnionDef livelinessObj stmt =
  mrgFmap mrgReturn $ concreteStmtInvalidatingDef livelinessObj stmt

concreteStmtUse ::
  ( ConcreteVarId conVarId,
    LivelinessOpResource livelinessObj bool op res ctx
  ) =>
  livelinessObj ->
  Concrete.Stmt op conVarId ->
  ctx [Use bool conVarId]
concreteStmtUse livelinessObj (Concrete.Stmt op argIds _) =
  livelinessOpUses livelinessObj op argIds (toSym False)

concreteStmtUnionUse ::
  ( ConcreteVarId conVarId,
    LivelinessOpResource livelinessObj SymBool op res ctx
  ) =>
  livelinessObj ->
  Concrete.Stmt op conVarId ->
  ctx (UnionUse conVarId)
concreteStmtUnionUse livelinessObj stmt =
  mrgFmap mrgReturn $ concreteStmtUse livelinessObj stmt

concreteArgDefs ::
  ( ConcreteVarId conVarId,
    LivelinessTypeResource livelinessObj res ty,
    BoolLike bool,
    Mergeable res
  ) =>
  livelinessObj ->
  [Concrete.ProgArg conVarId ty] ->
  [Def bool conVarId res]
concreteArgDefs livelinessObj =
  mapMaybe
    ( \(Concrete.ProgArg _ argId argType) ->
        (\res -> Def argId res (toSym False))
          <$> livelinessTypeDefResource livelinessObj argType
    )

concreteArgUnionDefs ::
  ( ConcreteVarId conVarId,
    LivelinessTypeResource livelinessObj res ty,
    Mergeable res
  ) =>
  livelinessObj ->
  [Concrete.ProgArg conVarId ty] ->
  UnionDef conVarId res
concreteArgUnionDefs livelinessObj = mrgReturn . concreteArgDefs livelinessObj

concreteResUses ::
  ( ConcreteVarId conVarId,
    LivelinessTypeResource livelinessObj res ty,
    BoolLike bool
  ) =>
  livelinessObj ->
  [Concrete.ProgRes conVarId ty] ->
  [Use bool conVarId]
concreteResUses livelinessObj =
  mapMaybe
    ( \(Concrete.ProgRes resId resType) ->
        if livelinessTypeValidUse livelinessObj resType
          then Just (Use resId (toSym False))
          else Nothing
    )

concreteResUnionUses ::
  (ConcreteVarId conVarId, LivelinessTypeResource livelinessObj res ty) =>
  livelinessObj ->
  [Concrete.ProgRes conVarId ty] ->
  UnionUse conVarId
concreteResUnionUses livelinessObj =
  mrgReturn . concreteResUses livelinessObj

invalidateSingle ::
  (Resource bool res) =>
  [Def bool conVarId res] ->
  Def bool conVarId res ->
  bool
invalidateSingle invalidatingDefs def =
  foldl'
    (.||)
    (toSym False)
    [ conflict (defResource invalidatingDef) (defResource def)
      | invalidatingDef <- invalidatingDefs
    ]

cannotUseInvalidatedSingle ::
  ( LivelinessName livelinessObj,
    Resource bool res,
    MonadBranching bool ctx,
    ConcreteVarId conVarId,
    MonadContext ctx
  ) =>
  livelinessObj ->
  Use bool conVarId ->
  Def bool conVarId res ->
  bool ->
  ctx ()
cannotUseInvalidatedSingle livelinessObj use def invalidated = do
  mrgIf
    ( useDisabled use
        .|| defDisabled def
        .|| ( toSym (useId use == defId def)
                `symImplies` symNot invalidated
            )
    )
    (return ())
    $ mrgThrowError
    $ "Cannot use invalidated resource for " <> livelinessName livelinessObj

cannotUseInvalidatedList ::
  ( LivelinessName livelinessObj,
    Resource bool res,
    MonadBranching bool ctx,
    ConcreteVarId conVarId,
    MonadContext ctx
  ) =>
  livelinessObj ->
  [Use bool conVarId] ->
  [(Def bool conVarId res, bool)] ->
  ctx ()
cannotUseInvalidatedList livelinessObj uses defsWithInvalidated =
  mrgSequence_
    [ cannotUseInvalidatedSingle livelinessObj use def invalidated
      | use <- uses,
        (def, invalidated) <- defsWithInvalidated
    ]

invalidateList ::
  (Resource bool res) =>
  [Def bool conVarId res] ->
  [(Def bool conVarId res, bool)] ->
  [(Def bool conVarId res, bool)]
invalidateList invalidatingDef =
  fmap
    ( \(def, invalidated) ->
        ( def,
          invalidated
            .|| invalidateSingle invalidatingDef def
        )
    )

instance
  ( LivelinessConstraint livelinessObj Bool op ty res ctx,
    ConcreteVarId conVarId
  ) =>
  ProgConstraints
    (Liveliness Bool livelinessObj)
    (Concrete.Prog op conVarId ty)
    ctx
  where
  constrainProg
    (Liveliness livelinessObj)
    (Concrete.Prog _ argList stmtList resList) = do
      let argDefs = concreteArgDefs livelinessObj argList
      let resUses = concreteResUses livelinessObj resList
      availableDefs <- goStmts ((,toSym False) <$> argDefs) stmtList
      cannotUseInvalidatedList livelinessObj resUses availableDefs
      mrgTraverse_
        (livelinessSubProgConstraint livelinessObj (Proxy :: Proxy Bool))
        $ Concrete.stmtOp <$> stmtList
      where
        goStmts ::
          [(Def Bool conVarId res, Bool)] ->
          [Concrete.Stmt op conVarId] ->
          ctx [(Def Bool conVarId res, Bool)]
        goStmts allDefs [] = mrgReturn allDefs
        goStmts allDefs (stmt : rest) = do
          uses <- concreteStmtUse livelinessObj stmt
          cannotUseInvalidatedList livelinessObj uses allDefs
          invalidatingDef <- concreteStmtInvalidatingDef livelinessObj stmt
          let invalidatedDefs = invalidateList invalidatingDef allDefs
          def <- concreteStmtDef livelinessObj stmt
          let newDefs = ((,toSym False) <$> def) ++ invalidatedDefs
          goStmts newDefs rest

instance
  ( LivelinessConstraint livelinessObj SymBool op ty res ctx,
    ConcreteVarId conVarId,
    MonadUnion ctx
  ) =>
  ProgConstraints
    (Liveliness SymBool livelinessObj)
    (Concrete.Prog op conVarId ty)
    ctx
  where
  constrainProg
    (Liveliness livelinessObj)
    (Concrete.Prog _ argList stmtList resList) = do
      let argDefs = concreteArgUnionDefs livelinessObj argList
      let resUses = concreteResUnionUses livelinessObj resList
      availableDefs <- goStmts [mrgFmap (fmap (,toSym False)) argDefs] stmtList
      cannotUseInvalidated resUses availableDefs
      mrgTraverse_
        (livelinessSubProgConstraint livelinessObj (Proxy :: Proxy SymBool))
        $ Concrete.stmtOp <$> stmtList
      where
        invalidate ::
          UnionDef conVarId res ->
          [UnionM [(Def SymBool conVarId res, SymBool)]] ->
          [UnionM [(Def SymBool conVarId res, SymBool)]]
        invalidate invalidatingDef =
          fmap (\defUnion -> invalidateList <$> invalidatingDef <*> defUnion)
        cannotUseInvalidatedUnion ::
          UnionUse conVarId ->
          UnionM [(Def SymBool conVarId res, SymBool)] ->
          ctx ()
        cannotUseInvalidatedUnion useUnion defUnion = do
          uses <- liftUnionM useUnion
          defs <- liftUnionM defUnion
          cannotUseInvalidatedList livelinessObj uses defs
        cannotUseInvalidated ::
          UnionUse conVarId ->
          [UnionM [(Def SymBool conVarId res, SymBool)]] ->
          ctx ()
        cannotUseInvalidated uses =
          mrgTraverse_ (cannotUseInvalidatedUnion uses)
        goStmts ::
          [UnionM [(Def SymBool conVarId res, SymBool)]] ->
          [Concrete.Stmt op conVarId] ->
          ctx [UnionM [(Def SymBool conVarId res, SymBool)]]
        goStmts allDefs [] = mrgReturn allDefs
        goStmts allDefs (stmt : rest) = do
          uses <- concreteStmtUnionUse livelinessObj stmt
          cannotUseInvalidated uses allDefs
          invalidatingDef <- concreteStmtInvalidatingUnionDef livelinessObj stmt
          let invalidatedDefs = invalidate invalidatingDef allDefs
          def <- concreteStmtUnionDef livelinessObj stmt
          let newDefs = mrgFmap (fmap (,toSym False)) def : invalidatedDefs
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
    LivelinessOpResource livelinessObj SymBool op res ctx
  ) =>
  livelinessObj ->
  Component.Stmt op symVarId ->
  ctx (UnionDef symVarId res)
componentStmtDefs
  livelinessObj
  (Component.Stmt opUnion _ _ resIds _ disabled) = do
    op <- liftUnionM opUnion
    mrgFmap mrgReturn $ livelinessOpDefs livelinessObj op resIds disabled

componentStmtInvalidatingDefs ::
  ( SymbolicVarId symVarId,
    MonadUnion ctx,
    Mergeable op,
    Mergeable res,
    LivelinessOpResource livelinessObj SymBool op res ctx
  ) =>
  livelinessObj ->
  Component.Stmt op symVarId ->
  ctx (UnionDef symVarId res)
componentStmtInvalidatingDefs
  livelinessObj
  (Component.Stmt opUnion _ _ resIds _ disabled) = do
    op <- liftUnionM opUnion
    mrgFmap mrgReturn $
      livelinessOpInvalidatingDefs livelinessObj op resIds disabled

componentStmtUses ::
  ( SymbolicVarId symVarId,
    MonadUnion ctx,
    Mergeable op,
    LivelinessOpResource livelinessObj SymBool op res ctx
  ) =>
  livelinessObj ->
  Component.Stmt op symVarId ->
  ctx (UnionComponentUse symVarId)
componentStmtUses
  livelinessObj
  (Component.Stmt opUnion argIds _ resIds _ disabled) = do
    op <- liftUnionM opUnion
    opUses <- livelinessOpUses livelinessObj op argIds disabled
    mrgReturn . mrgReturn $
      (\(Use i useDisabled) -> ComponentUse i (head resIds) useDisabled)
        <$> opUses

componentProgDefs ::
  ( SymbolicVarId symVarId,
    LivelinessConstraint livelinessObj SymBool op ty res ctx,
    MonadUnion ctx
  ) =>
  livelinessObj ->
  Component.Prog op symVarId ty ->
  ctx [UnionDef symVarId res]
componentProgDefs
  livelinessObj
  (Component.Prog _ argList stmtList _) = do
    stmtDefs <- mrgTraverse (componentStmtDefs livelinessObj) stmtList
    mrgReturn $ argDefs : stmtDefs
    where
      argDefs =
        mrgReturn
          $ mapMaybe
            ( \(i, Component.ProgArg _ ty) -> do
                res <- livelinessTypeDefResource livelinessObj ty
                return $ Def (fromIntegral i) res (con False)
            )
          $ zip [0 ..] argList

componentProgInvalidatingDefs ::
  ( SymbolicVarId symVarId,
    LivelinessConstraint livelinessObj SymBool op ty res ctx,
    MonadUnion ctx
  ) =>
  livelinessObj ->
  Component.Prog op symVarId ty ->
  ctx [UnionDef symVarId res]
componentProgInvalidatingDefs
  livelinessObj
  (Component.Prog _ _ stmtList _) =
    mrgTraverse (componentStmtInvalidatingDefs livelinessObj) stmtList

componentProgUses ::
  ( SymbolicVarId symVarId,
    LivelinessConstraint livelinessObj SymBool op ty res ctx,
    MonadUnion ctx
  ) =>
  livelinessObj ->
  Component.Prog op symVarId ty ->
  ctx [UnionComponentUse symVarId]
componentProgUses
  livelinessObj
  (Component.Prog _ argList stmtList resList) = do
    stmtUses <- mrgTraverse (componentStmtUses livelinessObj) stmtList
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
              ( livelinessTypeValidUse livelinessObj
                  . Component.progResType
              )
              resList

instance
  ( LivelinessConstraint livelinessObj SymBool op ty res ctx,
    SymbolicVarId symVarId,
    MonadUnion ctx
  ) =>
  ProgConstraints
    (Liveliness SymBool livelinessObj)
    (Component.Prog op symVarId ty)
    ctx
  where
  constrainProg (Liveliness livelinessObj) sketch = do
    mrgTraverse_
      ( \useUnion -> traverseUnion useUnion $ \use -> do
          invalidatingDefIds <- invalidatingDefs
          mrgTraverse_ (\d -> defUseConstraint use d invalidatingDefIds)
            =<< defs
      )
      =<< uses
    mrgTraverse_
      ( liftUnionM
          >=> livelinessSubProgConstraint livelinessObj (Proxy :: Proxy SymBool)
      )
      $ Component.stmtOp <$> Component.progStmtList sketch
    where
      traverseUnion ::
        (Mergeable a) => UnionM [a] -> (a -> ctx ()) -> ctx ()
      traverseUnion union f = liftUnionM union >>= mrgTraverse_ f
      invalidatingDefs = componentProgInvalidatingDefs livelinessObj sketch
      uses = componentProgUses livelinessObj sketch
      defs = componentProgDefs livelinessObj sketch
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
                          <> livelinessName livelinessObj
              )
              invalidatingDefIds
