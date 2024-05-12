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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Program.BuiltinProgConstraints.Liveliness
  ( Def (..),
    UnionDef,
    Use (..),
    UnionUse,
    Liveliness (..),
    LivelinessOpResource (..),
    LivelinessName (..),
    LivelinessTypeResource (..),
    Resource (..),
    LivelinessConstraint,
    StmtDefUse (..),
    ProgDefUse (..),
    livelinessSubProgInvalidatingDefs,
    livelinessSubProgArgUses,
    livelinessSubProgResDefs,
    livelinessSubProgDefUses,
    livelinessProgArgDefs,
    livelinessProgResUses,
    livelinessProgStmtDefUses,
    livelinessProgDefUses,
    livelinessOpDefUsesByType,
    livelinessTypeDefs,
    livelinessTypeUses,
    ComponentUse (..),
    ComponentUnionUse,
    ComponentStmtDefUse (..),
    ComponentProgDefUse (..),
    livelinessComponentProgStmtDefUses,
    livelinessComponentProgDefUses,
  )
where

import Control.Arrow (Arrow (second))
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Foldable (foldl')
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvaluateSym,
    LogicalOp (symImplies, symNot, (.&&), (.||)),
    Mergeable (rootStrategy),
    MergingStrategy (SimpleStrategy),
    MonadFresh,
    MonadUnion,
    SEq ((.==)),
    SOrd ((.<=), (.>=)),
    SimpleMergeable (mrgIte),
    Solvable (con),
    SymBool,
    ToSym (toSym),
    UnionM,
    liftUnionM,
    mrgFmap,
    mrgIf,
    mrgReturn,
    mrgSequence_,
    mrgTraverse,
    mrgTraverse_,
    simpleMerge,
    symAssertWith,
    symUnless,
    (.>>=),
  )
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Operator.OpTyping (OpTyping (typeOp))
import qualified Grisette.Lib.Synth.Program.ComponentSketch.Program as Component
import qualified Grisette.Lib.Synth.Program.Concrete.Program as Concrete
import Grisette.Lib.Synth.Program.NullProg (NullProg)
import Grisette.Lib.Synth.Program.ProgConstraints
  ( OpSubProgConstraints (constrainOpSubProg),
    ProgConstraints (constrainProg),
    progArgLocalIdent,
    progResLocalIdent,
    progStmtLocalIdent,
    setConstraintKind,
  )
import Grisette.Lib.Synth.Program.ProgNaming (ProgNaming)
import Grisette.Lib.Synth.Program.ProgTyping (ProgTyping (typeProg))
import Grisette.Lib.Synth.Program.ProgUtil
  ( ProgUtil
      ( ProgTypeType,
        getProgArgIds,
        getProgNumStmts,
        getProgResIds,
        getProgStmtAtIdx
      ),
    StmtUtil
      ( StmtOpType,
        getStmtArgIds,
        getStmtDisabled,
        getStmtOp,
        getStmtResIds
      ),
  )
import Grisette.Lib.Synth.Program.SubProg (HasSubProgs)
import Grisette.Lib.Synth.Program.SumProg (SumProg (SumProgL, SumProgR))
import Grisette.Lib.Synth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import Grisette.Lib.Synth.VarId (ConcreteVarId, SymbolicVarId)

newtype MergeResourceListWithDisabled res
  = MergeResourceListWithDisabled [(res, SymBool)]
  deriving (Generic)

instance Monoid (MergeResourceListWithDisabled res) where
  mempty = MergeResourceListWithDisabled []

instance Semigroup (MergeResourceListWithDisabled res) where
  MergeResourceListWithDisabled l1 <> MergeResourceListWithDisabled l2 =
    MergeResourceListWithDisabled $ l1 ++ l2

instance
  (SimpleMergeable res) =>
  SimpleMergeable (MergeResourceListWithDisabled res)
  where
  mrgIte
    cond
    (MergeResourceListWithDisabled l1)
    (MergeResourceListWithDisabled l2) =
      MergeResourceListWithDisabled $
        zipWith (mrgIte cond) (pad l2 l1) (pad l1 l2)
      where
        pad ref l
          | length l >= length ref = l
          | otherwise =
              l ++ (second (const $ con True) <$> drop (length l) ref)

instance
  (SimpleMergeable res) =>
  Mergeable (MergeResourceListWithDisabled res)
  where
  rootStrategy = SimpleStrategy mrgIte

class (SEq res, SimpleMergeable res) => Resource res where
  conflict :: res -> res -> SymBool

data Def varId res = Def
  { defId :: varId,
    defResource :: res,
    defDisabled :: SymBool
  }
  deriving (Show, Eq, Generic)
  deriving
    (Mergeable, SEq, EvaluateSym)
    via (Default (Def varId res))

data Use varId res = Use
  { useId :: varId,
    useResource :: res,
    useDisabled :: SymBool
  }
  deriving (Show, Eq, Generic)
  deriving (Mergeable, SEq, EvaluateSym) via (Default (Use varId res))

newtype Liveliness livelinessObj = Liveliness livelinessObj

type UnionDef varId res = UnionM [Def varId res]

type UnionUse varId res = UnionM [Use varId res]

class LivelinessName livelinessObj where
  livelinessName :: livelinessObj -> T.Text

livelinessTypeUses ::
  ( Mergeable varId,
    LivelinessTypeResource livelinessObj res ty ctx,
    MonadContext ctx
  ) =>
  livelinessObj ->
  [ty] ->
  [varId] ->
  SymBool ->
  ctx (UnionUse varId res)
livelinessTypeUses livelinessObj tys resIds disabled = do
  r <-
    traverse
      ( \(varId, ty) -> do
          res <- livelinessTypeUseResource livelinessObj ty
          mrgReturn $ (\r -> Use varId r disabled) <$> res
      )
      $ zip resIds tys
  mrgReturn $ mrgReturn $ catMaybes r

livelinessTypeDefs ::
  ( Mergeable varId,
    LivelinessTypeResource livelinessObj res ty ctx,
    MonadContext ctx
  ) =>
  livelinessObj ->
  [ty] ->
  [varId] ->
  SymBool ->
  ctx (UnionDef varId res)
livelinessTypeDefs livelinessObj tys resIds disabled = do
  r <-
    traverse
      ( \(varId, ty) -> do
          res <- livelinessTypeDefResource livelinessObj ty
          mrgReturn $ (\r -> Def varId r disabled) <$> res
      )
      $ zip resIds tys
  mrgReturn $ mrgReturn $ catMaybes r

data StmtDefUse varId res = StmtDefUse
  { stmtDef :: UnionDef varId res,
    stmtInvalidatingDef :: UnionDef varId res,
    stmtUse :: UnionUse varId res
  }
  deriving (Show, Eq, Generic)
  deriving (Mergeable, SimpleMergeable) via (Default (StmtDefUse varId res))

data ProgDefUse varId res = ProgDefUse
  { progArgDefs :: UnionDef varId res,
    progStmtDefUses :: [StmtDefUse varId res],
    progResUses :: UnionUse varId res
  }
  deriving (Show, Eq, Generic)
  deriving (Mergeable) via (Default (ProgDefUse varId res))

livelinessOpDefUsesByType ::
  ( Mergeable varId,
    OpTyping op ty ctx,
    HasSubProgs op subProg ctx,
    LivelinessTypeResource livelinessObj res ty ctx
  ) =>
  livelinessObj ->
  op ->
  [varId] ->
  [varId] ->
  SymBool ->
  ctx (StmtDefUse varId res)
livelinessOpDefUsesByType livelinessObj op argIds resIds disabled = do
  ty <- typeOp op
  defs <- livelinessTypeDefs livelinessObj (resTypes ty) resIds disabled
  uses <- livelinessTypeUses livelinessObj (argTypes ty) argIds disabled
  mrgReturn $ StmtDefUse defs defs uses

mergeResourceListFromDefList ::
  (Resource res) => UnionDef varId res -> MergeResourceListWithDisabled res
mergeResourceListFromDefList defList =
  simpleMerge $
    MergeResourceListWithDisabled . fmap (\d -> (defResource d, defDisabled d))
      <$> defList

class
  (LivelinessName livelinessObj, MonadContext ctx, Resource res) =>
  LivelinessOpResource livelinessObj op res ctx
    | livelinessObj -> res
  where
  livelinessOpDefUses ::
    (Mergeable varId) =>
    livelinessObj ->
    op ->
    [varId] ->
    [varId] ->
    SymBool ->
    ctx (StmtDefUse varId res)
  default livelinessOpDefUses ::
    ( Mergeable varId,
      OpTyping op ty ctx,
      HasSubProgs op NullProg ctx,
      LivelinessTypeResource livelinessObj res ty ctx
    ) =>
    livelinessObj ->
    op ->
    [varId] ->
    [varId] ->
    SymBool ->
    ctx (StmtDefUse varId res)
  livelinessOpDefUses = livelinessOpDefUsesByType

instance
  ( LivelinessOpResource livelinessObj op res ctx,
    MonadUnion ctx,
    Mergeable op
  ) =>
  LivelinessOpResource livelinessObj (UnionM op) res ctx
  where
  livelinessOpDefUses livelinessObj opUnion argIds resIds disabled =
    liftUnionM opUnion
      .>>= \op -> livelinessOpDefUses livelinessObj op argIds resIds disabled

livelinessProgArgDefs ::
  forall livelinessObj prog stmt varId res ctx.
  ( Mergeable varId,
    ProgUtil prog stmt varId,
    LivelinessTypeResource livelinessObj res (ProgTypeType prog) ctx,
    ProgTyping prog (ProgTypeType prog),
    MonadFresh ctx,
    ProgNaming prog
  ) =>
  livelinessObj ->
  prog ->
  SymBool ->
  ctx (UnionDef varId res)
livelinessProgArgDefs livelinessObj prog disabled =
  progArgLocalIdent prog $ setConstraintKind "Liveliness" $ do
    TypeSignature argTypes _ <- typeProg prog
    livelinessTypeDefs
      livelinessObj
      (argTypes :: [ProgTypeType prog])
      (getProgArgIds prog)
      disabled

livelinessSubProgArgUses ::
  forall livelinessObj prog useVarId res ctx.
  ( Mergeable useVarId,
    LivelinessTypeResource livelinessObj res (ProgTypeType prog) ctx,
    ProgTyping prog (ProgTypeType prog),
    MonadFresh ctx,
    ProgNaming prog
  ) =>
  livelinessObj ->
  prog ->
  [useVarId] ->
  SymBool ->
  ctx (UnionUse useVarId res)
livelinessSubProgArgUses livelinessObj prog useVarIds disabled =
  progArgLocalIdent prog $ setConstraintKind "Liveliness" $ do
    TypeSignature argTypes _ <- typeProg prog
    livelinessTypeUses
      livelinessObj
      (argTypes :: [ProgTypeType prog])
      useVarIds
      disabled

livelinessProgResUses ::
  forall livelinessObj prog stmt varId res ctx.
  ( ProgUtil prog stmt varId,
    LivelinessTypeResource livelinessObj res (ProgTypeType prog) ctx,
    ProgTyping prog (ProgTypeType prog),
    MonadFresh ctx,
    ProgNaming prog,
    Mergeable varId
  ) =>
  livelinessObj ->
  prog ->
  SymBool ->
  ctx (UnionUse varId res)
livelinessProgResUses livelinessObj prog disabled =
  progResLocalIdent prog $ setConstraintKind "Liveliness" $ do
    TypeSignature _ resTypes <- typeProg prog
    livelinessTypeUses
      livelinessObj
      (resTypes :: [ProgTypeType prog])
      (getProgResIds prog)
      disabled

livelinessSubProgResDefs ::
  forall livelinessObj prog defVarId res ctx.
  ( Mergeable defVarId,
    LivelinessTypeResource livelinessObj res (ProgTypeType prog) ctx,
    ProgTyping prog (ProgTypeType prog),
    MonadFresh ctx,
    ProgNaming prog
  ) =>
  livelinessObj ->
  prog ->
  [defVarId] ->
  SymBool ->
  ctx (UnionDef defVarId res)
livelinessSubProgResDefs livelinessObj prog defVarIds disabled =
  progResLocalIdent prog $ setConstraintKind "Liveliness" $ do
    TypeSignature _ resTypes <- typeProg prog
    livelinessTypeDefs
      livelinessObj
      (resTypes :: [ProgTypeType prog])
      defVarIds
      disabled

livelinessProgStmtDefUses ::
  ( Mergeable varId,
    MonadUnion ctx,
    MonadContext ctx,
    MonadFresh ctx,
    ProgNaming prog,
    ProgUtil prog stmt varId,
    LivelinessOpResource livelinessObj (StmtOpType stmt) res ctx
  ) =>
  livelinessObj ->
  prog ->
  Int ->
  SymBool ->
  ctx (StmtDefUse varId res)
livelinessProgStmtDefUses livelinessObj prog idx disabled =
  progStmtLocalIdent prog idx $ setConstraintKind "Liveliness" $ do
    stmt <- getProgStmtAtIdx prog idx
    let op = getStmtOp stmt
    let argIds = getStmtArgIds stmt
    let resIds = getStmtResIds stmt
    let stmtDisabled = getStmtDisabled stmt
    StmtDefUse stmtDef stmtInvalidatingDef stmtUse <-
      livelinessOpDefUses
        livelinessObj
        op
        argIds
        resIds
        (disabled .|| stmtDisabled)
    mrgReturn $
      StmtDefUse stmtDef stmtInvalidatingDef stmtUse

livelinessProgDefUses ::
  ( Mergeable varId,
    ProgUtil prog stmt varId,
    LivelinessTypeResource livelinessObj res (ProgTypeType prog) ctx,
    ProgTyping prog (ProgTypeType prog),
    MonadFresh ctx,
    MonadUnion ctx,
    ProgNaming prog,
    LivelinessOpResource livelinessObj (StmtOpType stmt) res ctx
  ) =>
  livelinessObj ->
  prog ->
  SymBool ->
  ctx (ProgDefUse varId res)
livelinessProgDefUses livelinessObj prog disabled = do
  argDefs <- livelinessProgArgDefs livelinessObj prog disabled
  stmtDefUses <-
    mrgTraverse
      (\i -> livelinessProgStmtDefUses livelinessObj prog i disabled)
      [0 .. getProgNumStmts prog - 1]
  resUses <- livelinessProgResUses livelinessObj prog disabled
  mrgReturn $ ProgDefUse argDefs stmtDefUses resUses

livelinessSubProgInvalidatingDefs ::
  ( Mergeable outerVarId,
    ProgNaming prog,
    ProgTyping prog (ProgTypeType prog),
    LivelinessTypeResource livelinessObj res (ProgTypeType prog) ctx,
    LivelinessOpResource livelinessObj (StmtOpType stmt) res ctx,
    ProgUtil prog stmt varId,
    Mergeable outerVarId,
    Mergeable varId,
    MonadFresh ctx,
    MonadUnion ctx
  ) =>
  livelinessObj ->
  prog ->
  [outerVarId] ->
  SymBool ->
  ctx (UnionDef outerVarId res)
livelinessSubProgInvalidatingDefs livelinessObj prog varId disabled = do
  ProgDefUse _ stmtDefUses _ <-
    livelinessProgDefUses livelinessObj prog disabled
  let MergeResourceListWithDisabled lst =
        mconcat $
          mergeResourceListFromDefList . stmtInvalidatingDef <$> stmtDefUses
  mrgReturn $ mrgReturn $ uncurry (Def $ head varId) <$> lst

livelinessSubProgDefUses ::
  ( Mergeable outerVarId,
    ProgNaming prog,
    ProgTyping prog (ProgTypeType prog),
    LivelinessTypeResource livelinessObj res (ProgTypeType prog) ctx,
    LivelinessOpResource livelinessObj (StmtOpType stmt) res ctx,
    ProgUtil prog stmt varId,
    Mergeable outerVarId,
    Mergeable varId,
    MonadFresh ctx,
    MonadUnion ctx
  ) =>
  livelinessObj ->
  prog ->
  [outerVarId] ->
  [outerVarId] ->
  SymBool ->
  ctx (StmtDefUse outerVarId res)
livelinessSubProgDefUses livelinessObj prog argIds resIds disabled = do
  resDefs <- livelinessSubProgResDefs livelinessObj prog resIds disabled
  invalidatingDefs <-
    livelinessSubProgInvalidatingDefs livelinessObj prog resIds disabled
  argUses <- livelinessSubProgArgUses livelinessObj prog argIds disabled
  mrgReturn $ StmtDefUse resDefs invalidatingDefs argUses

instance
  ( LivelinessConstraint livelinessObj op ty res ctx,
    OpSubProgConstraints (Liveliness livelinessObj) op ctx,
    ConcreteVarId conVarId,
    ToSym conVarId conVarId,
    MonadFresh ctx,
    Mergeable ty,
    MonadUnion ctx,
    HasSubProgs op subProg ctx,
    ProgConstraints (Liveliness livelinessObj) subProg ctx
  ) =>
  ProgConstraints (Liveliness livelinessObj) (Concrete.Prog op conVarId ty) ctx
  where
  constrainProg
    obj@(Liveliness livelinessObj)
    prog = do
      argDefs <- livelinessProgArgDefs livelinessObj prog (toSym False)
      resUses <- livelinessProgResUses livelinessObj prog (toSym False)
      availableDefs <- goStmts [mrgFmap (fmap (,toSym False)) argDefs] 0
      cannotUseInvalidated resUses availableDefs
      mrgTraverse_
        ( \i -> do
            Concrete.Stmt op _ _ <-
              getProgStmtAtIdx prog i
            progStmtLocalIdent prog i $ constrainOpSubProg obj op
        )
        [0 .. getProgNumStmts prog - 1]
      where
        invalidate ::
          UnionDef conVarId res ->
          [UnionM [(Def conVarId res, SymBool)]] ->
          [UnionM [(Def conVarId res, SymBool)]]
        invalidate invalidatingDef =
          fmap (\defUnion -> invalidateList <$> invalidatingDef <*> defUnion)
        cannotUseInvalidatedUnion ::
          UnionUse conVarId res ->
          UnionM [(Def conVarId res, SymBool)] ->
          ctx ()
        cannotUseInvalidatedUnion useUnion defUnion = do
          uses <- liftUnionM useUnion
          defs <- liftUnionM defUnion
          cannotUseInvalidatedList livelinessObj uses defs
        cannotUseInvalidated ::
          UnionUse conVarId res ->
          [UnionM [(Def conVarId res, SymBool)]] ->
          ctx ()
        cannotUseInvalidated uses =
          mrgTraverse_ (cannotUseInvalidatedUnion uses)
        goStmts ::
          [UnionM [(Def conVarId res, SymBool)]] ->
          Int ->
          ctx [UnionM [(Def conVarId res, SymBool)]]
        goStmts allDefs i
          | i == getProgNumStmts prog = mrgReturn allDefs
          | otherwise = do
              StmtDefUse defs invalidatingDefs uses <-
                livelinessProgStmtDefUses livelinessObj prog i (toSym False)
              cannotUseInvalidated uses allDefs
              let invalidatedDefs = invalidate invalidatingDefs allDefs
              let newDefs = mrgFmap (fmap (,toSym False)) defs : invalidatedDefs
              goStmts newDefs (i + 1)

class
  (Mergeable res, MonadContext ctx) =>
  LivelinessTypeResource livelinessObj res ty ctx
    | livelinessObj -> res
  where
  livelinessTypeDefResource :: livelinessObj -> ty -> ctx (Maybe res)
  livelinessTypeDefResource = livelinessTypeUseResource
  livelinessTypeUseResource :: livelinessObj -> ty -> ctx (Maybe res)
  livelinessTypeUseResource = livelinessTypeDefResource
  {-# MINIMAL livelinessTypeDefResource | livelinessTypeUseResource #-}

type LivelinessConstraint livelinessObj op ty res ctx =
  ( LivelinessOpResource livelinessObj op res ctx,
    LivelinessTypeResource livelinessObj res ty ctx,
    Mergeable op,
    OpTyping op ty ctx
  )

invalidateSingle ::
  (Resource res) =>
  [Def conVarId res] ->
  Def conVarId res ->
  SymBool
invalidateSingle invalidatingDefs def =
  foldl'
    (.||)
    (toSym False)
    [ conflict (defResource invalidatingDef) (defResource def)
      | invalidatingDef <- invalidatingDefs
    ]

--
cannotUseInvalidatedSingle ::
  ( LivelinessName livelinessObj,
    Resource res,
    ConcreteVarId conVarId,
    MonadContext ctx,
    MonadUnion ctx
  ) =>
  livelinessObj ->
  Use conVarId res ->
  Def conVarId res ->
  SymBool ->
  ctx ()
cannotUseInvalidatedSingle livelinessObj use def invalidated = do
  mrgIf (useDisabled use .|| defDisabled def) (return ())
    $ mrgIf
      (toSym (useId use == defId def))
      ( do
          symAssertWith
            ( "Inconsistent use/def resources for "
                <> livelinessName livelinessObj
            )
            (useResource use .== defResource def)
          symAssertWith
            ( "Cannot use invalidated resource for "
                <> livelinessName livelinessObj
            )
            (symNot invalidated)
      )
    $ return ()

cannotUseInvalidatedList ::
  ( LivelinessName livelinessObj,
    Resource res,
    ConcreteVarId conVarId,
    MonadContext ctx,
    MonadUnion ctx
  ) =>
  livelinessObj ->
  [Use conVarId res] ->
  [(Def conVarId res, SymBool)] ->
  ctx ()
cannotUseInvalidatedList livelinessObj uses defsWithInvalidated =
  mrgSequence_
    [ cannotUseInvalidatedSingle livelinessObj use def invalidated
      | use <- uses,
        (def, invalidated) <- defsWithInvalidated
    ]

invalidateList ::
  (Resource res) =>
  [Def conVarId res] ->
  [(Def conVarId res, SymBool)] ->
  [(Def conVarId res, SymBool)]
invalidateList invalidatingDef =
  fmap
    ( \(def, invalidated) ->
        ( def,
          invalidated
            .|| invalidateSingle invalidatingDef def
        )
    )

data ComponentUse varId res = ComponentUse
  { componentUseId :: varId,
    componentUseEffectiveDefId :: varId,
    componentUseResource :: res,
    componentUseDisabled :: SymBool
  }
  deriving (Show, Eq, Generic)
  deriving
    (Mergeable, SEq, EvaluateSym)
    via (Default (ComponentUse varId res))

--
type ComponentUnionUse varId res = UnionM [ComponentUse varId res]

data ComponentStmtDefUse varId res = ComponentStmtDefUse
  { componentStmtDef :: UnionDef varId res,
    componentStmtInvalidatingDef :: UnionDef varId res,
    componentStmtUse :: ComponentUnionUse varId res
  }
  deriving (Show, Eq, Generic)
  deriving
    (Mergeable, SimpleMergeable)
    via (Default (ComponentStmtDefUse varId res))

data ComponentProgDefUse varId res = ComponentProgDefUse
  { componentProgArgDefs :: UnionDef varId res,
    componentProgStmtDefUses :: [ComponentStmtDefUse varId res],
    componentProgResUses :: ComponentUnionUse varId res
  }
  deriving (Show, Eq, Generic)
  deriving (Mergeable) via (Default (ComponentProgDefUse varId res))

livelinessComponentProgStmtDefUses ::
  ( SymbolicVarId varId,
    MonadUnion ctx,
    MonadContext ctx,
    MonadFresh ctx,
    LivelinessOpResource livelinessObj op res ctx
  ) =>
  livelinessObj ->
  Component.Prog op varId ty ->
  Int ->
  SymBool ->
  ctx (ComponentStmtDefUse varId res)
livelinessComponentProgStmtDefUses livelinessObj prog idx disabled = do
  stmt <- getProgStmtAtIdx prog idx
  StmtDefUse defs invalidateDefs uses <-
    livelinessProgStmtDefUses livelinessObj prog idx disabled
  mrgReturn $ ComponentStmtDefUse defs invalidateDefs $ do
    useList <- uses
    mrgReturn $
      ( \use ->
          ComponentUse
            (useId use)
            (head $ getStmtResIds stmt)
            (useResource use)
            (useDisabled use)
      )
        <$> useList

livelinessComponentProgDefUses ::
  ( SymbolicVarId varId,
    Mergeable ty,
    LivelinessTypeResource livelinessObj res ty ctx,
    MonadFresh ctx,
    MonadUnion ctx,
    LivelinessOpResource livelinessObj op res ctx
  ) =>
  livelinessObj ->
  Component.Prog op varId ty ->
  SymBool ->
  ctx (ComponentProgDefUse varId res)
livelinessComponentProgDefUses livelinessObj prog disabled = do
  argDefs <- livelinessProgArgDefs livelinessObj prog disabled
  stmtDefUses <-
    mrgTraverse
      (\i -> livelinessComponentProgStmtDefUses livelinessObj prog i disabled)
      [0 .. getProgNumStmts prog - 1]
  resUses <- livelinessProgResUses livelinessObj prog disabled
  let resEffectiveDefId =
        fromIntegral $
          length (Component.progArgList prog)
            + sum
              (length . Component.stmtResIds <$> Component.progStmtList prog)
  mrgReturn $ ComponentProgDefUse argDefs stmtDefUses $ do
    useList <- resUses
    mrgReturn $
      ( \use ->
          ComponentUse
            (useId use)
            resEffectiveDefId
            (useResource use)
            (useDisabled use)
      )
        <$> useList

instance
  ( LivelinessConstraint livelinessObj op ty res ctx,
    OpSubProgConstraints (Liveliness livelinessObj) op ctx,
    SymbolicVarId symVarId,
    MonadUnion ctx,
    Mergeable ty,
    MonadFresh ctx,
    Show res
  ) =>
  ProgConstraints
    (Liveliness livelinessObj)
    (Component.Prog op symVarId ty)
    ctx
  where
  constrainProg obj@(Liveliness livelinessObj) sketch = do
    ComponentProgDefUse arg stmt res <-
      livelinessComponentProgDefUses livelinessObj sketch (con False)
    let invalidatingDefs = componentStmtInvalidatingDef <$> stmt
    let defs = arg : (componentStmtDef <$> stmt)
    let uses = res : (componentStmtUse <$> stmt)
    mrgTraverse_
      ( \useUnion -> traverseUnion useUnion $ \use ->
          mrgTraverse_ (\d -> defUseConstraint use d invalidatingDefs) defs
      )
      uses
    mrgTraverse_
      ( \i -> do
          Component.Stmt op _ _ _ _ stmtDisabled _ <-
            getProgStmtAtIdx sketch i
          mrgIf stmtDisabled (return ()) $ do
            progStmtLocalIdent sketch i $ constrainOpSubProg obj op
      )
      [0 .. getProgNumStmts sketch - 1]
    where
      traverseUnion ::
        (Mergeable a) => UnionM [a] -> (a -> ctx ()) -> ctx ()
      traverseUnion union f = liftUnionM union >>= mrgTraverse_ f
      defUseConstraint ::
        ComponentUse symVarId res ->
        UnionDef symVarId res ->
        [UnionDef symVarId res] ->
        ctx ()
      defUseConstraint
        (ComponentUse useId eDefId useRes disabled)
        defUnion
        invalidatingDefIds = traverseUnion defUnion $
          \(Def defId defRes defDisabled) -> do
            symUnless
              ( symImplies
                  ( symNot disabled
                      .&& symNot defDisabled
                      .&& (useId .== defId)
                  )
                  (useRes .== defRes)
              )
              $ throwError
              $ "Inconsistent use/def resources for "
                <> livelinessName livelinessObj
            mrgTraverse_
              ( \invalidatingDefUnion ->
                  traverseUnion invalidatingDefUnion $
                    \(Def invalidateDefId invalidateRes invalidateDisabled) ->
                      symUnless
                        ( symImplies
                            ( symNot disabled
                                .&& (useId .== defId)
                                .&& symNot invalidateDisabled
                                .&& conflict defRes invalidateRes
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

instance
  ( ProgConstraints (Liveliness livelinessObj) l ctx,
    ProgConstraints (Liveliness livelinessObj) r ctx
  ) =>
  ProgConstraints
    (Liveliness livelinessObj)
    (SumProg l r)
    ctx
  where
  constrainProg obj (SumProgL prog) = constrainProg obj prog
  constrainProg obj (SumProgR prog) = constrainProg obj prog
