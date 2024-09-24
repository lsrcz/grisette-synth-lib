{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
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
    -- ProgDefUse (..),
    -- livelinessSubProgInvalidatingDefs,
    livelinessSubProgArgUses,
    -- livelinessSubProgResDefs,
    -- livelinessSubProgDefUses,
    livelinessProgArgDefs,
    livelinessProgResUses,
    -- livelinessProgStmtDefUses,
    -- livelinessProgDefUses,
    livelinessOpDefUsesByType,
    livelinessTypeDefs,
    livelinessTypeUses,
    livelinessProgDefUseTable,
    lookupProgDefUse,
    -- ComponentUse (..),
    -- ComponentUnionUse,
    -- ComponentStmtDefUse (..),
    -- ComponentProgDefUse (..),
    -- livelinessComponentProgStmtDefUses,
    -- livelinessComponentProgDefUses,
    ProgDefUseEntry (..),
    ProgDefUseTable (..),
  )
where

import Control.Arrow (Arrow (second))
import Control.DeepSeq (NFData)
import Control.Monad.Error.Class (MonadError (throwError), liftEither)
import Data.Bytes.Serial (Serial)
import Data.Foldable (foldl')
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvalSym,
    LogicalOp (symImplies, symNot, (.&&), (.||)),
    Mergeable (rootStrategy),
    MergingStrategy (SimpleStrategy),
    MonadFresh,
    MonadUnion,
    SimpleMergeable (mrgIte),
    Solvable (con),
    SymBool,
    SymEq ((.==)),
    SymOrd ((.<=), (.>=)),
    ToSym (toSym),
    Union,
    liftUnion,
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
import Grisette.Lib.Synth.Context (MonadAngelicContext, MonadContext)
import Grisette.Lib.Synth.Operator.OpTyping (OpTyping (OpTypeType, typeOp))
import qualified Grisette.Lib.Synth.Program.ComponentSketch.Program as Component
import qualified Grisette.Lib.Synth.Program.Concrete.Program as Concrete
import Grisette.Lib.Synth.Program.ProgConstraints
  ( OpSubProgConstraints (constrainOpSubProg),
    ProgConstraints (constrainProg),
    progArgLocalIdent,
    progNameArgLocalIdent,
    progNameResLocalIdent,
    progNameStmtLocalIdent,
    progResLocalIdent,
    progStmtLocalIdent,
    setConstraintKind,
  )
import Grisette.Lib.Synth.Program.ProgTyping
  ( ProgTypeTable,
    ProgTyping (typeProg),
    typeSymbolTable,
  )
import Grisette.Lib.Synth.Program.ProgUtil
  ( ProgUtil (ProgOpType, ProgTypeType, ProgVarIdType),
    ProgUtilImpl
      ( getProgArgIds,
        getProgNumStmts,
        getProgResIds,
        getProgStmtAtIdx
      ),
    StmtUtilImpl
      ( getStmtArgIds,
        getStmtDisabled,
        getStmtOp,
        getStmtResIds
      ),
  )
import Grisette.Lib.Synth.Program.SumProg (SumOp (SumOpL, SumOpR), SumProg (SumProgL, SumProgR))
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable (SymbolTable))
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

class (SymEq res, SimpleMergeable res) => Resource res where
  conflict :: res -> res -> SymBool

data Def varId res = Def
  { defId :: varId,
    defResource :: res,
    defDisabled :: SymBool
  }
  deriving (Show, Eq, Generic)
  deriving
    (Mergeable, SymEq, EvalSym)
    via (Default (Def varId res))

data Use varId res = Use
  { useId :: varId,
    useResource :: res,
    useDisabled :: SymBool
  }
  deriving (Show, Eq, Generic)
  deriving (Mergeable, SymEq, EvalSym) via (Default (Use varId res))

newtype Liveliness livelinessObj = Liveliness livelinessObj
  deriving (Eq, Generic)
  deriving anyclass (NFData, Serial)

type UnionDef varId res = Union [Def varId res]

type UnionUse varId res = Union [Use varId res]

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

{-

  -}

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
    ProgDefUseTable res ctx ->
    ProgTypeTable (OpTypeType op) ->
    op ->
    [varId] ->
    [varId] ->
    SymBool ->
    ctx (StmtDefUse varId res)
  default livelinessOpDefUses ::
    ( Mergeable varId,
      OpTyping op ctx,
      LivelinessTypeResource livelinessObj res (OpTypeType op) ctx
    ) =>
    livelinessObj ->
    ProgDefUseTable res ctx ->
    ProgTypeTable (OpTypeType op) ->
    op ->
    [varId] ->
    [varId] ->
    SymBool ->
    ctx (StmtDefUse varId res)
  livelinessOpDefUses obj _ = livelinessOpDefUsesByType obj

instance
  ( LivelinessOpResource livelinessObj op1 res ctx,
    LivelinessOpResource livelinessObj op2 res ctx,
    OpTypeType op1 ~ OpTypeType op2
  ) =>
  LivelinessOpResource livelinessObj (SumOp op1 op2) res ctx
  where
  livelinessOpDefUses livelinessObj duTable tyTable (SumOpL op) =
    livelinessOpDefUses livelinessObj duTable tyTable op
  livelinessOpDefUses livelinessObj duTable tyTable (SumOpR op) =
    livelinessOpDefUses livelinessObj duTable tyTable op

-- class LivelinessProgResource livelinessObj prog res | livelinessObj -> res where
--   livelinessProgDefUses ::
--     livelinessObj ->
--     ProgDefUseTable res ->
--     ProgTypeTable (ProgTypeType prog) ->
--     prog ->
--     ProgDefUseEntry res

data ProgDefUseEntry res ctx where
  ProgDefUseEntry ::
    ( forall varId.
      (Mergeable varId) =>
      [varId] ->
      [varId] ->
      SymBool ->
      ctx (StmtDefUse varId res)
    ) ->
    ProgDefUseEntry res ctx

newtype ProgDefUseTable res ctx = ProgDefUseTable [(T.Text, ProgDefUseEntry res ctx)]

lookupProgDefUse ::
  (MonadContext ctx) => ProgDefUseTable res ctx -> T.Text -> ctx (ProgDefUseEntry res ctx)
lookupProgDefUse (ProgDefUseTable table) key =
  case lookup key table of
    Just entry -> return entry
    Nothing -> throwError $ "No program with key " <> key

livelinessOpDefUsesByType ::
  ( Mergeable varId,
    OpTyping op ctx,
    LivelinessTypeResource livelinessObj res (OpTypeType op) ctx
  ) =>
  livelinessObj ->
  ProgTypeTable (OpTypeType op) ->
  op ->
  [varId] ->
  [varId] ->
  SymBool ->
  ctx (StmtDefUse varId res)
livelinessOpDefUsesByType livelinessObj table op argIds resIds disabled = do
  ty <- typeOp table op
  defs <- livelinessTypeDefs livelinessObj (resTypes ty) resIds disabled
  uses <- livelinessTypeUses livelinessObj (argTypes ty) argIds disabled
  mrgReturn $ StmtDefUse defs defs uses

instance
  ( LivelinessOpResource livelinessObj op res ctx,
    MonadUnion ctx,
    Mergeable op
  ) =>
  LivelinessOpResource livelinessObj (Union op) res ctx
  where
  livelinessOpDefUses livelinessObj duTable tyTable opUnion argIds resIds disabled =
    liftUnion opUnion
      .>>= \op ->
        livelinessOpDefUses livelinessObj duTable tyTable op argIds resIds disabled

livelinessProgArgDefs ::
  forall livelinessObj prog varId res ctx.
  ( Mergeable varId,
    varId ~ ProgVarIdType prog,
    ProgUtil prog,
    LivelinessTypeResource livelinessObj res (ProgTypeType prog) ctx,
    ProgTyping prog,
    MonadFresh ctx
  ) =>
  livelinessObj ->
  T.Text ->
  prog ->
  SymBool ->
  ctx (UnionDef varId res)
livelinessProgArgDefs livelinessObj key prog disabled =
  progNameArgLocalIdent key $ setConstraintKind "Liveliness" $ do
    TypeSignature argTypes _ <- liftEither $ typeProg prog
    livelinessTypeDefs
      livelinessObj
      (argTypes :: [ProgTypeType prog])
      (getProgArgIds prog :: [varId])
      disabled

livelinessProgResUses ::
  forall livelinessObj prog varId res ctx.
  ( ProgUtil prog,
    varId ~ ProgVarIdType prog,
    LivelinessTypeResource livelinessObj res (ProgTypeType prog) ctx,
    ProgTyping prog,
    MonadFresh ctx,
    Mergeable varId
  ) =>
  livelinessObj ->
  T.Text ->
  prog ->
  SymBool ->
  ctx (UnionUse varId res)
livelinessProgResUses livelinessObj key prog disabled =
  progNameResLocalIdent key $ setConstraintKind "Liveliness" $ do
    TypeSignature _ resTypes <- liftEither $ typeProg prog
    livelinessTypeUses
      livelinessObj
      (resTypes :: [ProgTypeType prog])
      (getProgResIds prog)
      disabled

livelinessProgStmtDefUses ::
  ( Mergeable varId,
    MonadAngelicContext ctx,
    ProgUtil prog,
    LivelinessOpResource livelinessObj (ProgOpType prog) res ctx,
    varId ~ ProgVarIdType prog,
    OpTypeType (ProgOpType prog) ~ ProgTypeType prog
  ) =>
  livelinessObj ->
  ProgDefUseTable res ctx ->
  ProgTypeTable (ProgTypeType prog) ->
  T.Text ->
  prog ->
  Int ->
  SymBool ->
  ctx (StmtDefUse varId res)
livelinessProgStmtDefUses livelinessObj duTable tyTable key prog idx disabled =
  progNameStmtLocalIdent key idx $ setConstraintKind "Liveliness" $ do
    stmt <- getProgStmtAtIdx prog idx
    let op = getStmtOp stmt
    let argIds = getStmtArgIds stmt
    let resIds = getStmtResIds stmt
    let stmtDisabled = getStmtDisabled stmt
    StmtDefUse stmtDef stmtInvalidatingDef stmtUse <-
      livelinessOpDefUses
        livelinessObj
        duTable
        tyTable
        op
        argIds
        resIds
        (disabled .|| stmtDisabled)
    mrgReturn $
      StmtDefUse stmtDef stmtInvalidatingDef stmtUse

data ProgDefUse varId res = ProgDefUse
  { progArgDefs :: UnionDef varId res,
    progStmtDefUses :: [StmtDefUse varId res],
    progResUses :: UnionUse varId res
  }
  deriving (Show, Eq, Generic)
  deriving (Mergeable) via (Default (ProgDefUse varId res))

livelinessSubProgInvalidatingDefs ::
  ( Mergeable outerVarId,
    ProgTyping prog,
    LivelinessTypeResource livelinessObj res (ProgTypeType prog) ctx,
    LivelinessOpResource livelinessObj (ProgOpType prog) res ctx,
    ProgUtil prog,
    Mergeable outerVarId,
    Mergeable varId,
    varId ~ ProgVarIdType prog,
    MonadFresh ctx,
    MonadUnion ctx,
    OpTypeType (ProgOpType prog) ~ ProgTypeType prog
  ) =>
  livelinessObj ->
  ProgDefUseTable res ctx ->
  ProgTypeTable (ProgTypeType prog) ->
  T.Text ->
  prog ->
  [outerVarId] ->
  SymBool ->
  ctx (UnionDef outerVarId res)
livelinessSubProgInvalidatingDefs
  livelinessObj
  duTable
  table
  key
  prog
  varId
  disabled = do
    ProgDefUse _ stmtDefUses _ <-
      livelinessProgDefUses livelinessObj duTable table key prog disabled
    let MergeResourceListWithDisabled lst =
          mconcat $
            mergeResourceListFromDefList . stmtInvalidatingDef <$> stmtDefUses
    mrgReturn $ mrgReturn $ uncurry (Def $ head varId) <$> lst

livelinessProgDefUses ::
  ( Mergeable varId,
    ProgUtil prog,
    LivelinessTypeResource livelinessObj res (ProgTypeType prog) ctx,
    ProgTyping prog,
    MonadFresh ctx,
    MonadUnion ctx,
    varId ~ ProgVarIdType prog,
    LivelinessOpResource livelinessObj (ProgOpType prog) res ctx,
    OpTypeType (ProgOpType prog) ~ ProgTypeType prog
  ) =>
  livelinessObj ->
  ProgDefUseTable res ctx ->
  ProgTypeTable (ProgTypeType prog) ->
  T.Text ->
  prog ->
  SymBool ->
  ctx (ProgDefUse varId res)
livelinessProgDefUses livelinessObj duTable tyTable key prog disabled = do
  argDefs <- livelinessProgArgDefs livelinessObj key prog disabled
  stmtDefUses <-
    mrgTraverse
      (\i -> livelinessProgStmtDefUses livelinessObj duTable tyTable key prog i disabled)
      [0 .. getProgNumStmts prog - 1]
  resUses <- livelinessProgResUses livelinessObj key prog disabled
  mrgReturn $ ProgDefUse argDefs stmtDefUses resUses

livelinessSubProgResDefs ::
  forall livelinessObj prog defVarId res ctx.
  ( Mergeable defVarId,
    LivelinessTypeResource livelinessObj res (ProgTypeType prog) ctx,
    ProgTyping prog,
    MonadFresh ctx
  ) =>
  livelinessObj ->
  T.Text ->
  prog ->
  [defVarId] ->
  SymBool ->
  ctx (UnionDef defVarId res)
livelinessSubProgResDefs livelinessObj key prog defVarIds disabled =
  progNameResLocalIdent key $ setConstraintKind "Liveliness" $ do
    TypeSignature _ resTypes <- liftEither $ typeProg prog
    livelinessTypeDefs
      livelinessObj
      (resTypes :: [ProgTypeType prog])
      defVarIds
      disabled

livelinessSubProgArgUses ::
  forall livelinessObj prog useVarId res ctx.
  ( Mergeable useVarId,
    LivelinessTypeResource livelinessObj res (ProgTypeType prog) ctx,
    ProgTyping prog,
    MonadFresh ctx
  ) =>
  livelinessObj ->
  T.Text ->
  prog ->
  [useVarId] ->
  SymBool ->
  ctx (UnionUse useVarId res)
livelinessSubProgArgUses livelinessObj key prog useVarIds disabled =
  progNameArgLocalIdent key $ setConstraintKind "Liveliness" $ do
    TypeSignature argTypes _ <- liftEither $ typeProg prog
    livelinessTypeUses
      livelinessObj
      (argTypes :: [ProgTypeType prog])
      useVarIds
      disabled

livelinessSubProgDefUses ::
  ( ProgTyping prog,
    LivelinessTypeResource livelinessObj res (ProgTypeType prog) ctx,
    LivelinessOpResource livelinessObj (ProgOpType prog) res ctx,
    ProgUtil prog,
    Mergeable varId,
    varId ~ ProgVarIdType prog,
    MonadAngelicContext ctx,
    OpTypeType (ProgOpType prog) ~ ProgTypeType prog
  ) =>
  livelinessObj ->
  ProgDefUseTable res ctx ->
  ProgTypeTable (ProgTypeType prog) ->
  T.Text ->
  prog ->
  ProgDefUseEntry res ctx
livelinessSubProgDefUses livelinessObj duTable tyTable key prog =
  ProgDefUseEntry $ \argIds resIds disabled -> do
    resDefs <- livelinessSubProgResDefs livelinessObj key prog resIds disabled
    invalidatingDefs <-
      livelinessSubProgInvalidatingDefs
        livelinessObj
        duTable
        tyTable
        key
        prog
        resIds
        disabled
    argUses <- livelinessSubProgArgUses livelinessObj key prog argIds disabled
    mrgReturn $ StmtDefUse resDefs invalidatingDefs argUses

livelinessProgDefUseTable ::
  ( ProgTyping prog,
    LivelinessTypeResource livelinessObj res (ProgTypeType prog) ctx,
    LivelinessOpResource livelinessObj (ProgOpType prog) res ctx,
    Mergeable (ProgVarIdType prog),
    MonadAngelicContext ctx,
    OpTypeType (ProgOpType prog) ~ ProgTypeType prog
  ) =>
  livelinessObj ->
  SymbolTable prog ->
  ProgDefUseTable res ctx
livelinessProgDefUseTable livelinessObj t@(SymbolTable table) =
  let tyTable = typeSymbolTable t
      res =
        ProgDefUseTable $
          fmap
            ( \(key, prog) ->
                (key, livelinessSubProgDefUses livelinessObj res tyTable key prog)
            )
            table
   in res

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
    LivelinessTypeResource livelinessObj res (OpTypeType op) ctx,
    Mergeable op,
    Mergeable ty,
    OpTyping op ctx,
    ty ~ OpTypeType op
  )

class LivelinessProgConstraints livelinessObj prog res ctx | livelinessObj -> res where
  conx ::
    livelinessObj ->
    ProgDefUseTable res ctx ->
    ProgTypeTable (ProgTypeType prog) ->
    T.Text ->
    prog ->
    ctx ()

instance
  ( LivelinessProgConstraints livelinessObj prog1 res ctx,
    LivelinessProgConstraints livelinessObj prog2 res ctx,
    ProgTypeType prog1 ~ ProgTypeType prog2
  ) =>
  LivelinessProgConstraints livelinessObj (SumProg prog1 prog2) res ctx
  where
  conx livelinessObj duTable tyTable key (SumProgL prog) =
    conx livelinessObj duTable tyTable key prog
  conx livelinessObj duTable tyTable key (SumProgR prog) =
    conx livelinessObj duTable tyTable key prog

instance
  ( LivelinessConstraint livelinessObj op ty res ctx,
    MonadAngelicContext ctx,
    ConcreteVarId conVarId
  ) =>
  LivelinessProgConstraints
    livelinessObj
    (Concrete.Prog op conVarId ty)
    res
    ctx
  where
  conx livelinessObj duTable tyTable key prog = do
    argDefs <- livelinessProgArgDefs livelinessObj key prog (toSym False)
    resUses <- livelinessProgResUses livelinessObj key prog (toSym False)
    availableDefs <-
      goStmts duTable tyTable key prog [mrgFmap (fmap (,toSym False)) argDefs] 0
    cannotUseInvalidated resUses availableDefs
    mrgTraverse_
      ( \i -> do
          Concrete.Stmt op _ _ <-
            getProgStmtAtIdx prog i
          undefined
          -- progStmtLocalIdent prog i $ constrainOpSubProg livelinessObj op
      )
      [0 .. getProgNumStmts prog - 1]
    where


      invalidate ::
        UnionDef conVarId res ->
        [Union [(Def conVarId res, SymBool)]] ->
        [Union [(Def conVarId res, SymBool)]]
      invalidate invalidatingDef =
        fmap (\defUnion -> invalidateList <$> invalidatingDef <*> defUnion)
      cannotUseInvalidatedUnion ::
        UnionUse conVarId res ->
        Union [(Def conVarId res, SymBool)] ->
        ctx ()
      cannotUseInvalidatedUnion useUnion defUnion = do
        uses <- liftUnion useUnion
        defs <- liftUnion defUnion
        cannotUseInvalidatedList livelinessObj uses defs
      cannotUseInvalidated ::
        UnionUse conVarId res ->
        [Union [(Def conVarId res, SymBool)]] ->
        ctx ()
      cannotUseInvalidated uses =
        mrgTraverse_ (cannotUseInvalidatedUnion uses)
      -- goStmts ::
      --   [Union [(Def conVarId res, SymBool)]] ->
      --   Int ->
      --   ctx [Union [(Def conVarId res, SymBool)]]
      goStmts duTable tyTable key prog allDefs i
        | i == getProgNumStmts prog = mrgReturn allDefs
        | otherwise = do
            StmtDefUse defs invalidatingDefs uses <-
              livelinessProgStmtDefUses livelinessObj duTable tyTable key prog i (toSym False)
            cannotUseInvalidated uses allDefs
            let invalidatedDefs = invalidate invalidatingDefs allDefs
            let newDefs = mrgFmap (fmap (,toSym False)) defs : invalidatedDefs
            goStmts duTable tyTable key prog newDefs (i + 1)

{-
instance
  ( LivelinessConstraint livelinessObj op ty res ctx,
    OpSubProgConstraints (Liveliness livelinessObj) op ctx,
    ConcreteVarId conVarId,
    ToSym conVarId conVarId,
    Mergeable ty,
    MonadFresh ctx,
    MonadUnion ctx
  ) =>
  ProgConstraints
    (Liveliness livelinessObj)
    (Concrete.Prog op conVarId ty)
    ctx
  where
  constrainProg obj@(Liveliness livelinessObj) table@(SymbolTable t) = do
    let tyTable = typeSymbolTable table
    let duTable = livelinessProgDefUseTable livelinessObj table
    let f key prog = do
          argDefs <- livelinessProgArgDefs livelinessObj key prog (toSym False)
          resUses <- livelinessProgResUses livelinessObj key prog (toSym False)
          availableDefs <-
            goStmts duTable tyTable key prog [mrgFmap (fmap (,toSym False)) argDefs] 0
          cannotUseInvalidated resUses availableDefs
          mrgTraverse_
            ( \i -> do
                Concrete.Stmt op _ _ <-
                  getProgStmtAtIdx prog i
                progStmtLocalIdent prog i $ constrainOpSubProg obj op
            )
            [0 .. getProgNumStmts prog - 1]
    mapM_ (uncurry f) t
    where
      invalidate ::
        UnionDef conVarId res ->
        [Union [(Def conVarId res, SymBool)]] ->
        [Union [(Def conVarId res, SymBool)]]
      invalidate invalidatingDef =
        fmap (\defUnion -> invalidateList <$> invalidatingDef <*> defUnion)
      cannotUseInvalidatedUnion ::
        UnionUse conVarId res ->
        Union [(Def conVarId res, SymBool)] ->
        ctx ()
      cannotUseInvalidatedUnion useUnion defUnion = do
        uses <- liftUnion useUnion
        defs <- liftUnion defUnion
        cannotUseInvalidatedList livelinessObj uses defs
      cannotUseInvalidated ::
        UnionUse conVarId res ->
        [Union [(Def conVarId res, SymBool)]] ->
        ctx ()
      cannotUseInvalidated uses =
        mrgTraverse_ (cannotUseInvalidatedUnion uses)
      -- goStmts ::
      --   [Union [(Def conVarId res, SymBool)]] ->
      --   Int ->
      --   ctx [Union [(Def conVarId res, SymBool)]]
      goStmts duTable tyTable key prog allDefs i
        | i == getProgNumStmts prog = mrgReturn allDefs
        | otherwise = do
            StmtDefUse defs invalidatingDefs uses <-
              livelinessProgStmtDefUses livelinessObj duTable tyTable key prog i (toSym False)
            cannotUseInvalidated uses allDefs
            let invalidatedDefs = invalidate invalidatingDefs allDefs
            let newDefs = mrgFmap (fmap (,toSym False)) defs : invalidatedDefs
            goStmts duTable tyTable key prog newDefs (i + 1)
            -}

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

--
data ComponentUse varId res = ComponentUse
  { componentUseId :: varId,
    componentUseEffectiveDefId :: varId,
    componentUseResource :: res,
    componentUseDisabled :: SymBool
  }
  deriving (Show, Eq, Generic)
  deriving
    (Mergeable, SymEq, EvalSym)
    via (Default (ComponentUse varId res))

type ComponentUnionUse varId res = Union [ComponentUse varId res]

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
    LivelinessOpResource livelinessObj op res ctx,
    ty ~ OpTypeType op
  ) =>
  livelinessObj ->
  ProgDefUseTable res ctx ->
  ProgTypeTable ty ->
  T.Text ->
  Component.Prog op varId ty ->
  Int ->
  SymBool ->
  ctx (ComponentStmtDefUse varId res)
livelinessComponentProgStmtDefUses
  livelinessObj
  duTable
  table
  key
  prog
  idx
  disabled = do
    stmt <- getProgStmtAtIdx prog idx
    StmtDefUse defs invalidateDefs uses <-
      livelinessProgStmtDefUses livelinessObj duTable table key prog idx disabled
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
    LivelinessOpResource livelinessObj op res ctx,
    ty ~ OpTypeType op
  ) =>
  livelinessObj ->
  ProgDefUseTable res ctx ->
  ProgTypeTable ty ->
  T.Text ->
  Component.Prog op varId ty ->
  SymBool ->
  ctx (ComponentProgDefUse varId res)
livelinessComponentProgDefUses livelinessObj duTable table key prog disabled = do
  argDefs <- livelinessProgArgDefs livelinessObj key prog disabled
  stmtDefUses <-
    mrgTraverse
      ( \i ->
          livelinessComponentProgStmtDefUses
            livelinessObj
            duTable
            table
            key
            prog
            i
            disabled
      )
      [0 .. getProgNumStmts prog - 1]
  resUses <- livelinessProgResUses livelinessObj key prog disabled
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
  ( MonadAngelicContext ctx,
    LivelinessConstraint livelinessObj op ty res ctx,
    SymbolicVarId symVarId
  ) =>
  LivelinessProgConstraints livelinessObj (Component.Prog op symVarId ty) res ctx
  where
  conx livelinessObj duTable tyTable key prog = do
    ComponentProgDefUse arg stmt res <-
      livelinessComponentProgDefUses
        livelinessObj
        duTable
        tyTable
        key
        prog
        (con False)
    let invalidatingDefs = componentStmtInvalidatingDef <$> stmt
    let defs = arg : (componentStmtDef <$> stmt)
    let uses = res : (componentStmtUse <$> stmt)
    mrgTraverse_
      ( \useUnion -> traverseUnion useUnion $ \use ->
          mrgTraverse_ (\d -> defUseConstraint use d invalidatingDefs) defs
      )
      uses
    -- mrgTraverse_
    --   ( \i -> do
    --       Component.Stmt op _ _ _ _ stmtDisabled _ <-
    --         getProgStmtAtIdx sketch i
    --       mrgIf stmtDisabled (return ()) $ do
    --         progStmtLocalIdent sketch i $ constrainOpSubProg obj op
    --   )
    --   [0 .. getProgNumStmts sketch - 1]
    where
      traverseUnion ::
        (Mergeable a) => Union [a] -> (a -> ctx ()) -> ctx ()
      traverseUnion union f = liftUnion union >>= mrgTraverse_ f
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

{-
instance
  ( LivelinessConstraint livelinessObj op ty res ctx,
    OpSubProgConstraints (Liveliness livelinessObj) op ctx,
    SymbolicVarId symVarId,
    MonadUnion ctx,
    Mergeable ty,
    ty ~ OpTypeType op,
    MonadFresh ctx,
    Show res
  ) =>
  ProgConstraints
    (Liveliness livelinessObj)
    (Component.Prog op symVarId ty)
    ctx
  where
  constrainProg obj@(Liveliness livelinessObj) table sketch = do
    ComponentProgDefUse arg stmt res <-
      livelinessComponentProgDefUses livelinessObj table sketch (con False)
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
        (Mergeable a) => Union [a] -> (a -> ctx ()) -> ctx ()
      traverseUnion union f = liftUnion union >>= mrgTraverse_ f
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
    ProgConstraints (Liveliness livelinessObj) r ctx,
    ProgTypeType l ~ ProgTypeType r
  ) =>
  ProgConstraints
    (Liveliness livelinessObj)
    (SumProg l r)
    ctx
  where
  constrainAllProg obj table (SumProgL prog) = constrainProg obj table prog
  constrainAllProg obj table (SumProgR prog) = constrainProg obj table prog

-}
instance
  ( LivelinessProgConstraints livelinessObj prog res ctx,
    MonadContext ctx,
    ProgTyping prog,
    LivelinessTypeResource livelinessObj res (ProgTypeType prog) ctx,
    LivelinessOpResource livelinessObj (ProgOpType prog) res ctx,
    Mergeable (ProgVarIdType prog),
    MonadAngelicContext ctx,
    ProgUtil prog,
    OpTypeType (ProgOpType prog) ~ ProgTypeType prog
  ) =>
  ProgConstraints (Liveliness livelinessObj) prog ctx
  where
  constrainProg (Liveliness livelinessObj) table@(SymbolTable t) = do
    let tyTable = typeSymbolTable table
    let duTable = livelinessProgDefUseTable livelinessObj table
    mapM_ (uncurry (conx livelinessObj duTable tyTable)) t
