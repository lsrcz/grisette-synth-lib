{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Grisette.Lib.Synth.Program.ComponentSketch.Program
  ( Stmt (..),
    ProgArg (..),
    ProgRes (..),
    Prog (..),
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (join)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State
  ( MonadState (get, put),
    MonadTrans (lift),
    State,
    StateT,
    evalState,
    gets,
  )
import Data.Bifunctor (Bifunctor (first))
import Data.Data (Proxy (Proxy))
import Data.Foldable (Foldable (foldl'))
import qualified Data.HashMap.Lazy as M
import Data.Hashable (Hashable)
import Data.List (sortOn, tails)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvaluateSym,
    LogicalOp (symImplies, (.&&), (.||)),
    Mergeable (rootStrategy),
    MergingStrategy (NoStrategy),
    MonadUnion,
    SEq ((./=), (.==)),
    SOrd ((.<), (.<=), (.>)),
    Solvable (con),
    SymBool,
    ToCon (toCon),
    ToSym (toSym),
    UnionM,
    mrgFmap,
    mrgIf,
    mrgSequence_,
    mrgTraverse_,
    symAny,
    symAssertWith,
  )
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Control.Monad.State.Class (mrgModify)
import Grisette.Lib.Control.Monad.Trans.State (mrgEvalStateT)
import Grisette.Lib.Synth.Context (MonadAngelicContext, MonadContext)
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Operator.OpTyping (OpTyping (typeOp))
import Grisette.Lib.Synth.Program.ComponentSketch.GenIntermediate
  ( GenIntermediate,
    Intermediates (Intermediates),
    genIntermediates,
    genOpIntermediates,
  )
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.CostModel.PerStmtCostModel
  ( OpCost (opCost),
    PerStmtCostObj (PerStmtCostObj),
  )
import Grisette.Lib.Synth.Program.ProgCost (ProgCost (progCost))
import Grisette.Lib.Synth.Program.ProgNaming (ProgNaming (nameProg))
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
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
import Grisette.Lib.Synth.TypeSignature
  ( TypeSignature (TypeSignature),
  )
import Grisette.Lib.Synth.Util.Show (showText)
import Grisette.Lib.Synth.VarId (ConcreteVarId, RelatedVarId, SymbolicVarId)

data Stmt op symVarId = Stmt
  { stmtOp :: op,
    stmtArgIds :: [symVarId],
    stmtArgNum :: symVarId,
    stmtResIds :: [symVarId],
    stmtResNum :: symVarId,
    stmtDisabled :: SymBool,
    stmtMustBeAfter :: [symVarId]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData)
  deriving (EvaluateSym) via (Default (Stmt op symVarId))

deriving via
  (Default (Stmt conOp symVarId))
  instance
    (ToCon symOp conOp, ToCon symVarId symVarId) =>
    ToCon (Stmt symOp symVarId) (Stmt conOp symVarId)

deriving via
  (Default (Stmt symOp symVarId))
  instance
    (ToSym conOp symOp, ToSym symVarId symVarId) =>
    ToSym (Stmt conOp symVarId) (Stmt symOp symVarId)

instance Mergeable (Stmt op symVarId) where
  rootStrategy = NoStrategy

data ProgArg ty = ProgArg
  { progArgName :: T.Text,
    progArgType :: ty
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData)
  deriving (EvaluateSym) via (Default (ProgArg ty))

deriving via
  (Default (ProgArg conTy))
  instance
    (ToCon symTy conTy) =>
    ToCon (ProgArg symTy) (ProgArg conTy)

deriving via
  (Default (ProgArg symTy))
  instance
    (ToSym conTy symTy) =>
    ToSym (ProgArg conTy) (ProgArg symTy)

instance Mergeable (ProgArg ty) where
  rootStrategy = NoStrategy

data ProgRes symVarId ty = ProgRes
  { progResId :: symVarId,
    progResType :: ty
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData)
  deriving (EvaluateSym) via (Default (ProgRes symVarId ty))

deriving via
  (Default (ProgRes symVarId conTy))
  instance
    (ToCon symTy conTy, ToCon symVarId symVarId) =>
    ToCon (ProgRes symVarId symTy) (ProgRes symVarId conTy)

deriving via
  (Default (ProgRes symVarId symTy))
  instance
    (ToSym conTy symTy, ToSym symVarId symVarId) =>
    ToSym (ProgRes symVarId conTy) (ProgRes symVarId symTy)

instance Mergeable (ProgRes symVarId ty) where
  rootStrategy = NoStrategy

data Prog op symVarId ty = Prog
  { progName :: T.Text,
    progArgList :: [ProgArg ty],
    progStmtList :: [Stmt op symVarId],
    progResList :: [ProgRes symVarId ty]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData)
  deriving (EvaluateSym) via (Default (Prog op symVarId ty))

deriving via
  (Default (Prog conOp symVarId conTy))
  instance
    (ToCon symOp conOp, ToCon symTy conTy, ToCon symVarId symVarId) =>
    ToCon (Prog symOp symVarId symTy) (Prog conOp symVarId conTy)

deriving via
  (Default (Prog symOp symVarId symTy))
  instance
    (ToSym conOp symOp, ToSym conTy symTy, ToSym symVarId symVarId) =>
    ToSym (Prog conOp symVarId conTy) (Prog symOp symVarId symTy)

instance Mergeable (Prog op symVarId ty) where
  rootStrategy = NoStrategy

instance
  ( ToSym conOp symOp,
    ConcreteVarId conVarId,
    SymbolicVarId symVarId,
    Mergeable symOp,
    ToSym conTy symTy
  ) =>
  ToSym (Concrete.Prog conOp conVarId conTy) (Prog symOp symVarId symTy)
  where
  toSym (Concrete.Prog name argList stmtList resList) =
    flip evalState initialMapping $ do
      stmts <- traverse toSymStmt stmtList
      res <- traverse toSymRes resList
      return $ Prog name componentArgList stmts res
    where
      componentArgList =
        (\(Concrete.ProgArg name _ ty) -> ProgArg name (toSym ty)) <$> argList
      initialMapping :: M.HashMap conVarId symVarId
      initialMapping =
        M.fromList $
          first Concrete.progArgId <$> zip argList (fromIntegral <$> [0 ..])
      lookupId :: conVarId -> State (M.HashMap conVarId symVarId) symVarId
      lookupId varId = gets (fromMaybe (-1) . M.lookup varId)
      addId :: conVarId -> State (M.HashMap conVarId symVarId) symVarId
      addId conVarId = do
        m <- get
        let newId = fromIntegral $ M.size m
        put $ M.insert conVarId newId m
        return newId
      toSymStmt ::
        Concrete.Stmt conOp conVarId ->
        State (M.HashMap conVarId symVarId) (Stmt symOp symVarId)
      toSymStmt (Concrete.Stmt op argIds resIds) = do
        let symOp = toSym op
        conArgIds <- traverse lookupId argIds
        conResIds <- traverse addId resIds
        return $
          Stmt
            { stmtOp = symOp,
              stmtArgIds = conArgIds,
              stmtArgNum = fromIntegral $ length conArgIds,
              stmtResIds = conResIds,
              stmtResNum = fromIntegral $ length conResIds,
              stmtDisabled = con False,
              stmtMustBeAfter = []
            }
      toSymRes ::
        Concrete.ProgRes conVarId conTy ->
        State (M.HashMap conVarId symVarId) (ProgRes symVarId symTy)
      toSymRes (Concrete.ProgRes conId ty) = do
        symId <- lookupId conId
        let symTy = toSym ty
        return $ ProgRes symId symTy

instance
  ( Mergeable symOp,
    ToCon symOp conOp,
    RelatedVarId conVarId symVarId,
    ToCon symTy conTy
  ) =>
  ToCon (Prog symOp symVarId symTy) (Concrete.Prog conOp conVarId conTy)
  where
  toCon (Prog name argList stmtList resList) = do
    conArgList <-
      traverse
        ( \(ProgArg name ty, varId) ->
            Concrete.ProgArg name varId <$> toCon ty
        )
        $ zip argList [0 ..]
    let toConStmt (Stmt op argIds argNum resIds resNum disabled _) = do
          disabled <- toCon disabled
          if disabled
            then return []
            else do
              conOp <- toCon op
              conArgIds <- toCon argIds
              conArgNum :: conVarId <- toCon argNum
              conResIds <- toCon resIds
              conResNum :: conVarId <- toCon resNum
              return
                [ Concrete.Stmt
                    conOp
                    (take (fromIntegral conArgNum) conArgIds)
                    (take (fromIntegral conResNum) conResIds)
                ]
    conStmts <- join <$> traverse toConStmt stmtList
    conResList <-
      traverse
        ( \(ProgRes resId ty) -> do
            conResId <- toCon resId
            conTy <- toCon ty
            return $
              Concrete.ProgRes
                { Concrete.progResId = conResId,
                  Concrete.progResType = conTy
                }
        )
        resList
    return $
      Concrete.Prog
        name
        conArgList
        (sortOn (listToMaybe . Concrete.stmtResIds) conStmts)
        conResList

data IdValPair symVarId val = IdValPair SymBool symVarId (UnionM (Maybe val))
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym, Mergeable) via Default (IdValPair symVarId val)

data CollectedDefUse symVarId val = CollectedDefUse
  { collectedDef :: [IdValPair symVarId val],
    collectedUse :: [IdValPair symVarId val]
  }
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym, Mergeable) via Default (CollectedDefUse symVarId val)

addDefs ::
  ( MonadState (CollectedDefUse symVarId val) ctx,
    MonadUnion ctx,
    SymbolicVarId symVarId,
    Mergeable val
  ) =>
  [IdValPair symVarId val] ->
  ctx ()
addDefs def = mrgModify $ \s -> s {collectedDef = def ++ collectedDef s}

addUses ::
  ( MonadState (CollectedDefUse symVarId val) ctx,
    MonadUnion ctx,
    SymbolicVarId symVarId,
    Mergeable val
  ) =>
  [IdValPair symVarId val] ->
  ctx ()
addUses use = mrgModify $ \s -> s {collectedUse = use ++ collectedUse s}

addProgArgs ::
  ( MonadState (CollectedDefUse symVarId val) ctx,
    MonadUnion ctx,
    SymbolicVarId symVarId,
    Mergeable val
  ) =>
  [val] ->
  ctx ()
addProgArgs args =
  addDefs
    ( zipWith
        (\argId val -> IdValPair (con False) argId (mrgReturn $ Just val))
        (fromIntegral <$> [0 ..])
        args
    )

genProgResVals ::
  ( SymbolicVarId symVarId,
    Mergeable val,
    Mergeable ty,
    GenIntermediate sem ty val,
    MonadAngelicContext ctx
  ) =>
  sem ->
  [ProgRes symVarId ty] ->
  StateT (CollectedDefUse symVarId val) ctx [val]
genProgResVals sem resList = do
  resVals <- lift $ genIntermediates sem (progResType <$> resList)
  addUses
    ( zipWith
        (IdValPair (con False))
        (progResId <$> resList)
        (mrgReturn . Just <$> resVals)
    )
  return resVals

inBound :: (SymbolicVarId symVarId) => Int -> symVarId -> SymBool
inBound bound val =
  symInBound (fromIntegral bound) val
    .&& symAny (.== val) (fromIntegral <$> [0 .. bound - 1])

symInBound :: (SymbolicVarId symVarId) => symVarId -> symVarId -> SymBool
symInBound bound val = (0 .<= val) .&& (val .< bound)

symAll :: (Foldable t) => (a -> SymBool) -> t a -> SymBool
symAll f = foldl' (\acc v -> acc .&& f v) (con True)

constrainStmt ::
  forall sem op ty val ctx p symVarId.
  ( SymbolicVarId symVarId,
    GenIntermediate sem ty val,
    OpSemantics sem op val ctx,
    OpTyping op ty ctx,
    Mergeable op,
    SEq val,
    MonadAngelicContext ctx
  ) =>
  p ty ->
  sem ->
  Int ->
  Stmt op symVarId ->
  StateT (CollectedDefUse symVarId val) ctx ()
constrainStmt
  p
  sem
  idBound
  (Stmt opUnion argIds argNum resIds resNum disabled mustBeAfters) = do
    symAssertWith "Out-of-bound statement results." $
      symAll (inBound idBound) resIds

    symAssertWith "result not canonical." $
      symAll (\(i, isucc) -> isucc .== i + 1) $
        zip resIds (tail resIds)

    signature <- lift $ typeOp opUnion
    Intermediates argVals resVals <- lift $ genOpIntermediates p sem signature
    mrgIf disabled (return ()) $ do
      computedResVals <- lift $ applyOp sem opUnion argVals
      symAssertWith "Incorrect results." $ resVals .== computedResVals

    let getIdValPairs _ [] [] = mrgReturn []
        getIdValPairs disabled (i : is) [] =
          mrgFmap (IdValPair (con True) i (mrgReturn Nothing) :) $
            getIdValPairs disabled is []
        getIdValPairs _ [] _ =
          mrgThrowError $
            "The limit for is smaller than the actual number. "
              <> "Check your SymOpLimits."
        getIdValPairs disabled (i : is) (v : vs) =
          mrgFmap (IdValPair disabled i (mrgReturn . Just $ v) :) $
            getIdValPairs disabled is vs

    symAssertWith "Incorrect number of arguments." $
      argNum .== fromIntegral (length argVals)
    addUses =<< getIdValPairs disabled argIds argVals
    symAssertWith "Incorrect number of results." $
      resNum .== fromIntegral (length resVals)
    addDefs =<< getIdValPairs disabled resIds resVals

    mrgTraverse_
      ( \(res, mustBeAfter) ->
          symAssertWith "Failed must be after constraint." $ res .> mustBeAfter
      )
      [(res, mustBeAfter) | res <- resIds, mustBeAfter <- mustBeAfters]

    let usedArgIds = take (length argVals) argIds
    let usedResIds = take (length resVals) resIds

    symAssertWith "Variable is undefined." $
      symAll (\resId -> symAll (symInBound resId) usedArgIds) usedResIds

connected ::
  ( MonadUnion ctx,
    MonadContext ctx,
    SymbolicVarId symVarId,
    SEq val,
    Mergeable val
  ) =>
  StateT (CollectedDefUse symVarId val) ctx ()
connected = do
  CollectedDefUse def use <- get
  mrgSequence_ $
    [ do
        symAssertWith "Using disabled values" $
          symImplies (defDisabled .&& defId .== useId) useDisabled
        symAssertWith "Def/use with same ID does not have the same value." $
          useDisabled .|| symImplies (defId .== useId) (defVal .== useVal)
      | IdValPair defDisabled defId defVal <- def,
        IdValPair useDisabled useId useVal <- use
    ]

defDistinct ::
  ( MonadUnion ctx,
    MonadContext ctx,
    SymbolicVarId symVarId,
    SEq val,
    Mergeable val
  ) =>
  StateT (CollectedDefUse symVarId val) ctx ()
defDistinct = do
  CollectedDefUse def _ <- get
  let pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]
  mrgTraverse_ (symAssertWith "Variable is already defined." . uncurry (./=))
    . pairs
    . fmap (\(IdValPair _ defId _) -> defId)
    $ def

instance
  ( SymbolicVarId symVarId,
    GenIntermediate sem ty val,
    OpSemantics sem op val ctx,
    OpTyping op ty ctx,
    Mergeable op,
    SEq val,
    MonadAngelicContext ctx
  ) =>
  ProgSemantics sem (Prog op symVarId ty) val ctx
  where
  runProg sem (Prog _ arg stmts ret) inputs =
    flip mrgEvalStateT (CollectedDefUse [] []) $ do
      symAssertWith
        ( "Expected "
            <> showText (length arg)
            <> " arguments, but got "
            <> showText (length inputs)
            <> " arguments."
        )
        $ length inputs .== length arg
      addProgArgs inputs

      let bound = length inputs + sum (length . stmtResIds <$> stmts)
      mrgTraverse_ (constrainStmt (Proxy :: Proxy ty) sem bound) stmts
      resVals <- genProgResVals sem ret
      symAssertWith "Variable is undefined." $
        symAll (inBound bound) $
          progResId <$> ret
      connected
      defDistinct
      mrgReturn resVals

instance (Mergeable ty) => ProgTyping (Prog op varId ty) ty where
  typeProg prog =
    mrgReturn $
      TypeSignature
        (progArgType <$> progArgList prog)
        (progResType <$> progResList prog)

instance ProgNaming (Prog op varId ty) where
  nameProg = progName

instance StmtUtil (Stmt op varId) varId where
  type StmtOpType (Stmt op varId) = op
  getStmtArgIds = stmtArgIds
  getStmtResIds = stmtResIds
  getStmtOp = stmtOp
  getStmtDisabled = stmtDisabled

instance
  (SymbolicVarId varId) =>
  ProgUtil (Prog op varId ty) (Stmt op varId) varId
  where
  type ProgTypeType (Prog op varId ty) = ty
  getProgArgIds prog = fst <$> zip (fromIntegral <$> [0 ..]) (progArgList prog)
  getProgResIds = map progResId . progResList
  getProgNumStmts = length . progStmtList
  getProgStmtAtIdx prog idx
    | idx >= getProgNumStmts prog = throwError "Statement index out of bounds."
    | otherwise = return $ progStmtList prog !! idx

instance
  ( MonadContext ctx,
    MonadUnion ctx,
    OpCost opCostObj op cost ctx,
    Num cost,
    Mergeable cost
  ) =>
  ProgCost (PerStmtCostObj opCostObj) (Prog op varId ty) cost ctx
  where
  progCost (PerStmtCostObj obj) prog = do
    stmtCosts <-
      traverse
        ( \stmt ->
            mrgIf (stmtDisabled stmt) (return 0) $ opCost obj $ stmtOp stmt
        )
        (progStmtList prog)
    return $ sum stmtCosts
