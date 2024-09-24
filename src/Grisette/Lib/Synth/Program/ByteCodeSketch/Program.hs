{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Lib.Synth.Program.ByteCodeSketch.Program
  ( Stmt (..),
    ProgArg (..),
    ProgRes (..),
    Prog (..),
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State (MonadState (get), MonadTrans (lift), StateT)
import qualified Data.Binary as Bytes
import Data.Bytes.Serial (Serial (deserialize, serialize))
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import qualified Data.Serialize as Cereal
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvalSym,
    Mergeable,
    MergingStrategy (NoStrategy, SimpleStrategy, SortedStrategy),
    MonadUnion,
    SimpleMergeable (mrgIte),
    SymEq ((.==)),
    SymOrd ((.>=)),
    ToCon (toCon),
    ToSym (toSym),
    mrgIf,
    symAssertWith,
  )
import Grisette.Core (Mergeable (rootStrategy))
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Control.Monad.Trans.State (mrgEvalStateT, mrgPut)
import Grisette.Lib.Data.Foldable (mrgTraverse_)
import Grisette.Lib.Data.Traversable (mrgTraverse)
import Grisette.Lib.Synth.Context (MonadContext)
-- import Grisette.Lib.Synth.Program.ProgNaming (ProgNaming (nameProg))

import Grisette.Lib.Synth.Operator.OpReachableSymbols (OpReachableSymbols (opReachableSymbols))
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Operator.OpTyping (OpTyping (OpTypeType))
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Program.ProgTyping (ProgTyping (typeProg))
import Grisette.Lib.Synth.Program.ProgUtil
  ( ProgUtil
      ( ProgOpType,
        ProgStmtType,
        ProgTypeType,
        ProgVarIdType
      ),
    ProgUtilImpl
      ( getProgArgIds,
        getProgNumStmts,
        getProgResIds,
        getProgStmtAtIdx
      ),
    StmtUtil
      ( StmtOpType,
        StmtVarIdType
      ),
    StmtUtilImpl
      ( getStmtArgIds,
        getStmtDisabled,
        getStmtOp,
        getStmtResIds
      ),
  )
import Grisette.Lib.Synth.Program.SymbolTable (ProgReachableSymbols (progReachableSymbols))
import Grisette.Lib.Synth.TypeSignature
  ( TypeSignature (TypeSignature),
  )
import Grisette.Lib.Synth.Util.Show (showText)
import Grisette.Lib.Synth.VarId (ConcreteVarId, RelatedVarId, SymbolicVarId)

data Stmt op conVarId symVarId = Stmt
  { stmtOp :: op,
    stmtArgIds :: [symVarId],
    stmtArgNum :: symVarId,
    stmtResIds :: [conVarId],
    stmtResNum :: symVarId
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData, Serial)
  deriving (EvalSym) via (Default (Stmt op conVarId symVarId))

instance
  (Serial op, Serial conVarId, Serial symVarId) =>
  Cereal.Serialize (Stmt op conVarId symVarId)
  where
  put = serialize
  get = deserialize

instance
  (Serial op, Serial conVarId, Serial symVarId) =>
  Bytes.Binary (Stmt op conVarId symVarId)
  where
  put = serialize
  get = deserialize

instance
  (ToCon conOp symOp, RelatedVarId conVarId symVarId, Mergeable conOp) =>
  ToCon (Stmt conOp conVarId symVarId) (Concrete.Stmt symOp conVarId)
  where
  toCon (Stmt op argIds argNum resIds resNum) = do
    conOp <- toCon op
    conArgNum <- fromIntegral <$> (toCon argNum :: Maybe conVarId)
    conArgIds <- toCon (take conArgNum argIds)
    conResNum <- fromIntegral <$> (toCon resNum :: Maybe conVarId)
    return $ Concrete.Stmt conOp conArgIds (take conResNum resIds)

instance Mergeable (Stmt op conVarId symVarId) where
  rootStrategy = NoStrategy

data ProgArg conVarId ty = ProgArg
  { progArgName :: T.Text,
    progArgId :: conVarId,
    progArgType :: ty
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData, Serial)
  deriving (EvalSym) via (Default (ProgArg conVarId ty))

instance
  (Serial op, Serial conVarId) =>
  Cereal.Serialize (ProgArg op conVarId)
  where
  put = serialize
  get = deserialize

instance
  (Serial op, Serial conVarId) =>
  Bytes.Binary (ProgArg op conVarId)
  where
  put = serialize
  get = deserialize

instance
  (ToCon symTy conTy) =>
  ToCon (ProgArg conVarId symTy) (Concrete.ProgArg conVarId conTy)
  where
  toCon (ProgArg name varId ty) = Concrete.ProgArg name varId <$> toCon ty

instance Mergeable (ProgArg conVarId ty) where
  rootStrategy = NoStrategy

data ProgRes symVarId ty = ProgRes
  { progResId :: symVarId,
    progResType :: ty
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData, Serial)
  deriving (EvalSym) via (Default (ProgRes symVarId ty))

instance
  (Serial op, Serial symVarId) =>
  Cereal.Serialize (ProgRes op symVarId)
  where
  put = serialize
  get = deserialize

instance
  (Serial op, Serial symVarId) =>
  Bytes.Binary (ProgRes op symVarId)
  where
  put = serialize
  get = deserialize

instance
  (RelatedVarId conVarId symVarId, ToCon symTy conTy) =>
  ToCon (ProgRes symVarId symTy) (Concrete.ProgRes conVarId conTy)
  where
  toCon (ProgRes varId ty) = do
    conProgResId <- toCon varId
    conTy <- toCon ty
    return $
      Concrete.ProgRes
        { Concrete.progResId = conProgResId,
          Concrete.progResType = conTy
        }

instance Mergeable (ProgRes symVarId ty) where
  rootStrategy = NoStrategy

data Prog op conVarId symVarId ty = Prog
  { progArgList :: [ProgArg conVarId ty],
    progStmtList :: [Stmt op conVarId symVarId],
    progResList :: [ProgRes symVarId ty]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData, Serial)
  deriving (EvalSym) via (Default (Prog op conVarId symVarId ty))

instance
  (Serial op, Serial conVarId, Serial symVarId, Serial ty) =>
  Cereal.Serialize (Prog op conVarId symVarId ty)
  where
  put = serialize
  get = deserialize

instance
  (Serial op, Serial conVarId, Serial symVarId, Serial ty) =>
  Bytes.Binary (Prog op conVarId symVarId ty)
  where
  put = serialize
  get = deserialize

deriving via
  (Default (Concrete.Prog conOp conVarId conTy))
  instance
    ( ToCon symOp conOp,
      RelatedVarId conVarId symVarId,
      Mergeable symOp,
      ToCon symTy conTy
    ) =>
    ToCon
      (Prog symOp conVarId symVarId symTy)
      (Concrete.Prog conOp conVarId conTy)

newtype Env conVarId val = Env (HM.HashMap conVarId val)

instance
  (ConcreteVarId conVarId, SimpleMergeable val) =>
  Mergeable (Env conVarId val)
  where
  rootStrategy =
    SortedStrategy
      (\(Env m) -> HM.keysSet m)
      ( const $ SimpleStrategy $ \c (Env l) (Env r) ->
          Env $
            HM.mapWithKey
              ( \k v -> mrgIte c v (r HM.! k)
              )
              l
      )

addVal ::
  ( ConcreteVarId conVarId,
    MonadContext ctx,
    MonadUnion ctx,
    SimpleMergeable val
  ) =>
  conVarId ->
  val ->
  StateT (Env conVarId val) ctx ()
addVal varId val = do
  Env env <- get
  when (HM.member varId env) . mrgThrowError $
    "Variable " <> showText varId <> " is already defined."
  mrgPut $ Env $ HM.insert varId val env

lookupVal ::
  ( RelatedVarId conVarId symVarId,
    MonadContext ctx,
    MonadUnion ctx,
    SimpleMergeable val
  ) =>
  symVarId ->
  StateT (Env conVarId val) ctx val
lookupVal varId = do
  Env env <- get
  HM.foldlWithKey
    (\r conVarId val -> mrgIf (varId .== toSym conVarId) (mrgReturn val) r)
    (mrgThrowError "Variable is undefined.")
    env

takeNumArg ::
  forall ctx symVarId a.
  ( MonadContext ctx,
    MonadUnion ctx,
    SymbolicVarId symVarId,
    SimpleMergeable a
  ) =>
  symVarId ->
  [a] ->
  ctx [a]
takeNumArg n l =
  go $ (\i -> (fromIntegral i, take i l)) <$> [0 .. length l]
  where
    go [] = mrgThrowError "The specified argument number is too large."
    go ((i, list) : rest) =
      mrgIf (n .== i) (mrgReturn list) (go rest)

instance
  {-# OVERLAPPABLE #-}
  ( OpSemantics semObj op val ctx,
    RelatedVarId conVarId symVarId,
    MonadUnion ctx,
    SimpleMergeable val,
    Mergeable op,
    Mergeable ty,
    ty ~ OpTypeType op
  ) =>
  ProgSemantics semObj (Prog op conVarId symVarId ty) val ctx
  where
  runProg sem table (Prog arg stmts ret) inputs = do
    when (length inputs /= length arg) . mrgThrowError $
      "Expected "
        <> showText (length arg)
        <> " arguments, but got "
        <> showText (length inputs)
        <> " arguments."
    let initialEnv = Env $ HM.fromList $ zip (progArgId <$> arg) inputs
    let runStmt (Stmt op argIds argNum resIds resNum) = do
          args <- mrgTraverse lookupVal argIds
          res <- lift $ do
            keptArgs <- takeNumArg argNum args
            applyOp sem table op keptArgs
          symAssertWith "Incorrect number of results." $
            resNum .== fromIntegral (length res)
          symAssertWith "Insufficient result IDs." $
            length resIds .>= length res
          mrgTraverse_ (uncurry addVal) $ zip resIds res
    flip mrgEvalStateT initialEnv $ do
      mrgTraverse_ runStmt stmts
      mrgTraverse (lookupVal . progResId) ret

instance
  (Mergeable ty, RelatedVarId conVarId symVarId) =>
  ProgTyping (Prog op conVarId symVarId ty)
  where
  typeProg prog =
    TypeSignature
      (progArgType <$> progArgList prog)
      (progResType <$> progResList prog)

-- instance ProgNaming (Prog op conVarId symVarId ty) where
--   nameProg = progName

instance
  (RelatedVarId conVarId symVarId) =>
  StmtUtilImpl (Stmt op conVarId symVarId) op symVarId
  where
  getStmtArgIds = stmtArgIds
  getStmtResIds = toSym . stmtResIds
  getStmtOp = stmtOp
  getStmtDisabled _ = toSym False

instance
  (RelatedVarId conVarId symVarId) =>
  ProgUtilImpl
    (Prog op conVarId symVarId ty)
    op
    (Stmt op conVarId symVarId)
    symVarId
  where
  getProgArgIds = toSym . map progArgId . progArgList
  getProgResIds = map progResId . progResList
  getProgNumStmts = length . progStmtList
  getProgStmtAtIdx prog idx
    | idx >= getProgNumStmts prog = throwError "Statement index out of bounds."
    | otherwise = return $ progStmtList prog !! idx

instance
  (RelatedVarId conVarId symVarId) =>
  ProgUtil (Prog op conVarId symVarId ty)
  where
  type ProgTypeType (Prog op conVarId symVarId ty) = ty
  type ProgStmtType (Prog op conVarId symVarId ty) = Stmt op conVarId symVarId
  type ProgVarIdType (Prog op conVarId symVarId ty) = symVarId
  type ProgOpType (Prog op conVarId symVarId ty) = op

instance
  (RelatedVarId conVarId symVarId) =>
  StmtUtil (Stmt op conVarId symVarId)
  where
  type StmtOpType (Stmt op conVarId symVarId) = op
  type StmtVarIdType (Stmt op conVarId symVarId) = symVarId

instance
  (OpReachableSymbols op) =>
  ProgReachableSymbols (Prog op conVarId symVarId ty)
  where
  progReachableSymbols =
    mconcat . fmap (opReachableSymbols . stmtOp) . progStmtList
