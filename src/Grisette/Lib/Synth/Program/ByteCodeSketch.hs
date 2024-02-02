{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Grisette.Lib.Synth.Program.ByteCodeSketch
  ( Stmt (..),
    ProgArg (..),
    ProgRes (..),
    Prog (..),
  )
where

import Control.Monad (when)
import Control.Monad.State (MonadState (get), MonadTrans (lift), StateT)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvaluateSym,
    Mergeable,
    MergingStrategy (NoStrategy, SimpleStrategy, SortedStrategy),
    MonadUnion,
    SEq ((.==)),
    SOrd ((.>=)),
    SimpleMergeable (mrgIte),
    ToCon (toCon),
    ToSym (toSym),
    UnionLike,
    UnionM,
    liftToMonadUnion,
    mrgIf,
    symAssertWith,
  )
import Grisette.Core (Mergeable (rootStrategy))
import Grisette.Lib.Control.Monad.Trans.State (mrgEvalStateT, mrgPut)
import Grisette.Lib.Synth.Context
  ( MonadContext (raiseError, result),
    traverseC,
    traverseC_,
  )
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Operator.OpTyping (OpTyping)
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.ProgNaming (ProgNaming (nameProg))
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Program.ProgTyping (ProgTyping (typeProg))
import Grisette.Lib.Synth.Util.Show (showText)
import Grisette.Lib.Synth.VarId (ConcreteVarId, RelatedVarId, SymbolicVarId)

data Stmt op conVarId symVarId = Stmt
  { stmtOp :: UnionM op,
    stmtArgIds :: [symVarId],
    stmtArgNum :: symVarId,
    stmtResIds :: [conVarId],
    stmtResNum :: symVarId
  }
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym) via (Default (Stmt op conVarId symVarId))

instance
  (ToCon conOp symOp, RelatedVarId conVarId symVarId) =>
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
  { progArgType :: ty,
    progArgName :: T.Text,
    progArgId :: conVarId
  }
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym) via (Default (ProgArg conVarId ty))

instance ToCon (ProgArg conVarId ty) (Concrete.ProgArg conVarId ty) where
  toCon (ProgArg ty name varId) = Just $ Concrete.ProgArg ty name varId

instance Mergeable (ProgArg conVarId ty) where
  rootStrategy = NoStrategy

data ProgRes symVarId ty = ProgRes
  { progResType :: ty,
    progResId :: symVarId
  }
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym) via (Default (ProgRes symVarId ty))

instance
  (RelatedVarId conVarId symVarId) =>
  ToCon (ProgRes symVarId ty) (Concrete.ProgRes conVarId ty)
  where
  toCon (ProgRes ty varId) = Concrete.ProgRes ty <$> toCon varId

instance Mergeable (ProgRes symVarId ty) where
  rootStrategy = NoStrategy

data Prog op conVarId symVarId ty = Prog
  { progName :: T.Text,
    progArgList :: [ProgArg conVarId ty],
    progStmtList :: [Stmt op conVarId symVarId],
    progResList :: [ProgRes symVarId ty]
  }
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym) via (Default (Prog op conVarId symVarId ty))

deriving via
  (Default (Concrete.Prog conOp conVarId ty))
  instance
    (ToCon symOp conOp, RelatedVarId conVarId symVarId) =>
    ToCon
      (Prog symOp conVarId symVarId ty)
      (Concrete.Prog conOp conVarId ty)

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
  when (HM.member varId env) . raiseError $
    "Variable " <> showText varId <> " is already defined."
  mrgPut $ Env $ HM.insert varId val env

lookupVal ::
  ( RelatedVarId conVarId symVarId,
    MonadContext ctx,
    UnionLike ctx,
    SimpleMergeable val
  ) =>
  symVarId ->
  StateT (Env conVarId val) ctx val
lookupVal varId = do
  Env env <- get
  HM.foldlWithKey
    (\r conVarId val -> mrgIf (varId .== toSym conVarId) (result val) r)
    (raiseError "Variable is undefined.")
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
    go [] = raiseError "The specified argument number is too large."
    go ((i, list) : rest) =
      mrgIf (n .== i) (result list) (go rest)

instance
  ( OpSemantics semObj op val ctx,
    RelatedVarId conVarId symVarId,
    UnionLike ctx,
    SimpleMergeable val,
    Mergeable op
  ) =>
  ProgSemantics semObj (Prog op conVarId symVarId ty) val ctx
  where
  runProg sem (Prog _ arg stmts ret) inputs = do
    when (length inputs /= length arg) . raiseError $
      "Expected "
        <> showText (length arg)
        <> " arguments, but got "
        <> showText (length inputs)
        <> " arguments."
    let initialEnv = Env $ HM.fromList $ zip (progArgId <$> arg) inputs
    let runStmt (Stmt op argIds argNum resIds resNum) = do
          args <- traverseC lookupVal argIds
          res <- lift $ do
            singleOp <- liftToMonadUnion op
            keptArgs <- takeNumArg argNum args
            applyOp sem singleOp keptArgs
          symAssertWith "Incorrect number of results." $
            resNum .== fromIntegral (length res)
          symAssertWith "Insufficient result IDs." $
            length resIds .>= length res
          traverseC_ (uncurry addVal) $ zip resIds res
    flip mrgEvalStateT initialEnv $ do
      traverseC_ runStmt stmts
      traverseC (lookupVal . progResId) ret

instance
  ( OpTyping semObj op ty ctx,
    Mergeable ty
  ) =>
  ProgTyping semObj (Prog op conVarId symVarId ty) ty ctx
  where
  typeProg _ prog =
    result (progArgType <$> progArgList prog, progResType <$> progResList prog)

instance ProgNaming (Prog op conVarId symVarId ty) where
  nameProg = progName
