{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.Concrete.ProgramMayMultiPath
  ( ProgMayMultiPath (..),
  )
where

import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State
  ( MonadState (get),
    MonadTrans (lift),
    StateT,
    evalStateT,
  )
import qualified Data.HashMap.Lazy as HM
import Grisette
  ( Mergeable (rootStrategy),
    MergingStrategy (SimpleStrategy, SortedStrategy),
    MonadUnion,
    SimpleMergeable (mrgIte),
    UnionM,
    liftToMonadUnion,
    merge,
  )
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Control.Monad.State.Class (mrgPut)
import Grisette.Lib.Data.Foldable (mrgTraverse_)
import Grisette.Lib.Data.Traversable (mrgTraverse)
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Program.Concrete.Program (Prog (Prog), ProgArg (progArgId), ProgRes (progResId), Stmt (Stmt))
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Util.Show (showText)
import Grisette.Lib.Synth.VarId (ConcreteVarId)

newtype ProgMayMultiPath op varId ty = ProgMayMultiPath (Prog op varId ty)

newtype MayMultiPathEnv varId val
  = MayMultiPathEnv (HM.HashMap varId (UnionM val))

instance
  (ConcreteVarId conVarId, Mergeable val) =>
  Mergeable (MayMultiPathEnv conVarId val)
  where
  rootStrategy =
    SortedStrategy
      (\(MayMultiPathEnv m) -> HM.keysSet m)
      ( const $ SimpleStrategy $ \c (MayMultiPathEnv l) (MayMultiPathEnv r) ->
          MayMultiPathEnv $
            HM.mapWithKey
              ( \k v -> mrgIte c v (r HM.! k)
              )
              l
      )

addValMayMultiPath ::
  (ConcreteVarId varId, MonadContext ctx, MonadUnion ctx, Mergeable val) =>
  varId ->
  val ->
  StateT (MayMultiPathEnv varId val) ctx ()
addValMayMultiPath varId val = do
  MayMultiPathEnv env <- get
  when (HM.member varId env) . mrgThrowError $
    "Variable " <> showText varId <> " is already defined."
  mrgPut $ MayMultiPathEnv $ HM.insert varId (mrgReturn val) env

lookupValMayMultiPath ::
  (ConcreteVarId varId, MonadContext ctx, MonadUnion ctx, Mergeable val) =>
  varId ->
  StateT (MayMultiPathEnv varId val) ctx val
lookupValMayMultiPath varId = do
  MayMultiPathEnv env <- get
  case HM.lookup varId env of
    Nothing -> mrgThrowError $ "Variable " <> showText varId <> " is undefined."
    Just val -> liftToMonadUnion val

instance
  ( OpSemantics semObj op val ctx,
    ConcreteVarId varId,
    MonadUnion ctx,
    Mergeable val
  ) =>
  ProgSemantics semObj (ProgMayMultiPath op varId ty) val ctx
  where
  runProg sem (ProgMayMultiPath (Prog _ arg stmts ret)) inputs = merge $ do
    when (length inputs /= length arg) . mrgThrowError $
      "Expected "
        <> showText (length arg)
        <> " arguments, but got "
        <> showText (length inputs)
        <> " arguments."
    let initialEnv =
          MayMultiPathEnv . HM.fromList . zip (progArgId <$> arg) $
            mrgReturn <$> inputs
    let runStmt (Stmt op argIds resIds) = do
          args <- mrgTraverse lookupValMayMultiPath argIds
          res <- lift $ applyOp sem op args
          when (length res /= length resIds) . throwError $
            "Incorrect number of results."
          mrgTraverse_ (uncurry addValMayMultiPath) $ zip resIds res
    flip evalStateT initialEnv $ do
      mrgTraverse_ runStmt stmts
      mrgTraverse (lookupValMayMultiPath . progResId) ret
