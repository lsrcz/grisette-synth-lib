{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Program.ComponentSketch
  ( Stmt (..),
    ProgArg (..),
    ProgRes (..),
    Prog (..),
  )
where

import Control.Monad.State (MonadState (get), MonadTrans (lift), StateT)
import Data.Data (Proxy (Proxy))
import Data.Foldable (Foldable (foldl'))
import Data.List (tails)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvaluateSym,
    LogicalOp (symImplies, (.&&)),
    Mergeable (rootStrategy),
    MergingStrategy (NoStrategy),
    MonadUnion,
    SEq ((./=), (.==)),
    SOrd ((.<), (.<=)),
    Solvable (con),
    SymBool,
    mrgSequence_,
    mrgTraverse_,
    symAssertWith,
  )
import Grisette.Lib.Control.Monad.State.Class (mrgModify)
import Grisette.Lib.Control.Monad.Trans.State (mrgEvalStateT)
import Grisette.Lib.Synth.Context (MonadContext (result))
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Operator.OpTyping
  ( GenIntermediate,
    OpTyping,
    genIntermediates,
    genOpIntermediates,
  )
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Util.Show (showText)
import Grisette.Lib.Synth.VarId (SymbolicVarId)

data Stmt op symVarId = Stmt
  { stmtOp :: op,
    stmtArgIds :: [symVarId],
    stmtResIds :: [symVarId]
  }
  deriving (Show, Eq, Generic)

instance Mergeable (Stmt op symVarId) where
  rootStrategy = NoStrategy

data ProgArg ty = ProgArg
  { progArgType :: ty,
    progArgName :: T.Text
  }
  deriving (Show, Eq, Generic)

instance Mergeable (ProgArg ty) where
  rootStrategy = NoStrategy

data ProgRes symVarId ty = ProgRes
  { progResType :: ty,
    progResId :: symVarId
  }
  deriving (Show, Eq, Generic)

instance Mergeable (ProgRes symVarId ty) where
  rootStrategy = NoStrategy

data Prog op symVarId ty = Prog
  { progName :: T.Text,
    progArgList :: [ProgArg ty],
    progStmtList :: [Stmt op symVarId],
    progResList :: [ProgRes symVarId ty]
  }
  deriving (Show, Eq, Generic)

data IdValPair symVarId val = IdValPair symVarId val
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
  addDefs (zipWith IdValPair (fromIntegral <$> [0 ..]) args)

genProgResVals ::
  ( MonadUnion ctx,
    SymbolicVarId symVarId,
    Mergeable val,
    GenIntermediate sem ty val ctx
  ) =>
  sem ->
  [ProgRes symVarId ty] ->
  StateT (CollectedDefUse symVarId val) ctx [val]
genProgResVals sem resList = do
  resVals <- lift $ genIntermediates sem (progResType <$> resList)
  addUses (zipWith IdValPair (progResId <$> resList) resVals)
  return resVals

inBound :: (SymbolicVarId symVarId) => symVarId -> symVarId -> SymBool
inBound bound val = (0 .<= val) .&& (val .< bound)

symAll :: (Foldable t) => (a -> SymBool) -> t a -> SymBool
symAll f = foldl' (\acc v -> acc .&& f v) (con True)

constrainStmt ::
  forall sem op ty val ctx p symVarId.
  ( MonadContext ctx,
    MonadUnion ctx,
    SymbolicVarId symVarId,
    OpTyping sem op ty ctx,
    GenIntermediate sem ty val ctx,
    OpSemantics sem op val ctx,
    SEq val
  ) =>
  p ty ->
  sem ->
  Int ->
  Stmt op symVarId ->
  StateT (CollectedDefUse symVarId val) ctx ()
constrainStmt p sem idBound (Stmt op argIds resIds) = do
  symAssertWith "Variable is undefined." $
    symAll (\resId -> symAll (inBound resId) argIds) resIds
  symAssertWith "Out-of-bound statement results." $
    symAll (inBound (fromIntegral idBound)) resIds

  symAssertWith "Result not canonical." $
    symAll (\(i, isucc) -> isucc .== i + 1) $
      zip resIds (tail resIds)

  (argVals, resVals) <- lift $ genOpIntermediates p sem op (length argIds)
  addUses $ zipWith IdValPair argIds argVals
  symAssertWith "Incorrect number of results." $
    length resIds .== length resVals
  addDefs $ zipWith IdValPair resIds resVals

  computedResVals <- lift $ applyOp sem op argVals
  symAssertWith "Incorrect results." $ resVals .== computedResVals

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
    [ symAssertWith "Def/use with same ID does not have the same value." $
        (defId .== useId) `symImplies` (defVal .== useVal)
      | IdValPair defId defVal <- def,
        IdValPair useId useVal <- use
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
    . fmap (\(IdValPair varId _) -> varId)
    $ def

instance
  ( MonadContext ctx,
    MonadUnion ctx,
    Mergeable val,
    SymbolicVarId symVarId,
    OpTyping sem op ty ctx,
    GenIntermediate sem ty val ctx,
    OpSemantics sem op val ctx,
    SEq val,
    Show val
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
        symAll (inBound (fromIntegral bound)) $
          progResId <$> ret
      connected
      defDistinct
      result resVals
