{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Program.ComponentSketch
  ( Stmt (..),
    ProgArg (..),
    ProgRes (..),
    Prog (..),
    MkStmt (..),
    MkFreshStmt (..),
    MkProg (..),
    MkFreshProg (..),
  )
where

import Control.Monad (join)
import Control.Monad.State (MonadState (get), MonadTrans (lift), StateT)
import Data.Data (Proxy (Proxy))
import Data.Foldable (Foldable (foldl'))
import Data.List (sortOn, tails)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvaluateSym,
    Fresh,
    GenSymSimple (simpleFresh),
    LogicalOp (symImplies, (.&&), (.||)),
    Mergeable (rootStrategy),
    MergingStrategy (NoStrategy),
    MonadUnion,
    SEq ((./=), (.==)),
    SOrd ((.<), (.<=)),
    SimpleListSpec (SimpleListSpec),
    Solvable (con),
    SymBool,
    ToCon (toCon),
    mrgIf,
    mrgSequence_,
    mrgTraverse_,
    symAssertWith,
  )
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.State.Class (mrgModify)
import Grisette.Lib.Control.Monad.Trans.State (mrgEvalStateT)
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Operator.OpTyping
  ( GenIntermediate,
    OpTyping,
    genIntermediates,
    genOpIntermediates,
  )
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.ProgNaming (ProgNaming (nameProg))
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Program.ProgTyping (ProgTyping (typeProg))
import Grisette.Lib.Synth.Util.Show (showText)
import Grisette.Lib.Synth.VarId (RelatedVarId, SymbolicVarId)

data Stmt op symVarId = Stmt
  { stmtOp :: op,
    stmtArgIds :: [symVarId],
    stmtResIds :: [symVarId],
    stmtDisabled :: SymBool
  }
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym) via (Default (Stmt op symVarId))

instance Mergeable (Stmt op symVarId) where
  rootStrategy = NoStrategy

data ProgArg ty = ProgArg
  { progArgType :: ty,
    progArgName :: T.Text
  }
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym) via (Default (ProgArg ty))

instance Mergeable (ProgArg ty) where
  rootStrategy = NoStrategy

data ProgRes symVarId ty = ProgRes
  { progResType :: ty,
    progResId :: symVarId
  }
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym) via (Default (ProgRes symVarId ty))

instance Mergeable (ProgRes symVarId ty) where
  rootStrategy = NoStrategy

data Prog op symVarId ty = Prog
  { progName :: T.Text,
    progArgList :: [ProgArg ty],
    progStmtList :: [Stmt op symVarId],
    progResList :: [ProgRes symVarId ty]
  }
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym) via (Default (Prog op symVarId ty))

instance
  (ToCon conOp symOp, RelatedVarId conVarId symVarId) =>
  ToCon (Prog conOp symVarId ty) (Concrete.Prog symOp conVarId ty)
  where
  toCon (Prog name argList stmtList resList) = do
    let conArgList =
          zipWith
            (\(ProgArg ty name) -> Concrete.ProgArg ty name)
            argList
            [0 ..]
    let toConStmt (Stmt op argIds resIds disabled) = do
          disabled <- toCon disabled
          if disabled
            then return []
            else do
              conOp <- toCon op
              conArgIds <- toCon argIds
              conResIds <- toCon resIds
              return [Concrete.Stmt conOp conArgIds conResIds]
    conStmts <- join <$> traverse toConStmt stmtList
    conResList <-
      traverse
        (\(ProgRes ty resId) -> Concrete.ProgRes ty <$> toCon resId)
        resList
    return $
      Concrete.Prog
        name
        conArgList
        (sortOn (listToMaybe . Concrete.stmtResIds) conStmts)
        conResList

data IdValPair symVarId val = IdValPair SymBool symVarId val
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
  addDefs (zipWith (IdValPair (con False)) (fromIntegral <$> [0 ..]) args)

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
  addUses (zipWith (IdValPair (con False)) (progResId <$> resList) resVals)
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
constrainStmt p sem idBound (Stmt op argIds resIds disabled) = do
  symAssertWith "Variable is undefined." $
    symAll (\resId -> symAll (inBound resId) argIds) resIds
  symAssertWith "Out-of-bound statement results." $
    symAll (inBound (fromIntegral idBound)) resIds

  symAssertWith "result not canonical." $
    symAll (\(i, isucc) -> isucc .== i + 1) $
      zip resIds (tail resIds)

  (argVals, resVals) <- lift $ genOpIntermediates p sem op (length argIds)
  addUses $ zipWith (IdValPair disabled) argIds argVals
  symAssertWith "Incorrect number of results." $
    length resIds .== length resVals
  addDefs $ zipWith (IdValPair disabled) resIds resVals

  mrgIf disabled (return ()) $ do
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
      mrgReturn resVals

instance
  ( OpTyping semObj op ty ctx,
    Mergeable ty
  ) =>
  ProgTyping semObj (Prog op varId ty) ty ctx
  where
  typeProg _ prog =
    mrgReturn
      (progArgType <$> progArgList prog, progResType <$> progResList prog)

instance ProgNaming (Prog op varId ty) where
  nameProg = progName

class MkStmt stmt op symVarId bool | stmt -> op symVarId bool where
  mkStmt :: op -> [symVarId] -> [symVarId] -> bool -> stmt

class MkFreshStmt stmt op | stmt -> op where
  mkFreshStmt :: op -> Int -> Int -> Fresh stmt

instance MkStmt (Stmt op symVarId) op symVarId SymBool where
  mkStmt = Stmt

instance
  MkStmt
    (Fresh (Stmt op symVarId))
    op
    (Fresh symVarId)
    (Fresh SymBool)
  where
  mkStmt op freshArgIds freshResIds freshDisabled =
    Stmt op <$> sequence freshArgIds <*> sequence freshResIds <*> freshDisabled

instance
  (GenSymSimple () symVarId) =>
  MkFreshStmt (Stmt op symVarId) op
  where
  mkFreshStmt op argCount resCount = do
    freshArgIds <- simpleFresh (SimpleListSpec argCount ())
    freshResIds <- simpleFresh (SimpleListSpec resCount ())
    mkStmt op freshArgIds freshResIds <$> simpleFresh ()

class MkProg prog stmt op symVarId ty | prog -> stmt op symVarId ty where
  mkProg ::
    T.Text ->
    [(ty, T.Text)] ->
    [stmt] ->
    [(ty, symVarId)] ->
    prog

instance MkProg (Prog op symVarId ty) (Stmt op symVarId) op symVarId ty where
  mkProg name args stmts rets =
    Prog name (uncurry ProgArg <$> args) stmts (uncurry ProgRes <$> rets)

instance
  MkProg
    (Fresh (Prog op symVarId ty))
    (Fresh (Stmt op symVarId))
    op
    (Fresh symVarId)
    ty
  where
  mkProg name args stmts rets =
    Prog name (uncurry ProgArg <$> args)
      <$> sequence stmts
      <*> traverse (\(ty, freshVarId) -> ProgRes ty <$> freshVarId) rets

class MkFreshProg prog stmt op ty | prog -> stmt op ty where
  mkFreshProg :: T.Text -> [ty] -> [Fresh stmt] -> [ty] -> Fresh prog

instance
  (GenSymSimple () symVarId) =>
  MkFreshProg (Prog op symVarId ty) (Stmt op symVarId) op ty
  where
  mkFreshProg name argTypes freshStmts retTypes =
    Prog
      name
      [ProgArg ty ("arg" <> showText n) | (n, ty) <- zip [0 ..] argTypes]
      <$> sequence freshStmts
      <*> sequence [ProgRes ty <$> simpleFresh () | ty <- retTypes]
