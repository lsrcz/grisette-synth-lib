{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Program.Concrete.Flatten
  ( OpFlatten (..),
    flattenSymbolTable,
  )
where

import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT,
    evalStateT,
    modify,
  )
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Grisette.Lib.Synth.Context (ConcreteContext)
import Grisette.Lib.Synth.Program.Concrete.Program
  ( Prog (Prog),
    ProgArg (ProgArg, progArgId),
    ProgRes (ProgRes, progResId),
    Stmt (Stmt),
  )
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable (SymbolTable))
import Grisette.Lib.Synth.VarId (ConcreteVarId)

newtype FlattenedProgramMap op varId newVarId ty
  = FlattenedProgramMap
      [ ( T.Text,
          [newVarId] ->
          StateT
            (FlattenState varId newVarId)
            ConcreteContext
            ([newVarId], [Stmt op newVarId]),
          [ProgArg newVarId ty],
          [newVarId] -> [ProgRes newVarId ty]
        )
      ]

getFlattenedProgramInner ::
  (ConcreteVarId varId, ConcreteVarId newVarId) =>
  FlattenedProgramMap op varId newVarId ty ->
  T.Text ->
  [newVarId] ->
  StateT
    (FlattenState varId newVarId)
    ConcreteContext
    ([newVarId], [Stmt op newVarId])
getFlattenedProgramInner (FlattenedProgramMap table) symbol args = do
  r <- go table
  r args
  where
    go [] = throwError $ "getFlattenedProgram: " <> symbol <> " not found"
    go ((sym, ext, _, _) : xs) = if sym == symbol then return ext else go xs

getFlattenedProgram ::
  (ConcreteVarId varId, ConcreteVarId newVarId) =>
  FlattenedProgramMap op varId newVarId ty ->
  T.Text ->
  ConcreteContext (Prog op newVarId ty)
getFlattenedProgram t@(FlattenedProgramMap table) symbol = do
  progArgs <- args table
  let initialState =
        FlattenState
          { nextId = fromIntegral $ length progArgs,
            localIdMapping = HM.empty
          }
  (newRess, stmts) <-
    evalStateT (getFlattenedProgramInner t symbol $ progArgId <$> progArgs) initialState
  progRess <- ress table
  return $ Prog progArgs stmts (progRess newRess)
  where
    args [] = throwError $ "getFlattenedProgram: " <> symbol <> " not found"
    args ((sym, _, tableArg, _) : xs) =
      if sym == symbol then return tableArg else args xs
    ress [] = throwError $ "getFlattenedProgram: " <> symbol <> " not found"
    ress ((sym, _, _, tableRes) : xs) =
      if sym == symbol then return tableRes else ress xs

data FlattenState varId newVarId = FlattenState
  { nextId :: newVarId,
    localIdMapping :: HM.HashMap varId newVarId
  }
  deriving (Show)

class OpFlatten op flatOp where
  opForwardedSubProg :: op -> ConcreteContext (Either T.Text flatOp)

getExistingId ::
  (ConcreteVarId varId, ConcreteVarId newVarId) =>
  varId ->
  StateT
    (FlattenState varId newVarId)
    ConcreteContext
    newVarId
getExistingId varId = do
  state <- get
  case HM.lookup varId (localIdMapping state) of
    Just newId -> return newId
    Nothing ->
      throwError $
        "getExistingId: id not found in mapping, the program isn't "
          <> "well-formed: "
          <> T.pack (show varId)

remapNewId ::
  (ConcreteVarId varId, ConcreteVarId newVarId) =>
  varId ->
  StateT
    (FlattenState varId newVarId)
    ConcreteContext
    newVarId
remapNewId origId = do
  state <- get
  let newId = nextId state
  put $
    state
      { nextId = succ newId,
        localIdMapping = HM.insert origId newId (localIdMapping state)
      }
  return newId

remapProg ::
  (ConcreteVarId varId, ConcreteVarId newVarId, OpFlatten op flatOp) =>
  FlattenedProgramMap flatOp varId newVarId ty ->
  Prog op varId ty ->
  [newVarId] ->
  StateT
    (FlattenState varId newVarId)
    ConcreteContext
    ([newVarId], [Stmt flatOp newVarId])
remapProg flattened (Prog args stmts ress) newArgs = do
  state <- get
  put $ state {localIdMapping = HM.fromList $ zip (progArgId <$> args) newArgs}
  newStmts <- concat <$> traverse (remapUpToOneSubProgStmt flattened) stmts
  newRess <- traverse (getExistingId . progResId) ress
  modify $ \newState -> newState {localIdMapping = localIdMapping state}
  return (newRess, newStmts)

flattenSymbolTable ::
  (ConcreteVarId varId, ConcreteVarId newVarId, OpFlatten op flatOp) =>
  SymbolTable (Prog op varId ty) ->
  ConcreteContext (SymbolTable (Prog flatOp newVarId ty))
flattenSymbolTable t =
  let ft = flattenSymbolTable' t
   in do
        let keys = (\(k, _, _, _) -> k) <$> (\(FlattenedProgramMap x) -> x) ft
        progs <- traverse (getFlattenedProgram ft) keys
        return $ SymbolTable $ zip keys progs

flattenSymbolTable' ::
  (ConcreteVarId varId, ConcreteVarId newVarId, OpFlatten op flatOp) =>
  SymbolTable (Prog op varId ty) ->
  FlattenedProgramMap flatOp varId newVarId ty
flattenSymbolTable' (SymbolTable table) =
  let res =
        FlattenedProgramMap $
          fmap
            ( \(symbol, prog@(Prog args _ ress)) ->
                ( symbol,
                  \newArgs -> do
                    when (length args /= length newArgs) $
                      throwError $
                        "flattenSymbolTable: argument count mismatch. "
                          <> "Supplied "
                          <> T.pack (show $ length newArgs)
                          <> " arguments, but the program has "
                          <> T.pack (show $ length args)
                          <> " arguments."
                    remapProg res prog newArgs,
                  zipWith
                    (\(ProgArg name _ ty) i -> ProgArg name i ty)
                    args
                    [0 ..],
                  zipWith
                    (\(ProgRes _ ty) i -> ProgRes i ty)
                    ress
                )
            )
            table
   in res

remapUpToOneSubProgStmt ::
  (ConcreteVarId varId, ConcreteVarId newVarId, OpFlatten op flatOp) =>
  FlattenedProgramMap flatOp varId newVarId ty ->
  Stmt op varId ->
  StateT (FlattenState varId newVarId) ConcreteContext [Stmt flatOp newVarId]
remapUpToOneSubProgStmt flattened (Stmt op argIds resIds) = do
  r <- lift $ opForwardedSubProg op
  case r of
    Left subProgSymbol -> do
      newArgIds <- traverse getExistingId argIds
      (newResIds, newStmts) <-
        getFlattenedProgramInner flattened subProgSymbol newArgIds
      when
        (length resIds /= length newResIds)
        $ throwError
        $ "remapUpToOneSubProgStmt: forwarded sub-program result count mismatch. "
          <> "Supplied "
          <> T.pack (show resIds)
          <> " results, but the sub-program has "
          <> T.pack (show newResIds)
          <> " results."
      modify $ \st ->
        st
          { localIdMapping =
              HM.union (localIdMapping st) $
                HM.fromList $
                  zip resIds newResIds
          }
      return newStmts
    Right flatOp -> do
      newArgIds <- traverse getExistingId argIds
      newResIds <- traverse remapNewId resIds
      return [Stmt flatOp newArgIds newResIds]

{-

data X
  = X
  | XF T.Text
  deriving (Show)

instance PPrint X where
  pformat X = "X"
  pformat (XF v) = pformat v

instance PrefixByType XType where
  prefixByType _ = "r"

instance OpPPrint X

instance OpFlatten X where
  opForwardedSubProg X = return Nothing
  opForwardedSubProg (XF v) = return $ Just v

data XType = XType
  deriving (Show, Generic)
  deriving (Mergeable) via (Default XType)

instance PPrint XType where
  pformat XType = "XType"

instance (MonadContext ctx) => OpTyping X ctx where
  type OpTypeType X = XType
  typeOp _ X = return $ TypeSignature [XType, XType] [XType, XType]
  typeOp table (XF symbol) = liftEither $ lookupType table symbol

x :: SymbolTable (Prog X Int XType)
x =
  SymbolTable
    [ ( "a",
        Prog
          "a"
          [ProgArg "x" 0 XType, ProgArg "y" 1 XType]
          [ Stmt X [0, 1] [2, 3],
            Stmt X [1, 2] [4, 5]
          ]
          [ProgRes 4 XType, ProgRes 5 XType]
      ),
      ( "b",
        Prog
          "b"
          [ProgArg "x" 0 XType, ProgArg "y" 1 XType]
          [ Stmt (XF "a") [0, 1] [2, 3],
            Stmt X [1, 2] [4, 5]
          ]
          [ProgRes 4 XType, ProgRes 5 XType]
      ),
      ( "c",
        Prog
          "c"
          [ProgArg "x" 0 XType, ProgArg "y" 1 XType]
          [ Stmt (XF "b") [0, 1] [2, 3],
            Stmt (XF "a") [1, 2] [4, 5]
          ]
          [ProgRes 4 XType, ProgRes 5 XType]
      )
    ]

xf :: ConcreteContext (SymbolTable (Prog X Int XType))
xf = flattenSymbolTable x

-- xa :: ConcreteContext (Prog X Int XType)
-- xa = getFlattenedProgram xf "a"
--
-- xb :: ConcreteContext (Prog X Int XType)
-- xb = getFlattenedProgram xf "b"
--
-- xc :: ConcreteContext (Prog X Int XType)
-- xc = getFlattenedProgram xf "c"

newtype FlattenedProgramMap op varId newVarId ty
  = FlattenedProgramMap
      [ ( T.Text,
          [newVarId] ->
          StateT
            (FlattenState varId newVarId)
            ConcreteContext
            ([newVarId], [Stmt op newVarId]),
          [ProgArg newVarId ty],
          [newVarId] -> [ProgRes newVarId ty]
        )
      ]

getFlattenedProgramInner ::
  (ConcreteVarId varId, ConcreteVarId newVarId) =>
  FlattenedProgramMap op varId newVarId ty ->
  T.Text ->
  [newVarId] ->
  StateT
    (FlattenState varId newVarId)
    ConcreteContext
    ([newVarId], [Stmt op newVarId])
getFlattenedProgramInner (FlattenedProgramMap table) symbol args = do
  r <- go table
  r args
  where
    go [] = throwError $ "getFlattenedProgram: " <> symbol <> " not found"
    go ((sym, ext, _, _) : xs) = if sym == symbol then return ext else go xs

getFlattenedProgram ::
  (ConcreteVarId varId, ConcreteVarId newVarId) =>
  FlattenedProgramMap op varId newVarId ty ->
  T.Text ->
  ConcreteContext (Prog op newVarId ty)
getFlattenedProgram t@(FlattenedProgramMap table) symbol = do
  progArgs <- args table
  let initialState =
        FlattenState
          { nextId = fromIntegral $ length progArgs,
            localIdMapping = HM.empty
          }
  (newRess, stmts) <-
    evalStateT (getFlattenedProgramInner t symbol $ progArgId <$> progArgs) initialState
  progRess <- ress table
  return $ Prog symbol progArgs stmts (progRess newRess)
  where
    args [] = throwError $ "getFlattenedProgram: " <> symbol <> " not found"
    args ((sym, _, tableArg, _) : xs) =
      if sym == symbol then return tableArg else args xs
    ress [] = throwError $ "getFlattenedProgram: " <> symbol <> " not found"
    ress ((sym, _, _, tableRes) : xs) =
      if sym == symbol then return tableRes else ress xs

data FlattenState varId newVarId = FlattenState
  { nextId :: newVarId,
    localIdMapping :: HM.HashMap varId newVarId
  }
  deriving (Show)

class OpFlatten op where
  opForwardedSubProg :: op -> ConcreteContext (Maybe T.Text)

getExistingId ::
  (ConcreteVarId varId, ConcreteVarId newVarId) =>
  varId ->
  StateT
    (FlattenState varId newVarId)
    ConcreteContext
    newVarId
getExistingId varId = do
  state <- get
  case HM.lookup varId (localIdMapping state) of
    Just newId -> return newId
    Nothing ->
      throwError $
        "getExistingId: id not found in mapping, the program isn't "
          <> "well-formed: "
          <> T.pack (show varId)

remapNewId ::
  (ConcreteVarId varId, ConcreteVarId newVarId) =>
  varId ->
  StateT
    (FlattenState varId newVarId)
    ConcreteContext
    newVarId
remapNewId origId = do
  state <- get
  let newId = nextId state
  put $
    state
      { nextId = succ newId,
        localIdMapping = HM.insert origId newId (localIdMapping state)
      }
  return newId

remapSimpleStatement ::
  (ConcreteVarId varId, ConcreteVarId newVarId) =>
  Stmt op varId ->
  StateT
    (FlattenState varId newVarId)
    ConcreteContext
    [Stmt op newVarId]
remapSimpleStatement (Stmt op argIds resIds) = do
  newArgIds <- traverse getExistingId argIds
  newResIds <- traverse remapNewId resIds
  return [Stmt op newArgIds newResIds]

remapProg ::
  (ConcreteVarId varId, ConcreteVarId newVarId, OpFlatten op) =>
  FlattenedProgramMap op varId newVarId ty ->
  Prog op varId ty ->
  [newVarId] ->
  StateT
    (FlattenState varId newVarId)
    ConcreteContext
    ([newVarId], [Stmt op newVarId])
remapProg flattened (Prog _ args stmts ress) newArgs = do
  state <- get
  put $ state {localIdMapping = HM.fromList $ zip (progArgId <$> args) newArgs}
  newStmts <- concat <$> traverse (remapUpToOneSubProgStmt flattened) stmts
  newRess <- traverse (getExistingId . progResId) ress
  modify $ \newState -> newState {localIdMapping = localIdMapping state}
  return (newRess, newStmts)

flattenSymbolTable ::
  (ConcreteVarId varId, ConcreteVarId newVarId, OpFlatten op) =>
  SymbolTable (Prog op varId ty) ->
  ConcreteContext (SymbolTable (Prog op newVarId ty))
flattenSymbolTable t =
  let ft = flattenSymbolTable' t
   in do
        let keys = (\(k, _, _, _) -> k) <$> (\(FlattenedProgramMap x) -> x) ft
        progs <- traverse (getFlattenedProgram ft) keys
        return $ SymbolTable $ zip keys progs

flattenSymbolTable' ::
  (ConcreteVarId varId, ConcreteVarId newVarId, OpFlatten op) =>
  SymbolTable (Prog op varId ty) ->
  FlattenedProgramMap op varId newVarId ty
flattenSymbolTable' (SymbolTable table) =
  let res =
        FlattenedProgramMap $
          fmap
            ( \(symbol, prog@(Prog _ args _ ress)) ->
                ( symbol,
                  \newArgs -> do
                    when (length args /= length newArgs) $
                      throwError $
                        "flattenSymbolTable: argument count mismatch. "
                          <> "Supplied "
                          <> T.pack (show $ length newArgs)
                          <> " arguments, but the program has "
                          <> T.pack (show $ length args)
                          <> " arguments."
                    remapProg res prog newArgs,
                  zipWith
                    (\(ProgArg name _ ty) i -> ProgArg name i ty)
                    args
                    [0 ..],
                  zipWith
                    (\(ProgRes _ ty) i -> ProgRes i ty)
                    ress
                )
            )
            table
   in res

remapUpToOneSubProgStmt ::
  (ConcreteVarId varId, ConcreteVarId newVarId, OpFlatten op) =>
  FlattenedProgramMap op varId newVarId ty ->
  Stmt op varId ->
  StateT (FlattenState varId newVarId) ConcreteContext [Stmt op newVarId]
remapUpToOneSubProgStmt flattened stmt@(Stmt op argIds resIds) = do
  r <- lift $ opForwardedSubProg op
  case r of
    Just subProgSymbol -> do
      newArgIds <- traverse getExistingId argIds
      (newResIds, newStmts) <-
        getFlattenedProgramInner flattened subProgSymbol newArgIds
      when
        (length resIds /= length newResIds)
        $ throwError
        $ "remapUpToOneSubProgStmt: forwarded sub-program result count mismatch. "
          <> "Supplied "
          <> T.pack (show resIds)
          <> " results, but the sub-program has "
          <> T.pack (show newResIds)
          <> " results."
      modify $ \st ->
        st
          { localIdMapping =
              HM.union (localIdMapping st) $
                HM.fromList $
                  zip resIds newResIds
          }
      return newStmts
    Nothing -> remapSimpleStatement stmt

-}
