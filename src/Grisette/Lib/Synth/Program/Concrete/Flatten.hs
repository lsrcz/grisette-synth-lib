{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Program.Concrete.Flatten
  ( OpFlatten (..),
    flattenProg,
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
  ( Prog (Prog, progArgList, progResList, progStmtList),
    ProgArg (ProgArg, progArgId),
    ProgRes (ProgRes, progResId),
    Stmt (Stmt),
  )
import Grisette.Lib.Synth.Program.SubProg (HasSubProgs (getSubProgs))
import Grisette.Lib.Synth.VarId (ConcreteVarId)

class (ConcreteVarId varId) => OpFlatten op varId ty | op -> ty where
  opForwardedSubProg :: op -> ConcreteContext (Maybe (Prog op varId ty))
  default opForwardedSubProg ::
    (HasSubProgs op (Prog op varId ty) ConcreteContext) =>
    op ->
    ConcreteContext (Maybe (Prog op varId ty))
  opForwardedSubProg op = do
    subProgs <- getSubProgs op
    case subProgs of
      [] -> return Nothing
      [prog] -> return $ Just prog
      _ ->
        throwError $
          "opForwardedSubProg: multiple sub-programs, we don't know how to "
            <> "forward as a single concrete program. Please manually "
            <> "implement the opForwardedSubProg function."

data FlattenState varId newVarId = FlattenState
  { nextId :: newVarId,
    idMapping :: HM.HashMap varId newVarId
  }
  deriving (Show)

remapNewId ::
  (ConcreteVarId varId, ConcreteVarId newVarId) =>
  varId ->
  StateT (FlattenState varId newVarId) ConcreteContext newVarId
remapNewId origId = do
  state <- get
  let newId = nextId state
  put $
    state
      { nextId = succ newId,
        idMapping = HM.insert origId newId (idMapping state)
      }
  return newId

remapExistingId ::
  (ConcreteVarId varId, ConcreteVarId newVarId) =>
  varId ->
  StateT (FlattenState varId newVarId) ConcreteContext newVarId
remapExistingId varId = do
  state <- get
  case HM.lookup varId (idMapping state) of
    Just newId -> return newId
    Nothing ->
      throwError $
        "remapExistingId: id not found in mapping, the program isn't "
          <> "well-formed: "
          <> T.pack (show varId)

remapProgArgs ::
  (ConcreteVarId varId, ConcreteVarId newVarId) =>
  [ProgArg varId ty] ->
  StateT (FlattenState varId newVarId) ConcreteContext [ProgArg newVarId ty]
remapProgArgs = do
  traverse
    ( \(ProgArg name argId ty) -> do
        newId <- remapExistingId argId
        return $ ProgArg name newId ty
    )

flattenStmt ::
  (OpFlatten op varId ty, ConcreteVarId newVarId) =>
  Stmt op varId ->
  StateT (FlattenState varId newVarId) ConcreteContext [Stmt op newVarId]
flattenStmt (Stmt op argIds resIds) = do
  maybeProg :: Maybe (Prog op varId ty) <- lift $ opForwardedSubProg op
  case maybeProg of
    Nothing -> do
      newArgIds <- traverse remapExistingId argIds
      newResIds <- traverse remapNewId resIds
      return [Stmt op newArgIds newResIds]
    Just prog -> do
      state <- get
      subProgArgIds <- traverse remapExistingId argIds
      when (length argIds /= length subProgArgIds) $
        throwError $
          "flattenStmt: forwarded sub-program argument count mismatch. "
            <> "Supplied "
            <> T.pack (show argIds)
            <> " arguments, but the sub-program has "
            <> T.pack (show subProgArgIds)
            <> " arguments."
      let newState =
            state
              { idMapping =
                  HM.fromList $
                    zip (progArgId <$> progArgList prog) subProgArgIds
              }
      put newState
      flattenedSubProg <- flattenSubProg prog
      let subProgRemappedResIds = progResId <$> progResList flattenedSubProg
      when
        (length resIds /= length subProgRemappedResIds)
        $ throwError
        $ "flattenStmt: forwarded sub-program result count mismatch. "
          <> "Supplied "
          <> T.pack (show resIds)
          <> " results, but the sub-program has "
          <> T.pack (show subProgRemappedResIds)
          <> " results."
      when (any (\i -> HM.member i (idMapping state)) resIds) $
        throwError $
          "flattenStmt: result id already exists in the mapping. The original "
            <> "program isn't well-formed"
      modify $ \st ->
        st
          { idMapping =
              HM.union
                (idMapping state)
                ( HM.fromList $
                    zip resIds (progResId <$> progResList flattenedSubProg)
                )
          }
      return $ progStmtList flattenedSubProg

remapProgRess ::
  (ConcreteVarId varId, ConcreteVarId newVarId) =>
  [ProgRes varId ty] ->
  StateT (FlattenState varId newVarId) ConcreteContext [ProgRes newVarId ty]
remapProgRess = do
  traverse
    ( \(ProgRes argId ty) -> do
        newId <- remapExistingId argId
        return $ ProgRes newId ty
    )

flattenSubProg ::
  (OpFlatten op varId ty, ConcreteVarId newVarId) =>
  Prog op varId ty ->
  StateT (FlattenState varId newVarId) ConcreteContext (Prog op newVarId ty)
flattenSubProg (Prog name args stmts ress) = do
  newArgs <- remapProgArgs args
  newStmts <- concat <$> traverse flattenStmt stmts
  newRess <- remapProgRess ress
  return $ Prog name newArgs newStmts newRess

flattenProg ::
  (OpFlatten op varId ty, ConcreteVarId newVarId) =>
  Prog op varId ty ->
  ConcreteContext (Prog op newVarId ty)
flattenProg prog = do
  let initialState =
        FlattenState
          { nextId = fromIntegral $ length $ progArgList prog,
            idMapping =
              HM.fromList $
                zip (progArgId <$> progArgList prog) $
                  fromIntegral <$> [0 ..]
          }
  evalStateT (flattenSubProg prog) initialState
