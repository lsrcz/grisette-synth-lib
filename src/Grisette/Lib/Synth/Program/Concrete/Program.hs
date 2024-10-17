{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :    Grisette.Lib.Synth.Program.Concrete
-- Copyright   :    (c) Sirui Lu 2024
-- License     :    BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :    siruilu@cs.washington.edu
-- Stability   :    experimental
-- Portability :    GHC only
--
-- Do not use plain concrete programs on operators that may generate multiple
-- results that cannot be merged into a single result, or there can be
-- significant performance issues.
--
-- Use the `ProgMayMultiPath` wrapper to allow multiple results. However, note
-- that it is generally not a good idea to use non-simply-mergeable types as
-- results, as it can lead to exponential blowup in the number of results.
module Grisette.Lib.Synth.Program.Concrete.Program
  ( Stmt (..),
    ProgArg (..),
    ProgRes (..),
    Prog (..),
    ProgPPrintError (..),
    prettyStmt,
    prettyProg,
    stmtToDotNode,
    progToDotSubGraph,
    ProgPPrint (..),
    ProgToDot (..),
    eliminateDeadCode,
    eliminateProgTableDeadCode,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT,
    evalStateT,
  )
import Data.Bifunctor (Bifunctor (second))
import qualified Data.Binary as Binary
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Foldable (traverse_)
import Data.GraphViz
  ( DotEdge (DotEdge),
    DotNode (DotNode),
    DotStatements (DotStmts, attrStmts, edgeStmts, nodeStmts, subGraphs),
    DotSubGraph (DotSG, isCluster, subGraphID, subGraphStmts),
    GlobalAttributes (GraphAttrs),
    GraphID (Str),
    Shape (Record),
    shape,
  )
import Data.GraphViz.Attributes (textLabel)
import Data.GraphViz.Attributes.Complete
  ( Attribute (HeadPort, Label, TailPort),
    Label (RecordLabel),
    PortName (PN),
    PortPos (LabelledPort),
    RecordField (FieldLabel, FlipFields, LabelledTarget),
  )
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import qualified Data.Serialize as Cereal
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvalSym,
    Mergeable (rootStrategy),
    MergingStrategy (NoStrategy),
    PPrint (pformat),
    ToCon (toCon),
    ToSym (toSym),
    tryMerge,
  )
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Operator.OpReachableSymbols
  ( OpReachableSymbols (opReachableSymbols),
  )
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Operator.OpTyping (OpTyping (OpTypeType))
import Grisette.Lib.Synth.Program.Concrete.OpPPrint
  ( OpPPrint (pformatOp),
    OpPPrintError,
    VarIdMap,
    prettyArguments,
    prettyResults,
  )
import Grisette.Lib.Synth.Program.Concrete.OpToDot
  ( VarIdToLabel,
    argumentsToFieldEdges,
    resultsToFieldEdges,
  )
import Grisette.Lib.Synth.Program.CostModel.PerStmtCostModel
  ( OpCost (opCost),
    PerStmtCostObj (PerStmtCostObj),
  )
import Grisette.Lib.Synth.Program.ProgCost (ProgCost (progCost))
import Grisette.Lib.Synth.Program.ProgPPrint
  ( ProgPPrint (pformatProg),
  )
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Program.ProgToDot
  ( ProgToDot (toDotProg),
  )
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
import Grisette.Lib.Synth.Program.SymbolTable (ProgReachableSymbols (progReachableSymbols), SymbolTable (SymbolTable))
import Grisette.Lib.Synth.TypeSignature
  ( TypeSignature (TypeSignature),
  )
import Grisette.Lib.Synth.Util.Pretty
  ( Doc,
    concatWith,
    hardline,
    nest,
    parenCommaList,
    parenCommaListIfNotSingle,
    renderDoc,
    (<+>),
  )
import Grisette.Lib.Synth.Util.Show (showText)
import Grisette.Lib.Synth.VarId (ConcreteVarId)

data Stmt op varId = Stmt
  { stmtOp :: op,
    stmtArgIds :: [varId],
    stmtResIds :: [varId]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData, Serial)
  deriving (EvalSym) via (Default (Stmt op varId))

instance (Serial op, Serial varId) => Cereal.Serialize (Stmt op varId) where
  put = serialize
  get = deserialize

instance (Serial op, Serial varId) => Binary.Binary (Stmt op varId) where
  put = serialize
  get = deserialize

instance
  (ToCon symOp conOp) =>
  ToCon (Stmt symOp varId) (Stmt conOp varId)
  where
  toCon (Stmt op argIds resIds) =
    Stmt <$> toCon op <*> return argIds <*> return resIds

instance
  (ToSym conOp symOp) =>
  ToSym (Stmt conOp varId) (Stmt symOp varId)
  where
  toSym (Stmt op argIds resIds) = Stmt (toSym op) argIds resIds

instance Mergeable (Stmt op varId) where
  rootStrategy = NoStrategy

data ProgArg varId ty = ProgArg
  { progArgName :: T.Text,
    progArgId :: varId,
    progArgType :: ty
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData, Serial)
  deriving (EvalSym, Mergeable) via (Default (ProgArg varId ty))

instance (Serial varId, Serial ty) => Cereal.Serialize (ProgArg varId ty) where
  put = serialize
  get = deserialize

instance (Serial varId, Serial ty) => Binary.Binary (ProgArg varId ty) where
  put = serialize
  get = deserialize

instance
  (ToCon symTy conTy) =>
  ToCon (ProgArg varId symTy) (ProgArg varId conTy)
  where
  toCon (ProgArg name varId ty) = ProgArg name varId <$> toCon ty

instance
  (ToSym conTy symTy, Mergeable varId) =>
  ToSym (ProgArg varId conTy) (ProgArg varId symTy)
  where
  toSym (ProgArg name varId ty) = ProgArg name varId $ toSym ty

data ProgRes varId ty = ProgRes
  { progResId :: varId,
    progResType :: ty
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData, Serial)
  deriving (EvalSym, Mergeable) via (Default (ProgRes varId ty))

instance (Serial varId, Serial ty) => Cereal.Serialize (ProgRes varId ty) where
  put = serialize
  get = deserialize

instance (Serial varId, Serial ty) => Binary.Binary (ProgRes varId ty) where
  put = serialize
  get = deserialize

instance
  (ToCon symTy conTy) =>
  ToCon (ProgRes varId symTy) (ProgRes varId conTy)
  where
  toCon (ProgRes varId ty) = ProgRes varId <$> toCon ty

instance
  (ToSym conTy symTy, Mergeable varId) =>
  ToSym (ProgRes varId conTy) (ProgRes varId symTy)
  where
  toSym (ProgRes varId ty) = ProgRes varId $ toSym ty

data Prog op varId ty = Prog
  { progArgList :: [ProgArg varId ty],
    progStmtList :: [Stmt op varId],
    progResList :: [ProgRes varId ty]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData, Serial)
  deriving (EvalSym) via (Default (Prog op varId ty))

instance
  (Serial op, Serial varId, Serial ty) =>
  Cereal.Serialize (Prog op varId ty)
  where
  put = serialize
  get = deserialize

instance
  (Serial op, Serial varId, Serial ty) =>
  Binary.Binary (Prog op varId ty)
  where
  put = serialize
  get = deserialize

instance
  (ToCon symOp conOp, ToCon symTy conTy) =>
  ToCon (Prog symOp varId symTy) (Prog conOp varId conTy)
  where
  toCon (Prog arg stmt res) =
    Prog <$> toCon arg <*> traverse toCon stmt <*> toCon res

instance
  (ToSym conOp symOp, ToSym conTy symTy, Mergeable varId) =>
  ToSym (Prog conOp varId conTy) (Prog symOp varId symTy)
  where
  toSym (Prog arg stmt res) =
    Prog (toSym arg) (toSym stmt) (toSym res)

instance Mergeable (Prog op varId ty) where
  rootStrategy = NoStrategy

data ProgPPrintError varId op
  = StmtPPrintError (Stmt op varId) Int (OpPPrintError varId op)
  | ResultUndefined Int varId
  | ExtractSubProgError T.Text
  deriving (Show, Eq, Generic)
  deriving (Mergeable) via (Default (ProgPPrintError varId op))

instance
  (OpPPrint op, Show op, ConcreteVarId varId) =>
  PPrint (ProgPPrintError varId op)
  where
  pformat (StmtPPrintError stmt index err) =
    nest
      2
      ( "Error in statement "
          <> pformat index
          <> ": "
          <> hardline
          <> pformat err
          <> "."
      )
      <> hardline
      <> nest
        2
        ( "Raw statement: "
            <> hardline
            <> pformat (show stmt)
        )
  pformat (ResultUndefined index varId) =
    "Error in result "
      <> pformat index
      <> ": the variable "
      <> pformat (toInteger varId)
      <> " is undefined."
  pformat (ExtractSubProgError err) =
    "Error while extracting sub-program: " <> pformat err

prettyStmt ::
  ( ConcreteVarId varId,
    OpPPrint op
  ) =>
  Int ->
  Stmt op varId ->
  StateT (VarIdMap varId) (Either (ProgPPrintError varId op)) (Doc ann)
prettyStmt index stmt@(Stmt op argIds resIds) = do
  map <- get
  arpformat <- case prettyArguments op argIds map of
    Left err -> throwError $ StmtPPrintError stmt index err
    Right arpformat -> pure arpformat
  let opPretty = pformatOp op
  (newMap, resPretty) <- case prettyResults op resIds map of
    Left err -> throwError $ StmtPPrintError stmt index err
    Right resPretty -> pure resPretty
  put newMap
  return $ resPretty <> " = " <> opPretty <> arpformat

prettyProg ::
  ( ConcreteVarId varId,
    OpPPrint op,
    PPrint ty
  ) =>
  T.Text ->
  Prog op varId ty ->
  Either (ProgPPrintError varId op) (Doc ann)
prettyProg key (Prog argList stmtList resList) = do
  let initMap =
        HM.fromList $ map (\arg -> (progArgId arg, progArgName arg)) argList
  flip evalStateT initMap $ do
    stmtsPretty <- traverse (uncurry prettyStmt) (zip [0 ..] stmtList)
    let firstLine =
          nest (-2) $
            "def "
              <> pformat key
              <> parenCommaList
                ( map
                    ( \arg ->
                        pformat (progArgName arg)
                          <> ": "
                          <> pformat (progArgType arg)
                    )
                    argList
                )
              <> " -> "
              <> parenCommaListIfNotSingle (pformat . progResType <$> resList)
              <> ":"
    allMap <- get
    let lookupVarId (idx, varId) =
          maybe (throwError $ ResultUndefined idx varId) return $
            HM.lookup varId allMap
    retNames <- traverse lookupVarId (zip [0 ..] $ progResId <$> resList)
    let ret = "return" <+> parenCommaListIfNotSingle (pformat <$> retNames)
    return . nest 2 . concatWith (\x y -> x <> hardline <> y) $
      concat [[firstLine], stmtsPretty, [ret]]

instance
  ( OpPPrint op,
    ConcreteVarId varId,
    PPrint ty,
    Show op,
    Show ty
  ) =>
  ProgPPrint (Prog op varId ty)
  where
  pformatProg key prog = progDoc
    where
      progDoc = case prettyProg key prog of
        Left err ->
          Left $
            pformat err
              <> hardline
              <> nest 2 ("Raw program: " <> hardline <> pformat (show prog))
        Right doc -> Right doc

stmtToDotNode ::
  (ConcreteVarId varId, OpPPrint op) =>
  T.Text ->
  Int ->
  Stmt op varId ->
  StateT
    (VarIdToLabel varId)
    (Either (ProgPPrintError varId op))
    (DotNode T.Text, [DotEdge T.Text])
stmtToDotNode progName index stmt@(Stmt op argIds resIds) = do
  map <- get
  let nodeId = progName <> "_stmt" <> showText index
  (argFields, edges) <-
    case argumentsToFieldEdges nodeId op argIds map of
      Left err -> throwError $ StmtPPrintError stmt index err
      Right argFieldEdges -> pure argFieldEdges
  let opPretty = TL.fromStrict $ renderDoc 80 $ pformatOp op
  (newMap, resFields) <-
    case resultsToFieldEdges nodeId op resIds map of
      Left err -> throwError $ StmtPPrintError stmt index err
      Right resFields -> pure resFields
  put newMap
  return
    ( DotNode
        nodeId
        [ Label . RecordLabel $
            [ FlipFields
                [ FlipFields argFields,
                  FieldLabel opPretty,
                  FlipFields resFields
                ]
            ],
          shape Record
        ],
      edges
    )

progToDotSubGraph ::
  (ConcreteVarId varId, OpPPrint op, PPrint ty) =>
  T.Text ->
  Prog op varId ty ->
  Either (ProgPPrintError varId op) (DotSubGraph T.Text)
progToDotSubGraph key (Prog argList stmtList resList) = do
  let buildArgField arg =
        let argName = TL.fromStrict $ progArgName arg
            argType = TL.fromStrict $ renderDoc 80 (pformat $ progArgType arg)
         in LabelledTarget (PN argName) (argName <> ": " <> argType)
  let argNodeId = key <> "_args"
  let argNode =
        DotNode
          argNodeId
          [ Label . RecordLabel $
              [ FlipFields
                  [ FieldLabel "args",
                    FlipFields $ map buildArgField argList
                  ]
              ],
            shape Record
          ]
  let resPortAtPos pos = TL.fromStrict $ "res" <> showText pos
  let resLabel pos res =
        TL.fromStrict $
          "res"
            <> showText pos
            <> ": "
            <> renderDoc 80 (pformat (progResType res))
  let buildResField pos res =
        LabelledTarget (PN $ resPortAtPos pos) $ resLabel pos res
  let resNodeId = key <> "_res"
  let resNode =
        DotNode
          resNodeId
          [ Label . RecordLabel . return . FlipFields $
              [ FlipFields $ zipWith buildResField [0 ..] resList,
                FieldLabel "res"
              ],
            shape Record
          ]
  let initMap =
        HM.fromList $
          map
            ( \arg ->
                ( progArgId arg,
                  (argNodeId, PN $ TL.fromStrict $ progArgName arg)
                )
            )
            argList
  flip evalStateT initMap $ do
    stmtsPretty <-
      traverse (uncurry $ stmtToDotNode key) (zip [0 ..] stmtList)
    let nodes = fst <$> stmtsPretty
    let edges = concatMap snd stmtsPretty
    allMap <- get
    let lookupLabel map idx varId =
          maybe
            (throwError $ ResultUndefined idx varId)
            return
            (HM.lookup varId map)
    resPreLabels <-
      traverse (uncurry $ lookupLabel allMap) . zip [0 ..] $
        progResId <$> resList
    let preLabelToEdge (from, port) resPos =
          DotEdge
            from
            resNodeId
            [ HeadPort $ LabelledPort (PN $ resPortAtPos resPos) Nothing,
              TailPort $ LabelledPort port Nothing
            ]
    return $
      DotSG
        { isCluster = True,
          subGraphID = Just $ Str $ TL.fromStrict key,
          subGraphStmts =
            DotStmts
              { attrStmts = [GraphAttrs [textLabel $ TL.fromStrict key]],
                subGraphs = [],
                nodeStmts = [argNode] <> nodes <> [resNode],
                edgeStmts = edges <> zipWith preLabelToEdge resPreLabels [0 ..]
              }
        }

instance
  ( OpPPrint op,
    ConcreteVarId varId,
    PPrint ty,
    Show op,
    Show ty
  ) =>
  ProgToDot (Prog op varId ty)
  where
  toDotProg key prog =
    case progToDotSubGraph key prog of
      Left err ->
        let errTxt =
              renderDoc 80 $
                nest
                  2
                  ( "Error while pretty-printing program "
                      <> pformat key
                      <> hardline
                      <> pformat err
                  )
                  <> hardline
                  <> nest
                    2
                    ("Raw program: " <> hardline <> pformat (show prog))
         in DotSG
              { isCluster = True,
                subGraphID = Just $ Str $ TL.fromStrict key,
                subGraphStmts =
                  DotStmts
                    { attrStmts = [],
                      subGraphs = [],
                      nodeStmts = [DotNode errTxt []],
                      edgeStmts = []
                    }
              }
      Right graph -> graph

instance (ProgPPrint (Prog op varId ty)) => PPrint (Prog op varId ty) where
  pformat prog = case pformatProg "anonymous" prog of
    Left err ->
      nest
        2
        ( "Error while pretty-printing program "
            <> hardline
            <> err
        )
    Right doc -> doc

type Env varId val = HM.HashMap varId val

addVal ::
  (ConcreteVarId varId, MonadContext ctx) =>
  varId ->
  val ->
  StateT (Env varId val) ctx ()
addVal varId val = do
  env <- get
  when (HM.member varId env) . throwError $
    "Variable " <> showText varId <> " is already defined."
  put $ HM.insert varId val env

lookupVal ::
  (ConcreteVarId varId, MonadContext ctx) =>
  varId ->
  StateT (Env varId val) ctx val
lookupVal varId = do
  env <- get
  case HM.lookup varId env of
    Nothing -> throwError $ "Variable " <> showText varId <> " is undefined."
    Just val -> return val

instance
  {-# OVERLAPPABLE #-}
  ( OpSemantics semObj op val ctx,
    ConcreteVarId varId,
    Mergeable val,
    Mergeable ty,
    ty ~ OpTypeType op
  ) =>
  ProgSemantics semObj (Prog op varId ty) val ctx
  where
  runProg sem table (Prog arg stmts ret) inputs = tryMerge $ do
    when (length inputs /= length arg) . throwError $
      "Expected "
        <> showText (length arg)
        <> " arguments, but got "
        <> showText (length inputs)
        <> " arguments."
    let initialEnv = HM.fromList $ zip (progArgId <$> arg) inputs
    let runStmt (Stmt op argIds resIds) = do
          args <- traverse lookupVal argIds
          res <- lift $ applyOp sem table op args
          when (length res /= length resIds) . throwError $
            "Incorrect number of results."
          traverse_ (uncurry addVal) $ zip resIds res
    flip evalStateT initialEnv $ do
      traverse_ runStmt stmts
      traverse (lookupVal . progResId) ret

instance (Mergeable ty) => ProgTyping (Prog op varId ty) where
  typeProg prog =
    TypeSignature
      (progArgType <$> progArgList prog)
      (progResType <$> progResList prog)

-- instance ProgNaming (Prog op varId ty) where
--   nameProg = progName

instance StmtUtilImpl (Stmt op varId) op varId where
  getStmtArgIds = stmtArgIds
  getStmtResIds = stmtResIds
  getStmtOp = stmtOp
  getStmtDisabled _ = toSym False

instance StmtUtil (Stmt op varId) where
  type StmtVarIdType (Stmt op varId) = varId
  type StmtOpType (Stmt op varId) = op

instance ProgUtilImpl (Prog op varId ty) op (Stmt op varId) varId where
  getProgArgIds = map progArgId . progArgList
  getProgResIds = map progResId . progResList
  getProgNumStmts = length . progStmtList
  getProgStmtAtIdx prog idx
    | idx >= getProgNumStmts prog = throwError "Statement index out of bounds."
    | otherwise = return $ progStmtList prog !! idx

instance ProgUtil (Prog op varId ty) where
  type ProgTypeType (Prog op varId ty) = ty
  type ProgStmtType (Prog op varId ty) = Stmt op varId
  type ProgVarIdType (Prog op varId ty) = varId
  type ProgOpType (Prog op varId ty) = op

instance
  (MonadContext ctx, OpCost opCostObj op cost ctx, Num cost) =>
  ProgCost (PerStmtCostObj opCostObj) (Prog op varId ty) cost ctx
  where
  progCost (PerStmtCostObj obj) table (Prog _ stmts _) = do
    stmtCosts <- traverse (opCost obj table . stmtOp) stmts
    return $ sum stmtCosts

instance (OpReachableSymbols op) => ProgReachableSymbols (Prog op varId ty) where
  progReachableSymbols =
    mconcat . fmap (opReachableSymbols . stmtOp) . progStmtList

eliminateDeadCode ::
  (ConcreteVarId varId) => Prog op varId ty -> Prog op varId ty
eliminateDeadCode (Prog args stmts res) = do
  Prog args (reverse $ go (HS.fromList $ progResId <$> res) $ reverse stmts) res
  where
    go _ [] = []
    go reachable (stmt : rest) =
      if any (`HS.member` reachable) (stmtResIds stmt)
        then stmt : go (HS.union reachable (HS.fromList $ stmtArgIds stmt)) rest
        else go reachable rest

eliminateProgTableDeadCode ::
  (ConcreteVarId varId) =>
  SymbolTable (Prog op varId ty) ->
  SymbolTable (Prog op varId ty)
eliminateProgTableDeadCode (SymbolTable table) =
  SymbolTable $ map (second eliminateDeadCode) table
