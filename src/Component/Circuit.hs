{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Component.Circuit where

import Component.Index
import Component.IntermediateGen
import Component.SemMap
import Control.Monad.Except
import Data.List
import GHC.Generics
import Grisette
import Grisette.Experimental

data Node op idx = Node
  { nodeOp :: op,
    nodeIdx :: idx,
    nodeInputIdx :: [idx]
  }
  deriving (Show, Eq, Generic)
  deriving (Mergeable, SEq, EvaluateSym) via (Default (Node op idx))

data Circuit op idx = Circuit
  { cirInputNum :: Int,
    cirNodes :: [Node op idx],
    cirOutputIdx :: [idx]
  }
  deriving (Show, Eq, Generic)
  deriving (Mergeable, SEq, EvaluateSym) via (Default (Circuit op idx))

data ComponentSpec op = ComponentSpec
  { compSpecOp :: op,
    compSpecInputNum :: Int
  }

data CircuitSpec op fm = CircuitSpec
  { cirSpecComponents :: [(ComponentSpec op, Int)],
    cirSpecInputNum :: Int,
    cirSpecOutputNum :: Int,
    cirSpecSem :: fm,
    cirDoSymmReduction :: Bool
  }

defaultCircuitSpec :: [(ComponentSpec op, Int)] -> Int -> Int -> fm -> CircuitSpec op fm
defaultCircuitSpec components inum onum sem = CircuitSpec components inum onum sem True

circuitAcyclic :: (MonadError e m, Mergeable e, UnionLike m, Index idx) => e -> Circuit op idx -> m ()
circuitAcyclic e c = go (cirNodes c)
  where
    go [] = mrgReturn ()
    go (Node _ vo vis : vs) = do
      go1 vo vis
      go vs
    go1 _ [] = mrgReturn ()
    go1 vo (vi : vis) = do
      symAssertWith e (vi <~ vo)
      go1 vo vis

circuitDistinctSlot :: (MonadError e m, Mergeable e, UnionLike m, Index idx) => e -> Circuit op idx -> m ()
circuitDistinctSlot e c = go (cirNodes c)
  where
    go [] = mrgReturn ()
    go (n : vs) = do
      go1 (nodeIdx n) vs
      go vs
    go1 _ [] = mrgReturn ()
    go1 vo (n : vs) = do
      symAssertWith e (vo /=~ nodeIdx n)
      go1 vo vs

circuitInputSymmetryReduction ::
  ( MonadError e m,
    UnionLike m,
    Mergeable e,
    Index idx,
    SemMap fm op a e g
  ) =>
  e ->
  fm ->
  Circuit op idx ->
  m ()
circuitInputSymmetryReduction e sem c = go (cirNodes c)
  where
    go [] = mrgReturn ()
    go (Node op _ vis : vs) = do
      let OpSem _ inputFeat _ = opSem sem op
      case inputFeat of
        CommutativeTwoOperands -> do
          case vis of
            [v1, v2] -> symAssertWith e (v1 <=~ v2)
        CommutativeAssociativeListOperands -> do
          mrgTraverse_ (\(v1, v2) -> symAssertWith e (v1 <=~ v2)) $ zip vis (tail vis)
        _ -> return ()
      go vs

{-
circuitGroupSymmetryReduction ::
  forall op fm a e g m idx.
  ( MonadError e m,
    UnionLike m,
    Mergeable e,
    Index idx,
    SemMap fm op a e g,
    Ord g
  ) =>
  e ->
  fm ->
  Circuit op idx ->
  m ()
circuitGroupSymmetryReduction e sem c = do
  undefined
  where
    split :: [Node op idx] -> [(NOperands, [Node op idx])]
    split [] = []
    split ns@(a : _) =
      case split1 (nodeOp a) ns of
        (l, r) -> (opOperands (opSem sem (nodeOp a)), l) : split r
    split1 :: op -> [Node op idx] -> ([Node op idx], [Node op idx])
    split1 _ [] = ([], [])
    split1 o (a : as) | opGroup (opSem sem (nodeOp a)) == opGroup (opSem sem o) =
      case split1 o as of
        (cur, neq) -> (a : cur, neq)
    split1 _ l = ([], l)
    grouped = split (sortOn (opGroup . opSem sem . nodeOp) (cirNodes c))

    goOut :: [Node op idx] -> m ()
    goOut nodes =
      mrgTraverse_ (\(Node _ vo1 _, Node _ vo2 _) -> symAssertWith e (vo1 <=~ vo2)) $
        zip nodes (tail nodes)
        -}

genCircuit ::
  forall idx op m a e g fm.
  ( Num idx,
    Index idx,
    GenSymSimpleConstrained (SOrdBound idx ()) idx,
    Mergeable op,
    MonadFresh m,
    MonadError e m,
    UnionLike m,
    Mergeable e,
    SemMap fm op a e g
  ) =>
  e ->
  CircuitSpec op fm ->
  m (Circuit op idx)
genCircuit e (CircuitSpec components inum onum fm doSymmRed) = do
  o :: [idx] <- simpleFreshConstrained e (SimpleListSpec onum (SOrdBound 0 (fromIntegral $ inum + compNum) () :: SOrdBound idx ()))
  n <- mrgTraverse genNode specs
  let circuit = Circuit inum n o
  circuitAcyclic e circuit
  circuitDistinctSlot e circuit
  when doSymmRed $ circuitInputSymmetryReduction e fm circuit
  mrgSingle circuit
  where
    specs' [] = []
    specs' ((x, n) : xs) = replicate n x ++ specs' xs
    specs = specs' components
    compNum = length specs
    genNode (ComponentSpec op opinum) = do
      o :: idx <- simpleFreshConstrained e (SOrdBound (fromIntegral inum) (fromIntegral $ inum + compNum) () :: SOrdBound idx ())
      i :: [idx] <-
        simpleFreshConstrained
          e
          (SimpleListSpec opinum (SOrdBound 0 (fromIntegral $ inum + compNum) () :: SOrdBound idx ()))
      mrgSingle $ Node op o i

type ValWithIdx idx a = (idx, a)

data ENode op idx a
  = ENode
      { enodeOp :: op,
        enodeResult :: ValWithIdx idx a,
        enodeInputs :: [ValWithIdx idx a]
      }
  | IENode {ienodeInput :: ValWithIdx idx a}
  deriving (Show, Eq)

data ECircuit op idx a = ECircuit
  { ecirInputNum :: Int,
    ecirNodes :: [ENode op idx a],
    ecirOutputIdx :: [idx]
  }
  deriving (Show, Eq)

genECircuit ::
  ( MonadError e m,
    Mergeable e,
    UnionLike m,
    Index idx,
    MonadFresh m,
    IntermediateSGen intermediateSGen op a
  ) =>
  e ->
  [a] ->
  Circuit op idx ->
  Int ->
  intermediateSGen ->
  m (ECircuit op idx a)
genECircuit e inputs (Circuit inum nodes o) intermediateSize gen =
  (\x -> ECircuit inum x o) . (goInputs 0 inputs ++) <$> go nodes
  where
    goInputs _ [] = []
    goInputs idx (x : xs) =
      IENode (mkInputIndex idx, x) : goInputs (idx + 1) xs
    go [] = return []
    go (Node op idx inputIdx : xs) = do
      r <- go xs
      g <- traverse (intermediateGen gen intermediateSize op) [-1 .. length inputIdx - 1]
      case g of
        ret : inputs ->
          return $ ENode op (idx, ret) (zip inputIdx inputs) : r
        _ -> error "Should not happen"

connected ::
  forall op idx a e m.
  (UnionLike m, Monad m, SEq a, MonadError e m, Mergeable e, Index idx) =>
  e ->
  ECircuit op idx a ->
  m ()
connected e (ECircuit _ enodes _) =
  mrgTraverse_
    ( \((oi, ov), (ii, iv)) ->
        symAssertWith e ((oi ==~ ii) `implies` (ov ==~ iv))
    )
    $ [(o, i) | o <- outputs, i <- inputs]
  where
    outputs :: [(idx, a)]
    outputs =
      ( \case
          ENode _ idxv _ -> idxv
          IENode idxv -> idxv
      )
        <$> enodes
    inputs :: [(idx, a)]
    inputs =
      ( \case
          ENode _ _ vis -> vis
          _ -> []
      )
        =<< enodes

interpretOp ::
  forall fm op a e g m.
  (SemMap fm op g e a, MonadError e m, UnionLike m, MonadFresh m, Mergeable a) =>
  op ->
  fm ->
  [a] ->
  m a
interpretOp op fm args = case opSemMaybe fm op of
  Just (OpSem _ _ func) -> func args
  Nothing -> error "interpretOp: no semantics"

semanticsCorrect ::
  forall op idx a e g fm m.
  ( SemMap fm op g e a,
    MonadError e m,
    Mergeable e,
    UnionLike m,
    MonadFresh m,
    Mergeable a,
    Index idx,
    SEq a
  ) =>
  e ->
  fm ->
  ECircuit op idx a ->
  m ()
semanticsCorrect err sem (ECircuit _ enodes _) = go enodes
  where
    go :: [ENode op idx a] -> m ()
    go [] = mrgReturn ()
    go (IENode {} : xs) = go xs
    go (ENode op (_, retVal) vis : xs) = do
      let args = snd <$> vis
      r <- interpretOp op sem args
      symAssertWith err (retVal ==~ r)
      go xs

interpretCircuit ::
  ( MonadError e m,
    MonadFresh m,
    Index idx,
    UnionLike m,
    Mergeable e,
    SEq a,
    SemMap fm op g e a,
    IntermediateSGen intermediateSGen op a,
    Mergeable a
  ) =>
  e ->
  [a] ->
  Circuit op idx ->
  fm ->
  Int ->
  intermediateSGen ->
  m [a]
interpretCircuit err inputs c@(Circuit inum nodes oidx) sem intermediateSize igen = do
  ec <- genECircuit err inputs c intermediateSize igen
  connected err ec
  semanticsCorrect err sem ec
  mrgTraverse (go (getOutputs ec)) (ecirOutputIdx ec)
  where
    getOutputs (ECircuit _ enodes _) =
      ( \case
          ENode _ r _ -> r
          IENode r -> r
      )
        <$> enodes
    go [] _ = mrgThrowError err
    go ((i, v) : xs) idx =
      mrgIf (i ==~ idx) (mrgReturn v) (go xs idx)
