{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
import Data.ByteString qualified as B
import Data.List
import GHC.Generics
import Grisette
import Grisette.Experimental

data Node op idx = Node
  { nodeOp :: op,
    nodeIdx :: [idx],
    nodeInputIdx :: [idx]
  }
  deriving (Show, Eq, Generic)
  deriving (Mergeable, SEq, EvaluateSym) via (Default (Node op idx))

data Circuit op idx = Circuit
  { cirInputNames :: [B.ByteString],
    cirNodes :: [Node op idx],
    cirOutputIdx :: [idx]
  }
  deriving (Show, Eq, Generic)
  deriving (Mergeable, SEq, EvaluateSym) via (Default (Circuit op idx))

data ComponentSpec e op
  = ComponentSpec
      { csOp :: op,
        csInputNum :: Int,
        csOutputNum :: Int
      }
  | ComponentGenOpSpec
      { csgOp :: forall m. (MonadFresh m, MonadError e m, UnionLike m) => m op,
        csInputNum :: Int,
        csOutputNum :: Int
      }

data CircuitSpec e op fm = CircuitSpec
  { cirSpecComponents :: [(ComponentSpec e op, Int)],
    cirSpecInputNames :: [B.ByteString],
    cirSpecOutputNum :: Int,
    cirSpecSem :: fm,
    cirDoSymmReduction :: Bool
  }

defaultCircuitSpec :: [(ComponentSpec e op, Int)] -> [B.ByteString] -> Int -> fm -> CircuitSpec e op fm
defaultCircuitSpec components inputNames onum sem = CircuitSpec components inputNames onum sem True

circuitAcyclic :: (MonadError e m, Mergeable e, UnionLike m, Index idx) => e -> Circuit op idx -> m ()
circuitAcyclic e c = go (cirNodes c)
  where
    go [] = mrgReturn ()
    go (Node _ vo vis : vs) = do
      go1 (head vo) vis
      go vs
    go1 _ [] = mrgReturn ()
    go1 vo (vi : vis) = do
      symAssertWith e (vi <~ vo)
      go1 vo vis

circuitDistinctSlot ::
  (MonadError e m, Mergeable e, UnionLike m, Index idx) =>
  e ->
  Circuit op idx ->
  m ()
circuitDistinctSlot e c = goIndices indices
  where
    indices = cirNodes c >>= nodeIdx
    goIndices [] = mrgReturn ()
    goIndices [x] = mrgReturn ()
    goIndices (x : xs) = do
      mrgTraverse_ (\y -> symAssertWith e (x /=~ y)) xs
      goIndices xs

{-
go [] = mrgReturn ()
go (n : vs) = do
  go1 (nodeIdx n) vs
  go vs
go1 _ [] = mrgReturn ()
go1 vo (n : vs) = do
  symAssertWith e (vo /=~ nodeIdx n)
  go1 vo vs
  -}

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
      let OpSem _ inputFeat _ _ = opSem sem op
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
  CircuitSpec e op fm ->
  m (Circuit op idx)
genCircuit e (CircuitSpec components inames onum fm doSymmRed) = do
  o :: [idx] <- simpleFreshConstrained e (SimpleListSpec onum (SOrdBound 0 (fromIntegral $ inum + compNum) () :: SOrdBound idx ()))
  n <- mrgTraverse genNode specs
  let circuit = Circuit inames n o
  circuitAcyclic e circuit
  circuitDistinctSlot e circuit
  when doSymmRed $ circuitInputSymmetryReduction e fm circuit
  mrgSingle circuit
  where
    inum = length inames
    specs' [] = []
    specs' ((x, n) : xs) = replicate n x ++ specs' xs
    specs = specs' components
    compNum = sum $ csOutputNum <$> specs
    genNode (ComponentGenOpSpec op opinum oponum) = do
      o :: idx <-
        simpleFreshConstrained
          e
          ( SOrdBound
              (fromIntegral inum)
              (fromIntegral $ inum + compNum - oponum + 1)
              () ::
              SOrdBound idx ()
          )
      let os = fmap ((o +) . fromIntegral) [0 .. oponum - 1]
      i :: [idx] <-
        simpleFreshConstrained
          e
          (SimpleListSpec opinum (SOrdBound 0 (fromIntegral $ inum + compNum) () :: SOrdBound idx ()))
      op1 <- op
      mrgSingle $ Node op1 os i
    genNode (ComponentSpec op opinum oponum) = do
      o :: idx <-
        simpleFreshConstrained
          e
          ( SOrdBound
              (fromIntegral inum)
              (fromIntegral $ inum + compNum - oponum + 1)
              () ::
              SOrdBound idx ()
          )
      let os = fmap ((o +) . fromIntegral) [0 .. oponum - 1]
      i :: [idx] <-
        simpleFreshConstrained
          e
          (SimpleListSpec opinum (SOrdBound 0 (fromIntegral $ inum + compNum) () :: SOrdBound idx ()))
      mrgSingle $ Node op os i

type ValWithIdx idx a = (idx, a)

data ENode op idx a
  = ENode
      { enodeOp :: op,
        enodeResult :: [ValWithIdx idx a],
        enodeInputs :: [ValWithIdx idx a]
      }
  | IENode {ienodeInput :: ValWithIdx idx a}
  deriving (Show, Eq)

data ECircuit op idx a = ECircuit
  { ecirInputNames :: [B.ByteString],
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
genECircuit e inputs (Circuit inames nodes o) intermediateSize gen =
  (\x -> ECircuit inames x o) . (goInputs 0 inputs ++) <$> go nodes
  where
    goInputs _ [] = []
    goInputs idx (x : xs) =
      IENode (mkInputIndex idx, x) : goInputs (idx + 1) xs
    go [] = return []
    go (Node op idx inputIdx : xs) = do
      r <- go xs
      ret <-
        traverse (intermediateGen gen intermediateSize op) $
          SGenOutput <$> [0 .. length idx - 1]
      inputs <-
        traverse (intermediateGen gen intermediateSize op) $
          SGenInput <$> [0 .. length inputIdx - 1]
      return $ ENode op (zip idx ret) (zip inputIdx inputs) : r

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
          IENode idxv -> [idxv]
      )
        =<< enodes
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
  m [a]
interpretOp op fm args = case opSemMaybe fm op of
  Just (OpSem _ _ _ func) -> func args
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
    go (ENode op rets vis : xs) = do
      let args = snd <$> vis
      r <- interpretOp op sem args
      symAssertWith err (fmap snd rets ==~ r)
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
          IENode r -> [r]
      )
        =<< enodes
    go [] _ = mrgThrowError err
    go ((i, v) : xs) idx =
      mrgIf (i ==~ idx) (mrgReturn v) (go xs idx)
