{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Grisette.Lib.Synth.Reasoning.Parallel.BiasedQueue
  ( Priority (..),
    numericPriority,
    BiasedQueue,
    empty,
    delete,
    setPriority,
    insert,
    size,
    Grisette.Lib.Synth.Reasoning.Parallel.BiasedQueue.null,
    popMin,
  )
where

import qualified Data.HashPSQ as PSQ
import Grisette (PPrint, deriveGADT)
import Grisette.Lib.Synth.Reasoning.Parallel.DCTree (NodeId)
import System.Random.Stateful (AtomicGenM, StdGen, UniformRange (uniformRM))

data Priority = Priority
  { basePriority :: Double,
    randomPriority :: Double,
    knownWorking :: Bool,
    ancestorKnownWorking :: Bool,
    ancestorSiblingKnownWorking :: Bool
  }

deriveGADT [''Priority] [''Show, ''Eq, ''PPrint]

numericPriority :: Priority -> Double
numericPriority Priority {..} = basePriority * randomPriority

instance Ord Priority where
  Priority p1 r1 k1 pk1 ask1 <= Priority p2 r2 k2 pk2 ask2 =
    if k1 == k2
      then
        if pk1 == pk2
          then (if ask1 == ask2 then p1 * r1 <= p2 * r2 else ask1)
          else pk1
      else k1

data BiasedQueue = BiasedQueue
  { baseQueue :: PSQ.HashPSQ NodeId Priority NodeId,
    simpleQueue :: PSQ.HashPSQ NodeId Double NodeId,
    biasProbability :: Double
  }

empty :: Double -> BiasedQueue
empty biasProbability =
  BiasedQueue {baseQueue = PSQ.empty, simpleQueue = PSQ.empty, biasProbability}

delete :: NodeId -> BiasedQueue -> BiasedQueue
delete nid BiasedQueue {..} =
  BiasedQueue
    { baseQueue = PSQ.delete nid baseQueue,
      simpleQueue = PSQ.delete nid simpleQueue,
      biasProbability
    }

setPriority :: NodeId -> Priority -> BiasedQueue -> BiasedQueue
setPriority nid priority BiasedQueue {..} =
  BiasedQueue
    { baseQueue = case PSQ.lookup nid baseQueue of
        Nothing -> baseQueue
        Just (_, v) -> PSQ.insert nid priority v baseQueue,
      simpleQueue = case PSQ.lookup nid simpleQueue of
        Nothing -> simpleQueue
        Just (_, v) -> PSQ.insert nid (numericPriority priority) v simpleQueue,
      biasProbability
    }

insert :: NodeId -> Priority -> BiasedQueue -> BiasedQueue
insert nid priority BiasedQueue {..} =
  BiasedQueue
    { baseQueue = PSQ.insert nid priority nid baseQueue,
      simpleQueue = PSQ.insert nid (numericPriority priority) nid simpleQueue,
      biasProbability
    }

size :: BiasedQueue -> Int
size BiasedQueue {..} = PSQ.size baseQueue

null :: BiasedQueue -> Bool
null BiasedQueue {..} = PSQ.null baseQueue

popMin :: AtomicGenM StdGen -> BiasedQueue -> IO (NodeId, Bool, BiasedQueue)
popMin randGen queue@BiasedQueue {..} = do
  let Just (_, _, nodeIdBiased) = PSQ.findMin baseQueue
      Just (_, _, nodeIdRandom) = PSQ.findMin simpleQueue
  randVar <- uniformRM (0, 1) randGen
  let pickBiased = randVar < biasProbability
  let nodeId = if pickBiased then nodeIdBiased else nodeIdRandom
  return (nodeId, pickBiased, delete nodeId queue)