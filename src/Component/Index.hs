{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Component.Index
  ( Index (..),
    boundIndex,
    CIndex (..),
    unsafeAtCIndex,
    unsafeAtCIndex',
  )
where

import Control.Monad.Except
import GHC.TypeLits
import Grisette

class SOrd i => Index i where
  mkInputIndex :: Int -> i
  mkInternalIndex :: Int -> Int -> i
  mkIndex :: Int -> Int -> i
  mkIndex numInputs v = if v < numInputs then mkInputIndex v else mkInternalIndex numInputs (v - numInputs)
  atIndex :: (MonadUnion m, MonadError e m, Mergeable v) => e -> [v] -> i -> m v
  atIndex' :: (MonadUnion m, MonadError e m, Mergeable v) => e -> [v] -> [v] -> i -> m v
  atIndex' e inputs internals = atIndex e (inputs ++ internals)

boundIndex :: Index i => (i, i) -> i -> SymBool
boundIndex (lower, upper) i = lower <=~ i &&~ i <~ upper

class Ord i => CIndex i where
  mkCInputIndex :: Int -> i
  mkCInternalIndex :: Int -> Int -> i
  mkCIndex :: Int -> Int -> i
  mkCIndex numInputs v = if v < numInputs then mkCInputIndex v else mkCInternalIndex numInputs (v - numInputs)
  atCIndex :: (MonadError e m) => e -> [v] -> i -> m v
  atCIndex' :: (MonadError e m) => e -> [v] -> [v] -> i -> m v
  atCIndex' e inputs internals = atCIndex e (inputs ++ internals)

unsafeAtCIndex :: (CIndex i) => [v] -> i -> v
unsafeAtCIndex list i = case atCIndex (0 :: Integer) list i of
  Left _ -> error "No such elem"
  Right v -> v

unsafeAtCIndex' :: (CIndex i) => [v] -> [v] -> i -> v
unsafeAtCIndex' inputs internals = unsafeAtCIndex (inputs ++ internals)

instance Index SymInteger where
  mkInputIndex = fromIntegral
  mkInternalIndex numInputs v = fromIntegral $ numInputs + v
  atIndex e [] i = mrgThrowError e
  atIndex e (x : xs) i = mrgIf (i ==~ 0) (mrgReturn x) $ atIndex e xs (i - 1)

instance (KnownNat n, 1 <= n) => Index (SymWordN n) where
  mkInputIndex = fromIntegral
  mkInternalIndex numInputs v = fromIntegral $ numInputs + v
  atIndex e [] i = mrgThrowError e
  atIndex e (x : xs) i = mrgIf (i ==~ 0) (mrgReturn x) $ atIndex e xs (i - 1)

instance Index (UnionM Int) where
  mkInputIndex = fromIntegral
  mkInternalIndex numInputs v = fromIntegral $ numInputs + v

  atIndex e l i = do
    i1 <- liftToMonadUnion i
    if i1 < length l
      then return $ l !! i1
      else mrgThrowError e

instance CIndex Integer where
  mkCInputIndex = fromIntegral
  mkCInternalIndex numInputs v = fromIntegral $ numInputs + v
  atCIndex e [] i = throwError e
  atCIndex e (x : xs) i = if i == 0 then return x else atCIndex e xs (i - 1)

instance (KnownNat n, 1 <= n) => CIndex (WordN n) where
  mkCInputIndex = fromIntegral
  mkCInternalIndex numInputs v = fromIntegral $ numInputs + v
  atCIndex e [] i = throwError e
  atCIndex e (x : xs) i = if i == 0 then return x else atCIndex e xs (i - 1)
