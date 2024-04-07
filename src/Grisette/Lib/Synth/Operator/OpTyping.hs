{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Operator.OpTyping
  ( OpTyping (..),
    OpTypingSimple (..),
    SymOpLimits (..),
  )
where

import Control.Monad.Except (runExceptT)
import Grisette
  ( Mergeable (rootStrategy),
    MergingStrategy (SimpleStrategy),
    MonadUnion,
    SimpleMergeable (mrgIte),
    UnionM,
    liftUnionM,
    mrgFmap,
    mrgReturn,
    simpleMerge,
    tryMerge,
  )
import Grisette.Lib.Synth.Context (MonadContext, SymbolicContext)
import Grisette.Lib.Synth.TypeSignature
  ( TypeSignature (TypeSignature),
  )

class OpTypingSimple op ty | op -> ty where
  typeOpSimple :: op -> TypeSignature ty

class (MonadContext ctx) => OpTyping op ty ctx | op -> ty where
  typeOp :: op -> ctx (TypeSignature ty)
  default typeOp ::
    (OpTypingSimple op ty, MonadContext ctx, Mergeable ty) =>
    op ->
    ctx (TypeSignature ty)
  typeOp = mrgReturn . typeOpSimple

instance
  (MonadUnion ctx, OpTyping op ty ctx, Mergeable op, Mergeable ty) =>
  OpTyping (UnionM op) ty ctx
  where
  typeOp op = tryMerge $ liftUnionM op >>= typeOp

newtype MaxAcrossBranches = MaxAcrossBranches {unMaxAcrossBranches :: Int}

instance Mergeable MaxAcrossBranches where
  rootStrategy = SimpleStrategy mrgIte

instance SimpleMergeable MaxAcrossBranches where
  mrgIte _ (MaxAcrossBranches a) (MaxAcrossBranches b) =
    MaxAcrossBranches (max a b)

class SymOpLimits op where
  symOpMaximumArgNum :: op -> Int
  default symOpMaximumArgNum :: (OpTyping op ty SymbolicContext) => op -> Int
  symOpMaximumArgNum op =
    unMaxAcrossBranches $ simpleMerge $ mrgFmap MaxAcrossBranches $ do
      ty <- runExceptT $ typeOp op
      case ty of
        Left _ -> return 0 :: UnionM Int
        Right (TypeSignature argTypes _) -> return $ length argTypes
  symOpMaximumResNum :: op -> Int
  default symOpMaximumResNum :: (OpTyping op ty SymbolicContext) => op -> Int
  symOpMaximumResNum op =
    unMaxAcrossBranches $ simpleMerge $ mrgFmap MaxAcrossBranches $ do
      ty <- runExceptT $ typeOp op
      case ty of
        Left _ -> return 0 :: UnionM Int
        Right (TypeSignature _ resTypes) -> return $ length resTypes

instance
  (SymOpLimits op, Mergeable op) =>
  SymOpLimits (UnionM op)
  where
  symOpMaximumArgNum op =
    unMaxAcrossBranches $
      simpleMerge $
        mrgFmap (MaxAcrossBranches . symOpMaximumArgNum) op
  symOpMaximumResNum op =
    unMaxAcrossBranches $
      simpleMerge $
        mrgFmap (MaxAcrossBranches . symOpMaximumResNum) op
