{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Operator.OpTyping
  ( OpTyping (..),
    simpleTyping,
    symOpMaximumArgNum,
    symOpMaximumResNum,
    DefaultType (DefaultType),
  )
where

import Control.Monad.Except (runExceptT)
import Grisette
  ( Default (Default),
    GPretty (gpretty),
    GenSym (fresh),
    Mergeable (rootStrategy),
    MergingStrategy (SimpleStrategy),
    MonadUnion,
    SimpleMergeable (mrgIte),
    UnionM,
    deriveAllGrisetteExcept,
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

simpleTyping ::
  (MonadContext ctx, Mergeable ty) =>
  (op -> TypeSignature ty) ->
  op ->
  ctx (TypeSignature ty)
simpleTyping f = mrgReturn . f

class (MonadContext ctx) => OpTyping op ty ctx | op -> ty where
  typeOp :: op -> ctx (TypeSignature ty)

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

symOpMaximumArgNum :: (OpTyping op ty SymbolicContext) => op -> Int
symOpMaximumArgNum op =
  unMaxAcrossBranches $ simpleMerge $ mrgFmap MaxAcrossBranches $ do
    ty <- runExceptT $ typeOp op
    case ty of
      Left _ -> return 0 :: UnionM Int
      Right (TypeSignature argTypes _) -> return $ length argTypes

symOpMaximumResNum :: (OpTyping op ty SymbolicContext) => op -> Int
symOpMaximumResNum op =
  unMaxAcrossBranches $ simpleMerge $ mrgFmap MaxAcrossBranches $ do
    ty <- runExceptT $ typeOp op
    case ty of
      Left _ -> return 0 :: UnionM Int
      Right (TypeSignature _ resTypes) -> return $ length resTypes

data DefaultType = DefaultType

deriveAllGrisetteExcept ''DefaultType [''GPretty]

instance GPretty DefaultType where
  gpretty _ = "default"

instance (Mergeable a, GenSym () a) => GenSym DefaultType a where
  fresh _ = fresh ()
