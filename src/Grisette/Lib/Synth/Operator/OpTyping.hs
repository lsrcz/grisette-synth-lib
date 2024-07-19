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
{-# LANGUAGE TypeApplications #-}

module Grisette.Lib.Synth.Operator.OpTyping
  ( OpTyping (..),
    simpleTyping,
    symOpMaximumArgNum,
    symOpMaximumResNum,
    DefaultType (DefaultType),
    unaryDefaultType,
    binaryDefaultType,
    ternaryDefaultType,
  )
where

import Control.Monad.Except (runExceptT)
import Grisette
  ( Default (Default),
    PPrint (pformat),
    GenSym (fresh),
    Mergeable (rootStrategy),
    MergingStrategy (SimpleStrategy),
    MonadUnion,
    SimpleMergeable (mrgIte),
    Union,
    deriveAllExcept,
    liftUnion,
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
  OpTyping (Union op) ty ctx
  where
  typeOp op = tryMerge $ liftUnion op >>= typeOp

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
      Left _ -> return 0 :: Union Int
      Right (TypeSignature argTypes _) -> return $ length argTypes

symOpMaximumResNum :: (OpTyping op ty SymbolicContext) => op -> Int
symOpMaximumResNum op =
  unMaxAcrossBranches $ simpleMerge $ mrgFmap MaxAcrossBranches $ do
    ty <- runExceptT $ typeOp op
    case ty of
      Left _ -> return 0 :: Union Int
      Right (TypeSignature _ resTypes) -> return $ length resTypes

data DefaultType = DefaultType

deriveAllExcept ''DefaultType [''PPrint]

instance PPrint DefaultType where
  pformat _ = "default"

instance (Mergeable a, GenSym () a) => GenSym DefaultType a where
  fresh _ = fresh ()

unaryDefaultType :: TypeSignature DefaultType
unaryDefaultType = TypeSignature [DefaultType] [DefaultType]

binaryDefaultType :: TypeSignature DefaultType
binaryDefaultType = TypeSignature [DefaultType, DefaultType] [DefaultType]

ternaryDefaultType :: TypeSignature DefaultType
ternaryDefaultType =
  TypeSignature [DefaultType, DefaultType, DefaultType] [DefaultType]
