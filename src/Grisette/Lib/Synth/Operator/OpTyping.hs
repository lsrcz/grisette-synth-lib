{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Operator.OpTyping
  ( OpTyping (..),
    OpTypingSimple (..),
    SymOpLimits (..),
    SymOpTyping (..),
  )
where

import Grisette (Mergeable, MonadUnion, UnionM, mrgReturn)
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.TypeSignature
  ( TypeSignature (argTypes, resTypes),
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

class SymOpLimits op where
  symOpMaximumArgNum :: op -> Int
  default symOpMaximumArgNum :: (OpTypingSimple op ty) => op -> Int
  symOpMaximumArgNum = length . argTypes . typeOpSimple
  symOpMaximumResNum :: op -> Int
  default symOpMaximumResNum :: (OpTypingSimple op ty) => op -> Int
  symOpMaximumResNum = length . resTypes . typeOpSimple

class
  (Mergeable ty, MonadContext ctx, MonadUnion ctx) =>
  SymOpTyping op ty ctx
    | op -> ty
  where
  typeSymOp :: op -> ctx (UnionM (TypeSignature ty))
  default typeSymOp ::
    (OpTyping op ty ctx, Mergeable ty) =>
    op ->
    ctx (UnionM (TypeSignature ty))
  typeSymOp op = do
    tys <- typeOp op
    mrgReturn $ mrgReturn tys
