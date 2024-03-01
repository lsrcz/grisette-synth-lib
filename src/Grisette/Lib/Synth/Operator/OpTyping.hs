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
  )
where

import Grisette (Mergeable, mrgReturn)
import Grisette.Lib.Synth.Context (ConcreteContext, MonadContext)
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

class SymOpLimits op where
  symOpMaximumArgNum :: op -> Int
  default symOpMaximumArgNum :: (OpTyping op ty ConcreteContext) => op -> Int
  symOpMaximumArgNum op =
    case typeOp op of
      Right (TypeSignature argTypes _) -> length argTypes
      Left err -> error $ "symOpMaximumArgNum: " ++ show err
  symOpMaximumResNum :: op -> Int
  default symOpMaximumResNum :: (OpTyping op ty ConcreteContext) => op -> Int
  symOpMaximumResNum op =
    case typeOp op of
      Right (TypeSignature _ resTypes) -> length resTypes
      Left err -> error $ "symOpMaximumArgNum: " ++ show err
