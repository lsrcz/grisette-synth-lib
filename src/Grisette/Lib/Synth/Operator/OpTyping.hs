{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Operator.OpTyping
  ( OpTyping (..),
    GenIntermediate (..),
    TypeSignature (..),
    genIntermediates,
    genOpIntermediates,
  )
where

import GHC.Generics (Generic)
import Grisette (Default (Default), Mergeable, MonadFresh)
import Grisette.Lib.Data.Traversable (mrgTraverse)
import Grisette.Lib.Synth.Context (MonadContext)

data TypeSignature ty = TypeSignature {argTypes :: [ty], resTypes :: [ty]}
  deriving (Show, Eq, Generic)
  deriving (Mergeable) via (Default (TypeSignature ty))

class (MonadContext ctx) => OpTyping semObj op ty ctx where
  typeOp :: semObj -> op -> Int -> ctx (TypeSignature ty)

class
  (MonadFresh ctx, MonadContext ctx, Mergeable val) =>
  GenIntermediate sem ty val ctx
  where
  genIntermediate :: sem -> ty -> ctx val

genIntermediates ::
  (GenIntermediate sem ty val ctx) => sem -> [ty] -> ctx [val]
genIntermediates sem = mrgTraverse (genIntermediate sem)

genOpIntermediates ::
  forall semObj op ty val ctx p.
  (OpTyping semObj op ty ctx, GenIntermediate semObj ty val ctx) =>
  p ty ->
  semObj ->
  op ->
  Int ->
  ctx ([val], [val])
genOpIntermediates _ sem op argNum = do
  signature :: TypeSignature ty <- typeOp sem op argNum
  arg <- genIntermediates sem $ argTypes signature
  res <- genIntermediates sem $ resTypes signature
  return (arg, res)
