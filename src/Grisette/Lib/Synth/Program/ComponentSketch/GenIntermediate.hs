{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Program.ComponentSketch.GenIntermediate
  ( GenIntermediate (..),
    Intermediates (..),
    genIntermediates,
    genOpIntermediates,
  )
where

import GHC.Generics (Generic)
import Grisette (Default (Default), Mergeable, MonadFresh)
import Grisette.Lib.Data.Traversable (mrgTraverse)
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.TypeSignature (TypeSignature (argTypes, resTypes))

class
  (MonadFresh ctx, MonadContext ctx, Mergeable val) =>
  GenIntermediate sem ty val ctx
  where
  genIntermediate :: sem -> ty -> ctx val

genIntermediates ::
  (GenIntermediate sem ty val ctx) => sem -> [ty] -> ctx [val]
genIntermediates sem = mrgTraverse (genIntermediate sem)

data Intermediates val = Intermediates
  { argIntermediates :: [val],
    resIntermediates :: [val]
  }
  deriving (Show, Eq, Generic)
  deriving (Mergeable) via (Default (Intermediates val))

genOpIntermediates ::
  forall semObj ty val ctx p.
  (GenIntermediate semObj ty val ctx) =>
  p ty ->
  semObj ->
  TypeSignature ty ->
  ctx (Intermediates val)
genOpIntermediates _ sem signature = do
  arg <- genIntermediates sem $ argTypes signature
  res <- genIntermediates sem $ resTypes signature
  return $ Intermediates arg res
