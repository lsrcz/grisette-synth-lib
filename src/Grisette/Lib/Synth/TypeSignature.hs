{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.TypeSignature (TypeSignature (..)) where

import GHC.Generics (Generic)
import Grisette (Default (Default), EvaluateSym, Mergeable)

data TypeSignature ty = TypeSignature {argTypes :: [ty], resTypes :: [ty]}
  deriving (Show, Eq, Generic)
  deriving (EvaluateSym, Mergeable) via (Default (TypeSignature ty))
