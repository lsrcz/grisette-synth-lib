{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Operator.OpTyping
  ( OpTypingSimple (..),
    OpTypingByNumInputs (..),
    OpTyping (..),
    UseOpTypingSimple (..),
    UseOpTypingByNumInputs (..),
  )
where

import Grisette (Mergeable)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.TypeSignature (TypeSignature (TypeSignature))
import Grisette.Lib.Synth.Util.Show (showText)

class OpTypingSimple op ty where
  typeOpSimple :: (MonadContext ctx) => op -> ctx (TypeSignature ty)

class OpTypingByNumInputs op ty where
  typeOpByNumInputs ::
    (MonadContext ctx) => op -> Int -> ctx (TypeSignature ty)

class OpTyping op ty where
  typeOp :: (MonadContext ctx) => op -> [ty] -> ctx (TypeSignature ty)

newtype UseOpTypingSimple op = UseOpTypingSimple op

instance
  (OpTypingSimple op ty, Show op, Mergeable ty) =>
  OpTypingByNumInputs (UseOpTypingSimple op) ty
  where
  typeOpByNumInputs (UseOpTypingSimple op) numInputs = do
    signature@(TypeSignature argTypes _) <- typeOpSimple op
    if length argTypes /= numInputs
      then
        mrgThrowError $
          "typeOpByNumInputs: the operator "
            <> showText op
            <> " cannot accept "
            <> showText numInputs
            <> " arguments, expected "
            <> showText (length argTypes)
            <> " arguments."
      else return signature

instance
  (OpTypingSimple op ty, Show op, Eq ty, Mergeable ty, Show ty) =>
  OpTyping (UseOpTypingSimple op) ty
  where
  typeOp (UseOpTypingSimple op) ty = do
    signature@(TypeSignature argTypes _) <- typeOpSimple op
    if argTypes /= ty
      then
        mrgThrowError $
          "typeOp: the operator "
            <> showText op
            <> " cannot accept the arguments of type "
            <> showText ty
            <> ", expected "
            <> showText argTypes
            <> "."
      else return signature

newtype UseOpTypingByNumInputs op = UseOpTypingByNumInputs op

instance
  (OpTypingByNumInputs op ty, Show op, Eq ty, Mergeable ty, Show ty) =>
  OpTyping (UseOpTypingByNumInputs op) ty
  where
  typeOp (UseOpTypingByNumInputs op) ty = do
    signature@(TypeSignature argTypes _) <- typeOpByNumInputs op (length ty)
    if argTypes /= ty
      then
        mrgThrowError $
          "typeOp: the operator "
            <> showText op
            <> " cannot accept the arguments of type "
            <> showText ty
            <> ", expected "
            <> showText argTypes
            <> "."
      else return signature
