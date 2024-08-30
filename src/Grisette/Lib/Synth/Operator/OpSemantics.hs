{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Operator.OpSemantics
  ( OpSemantics (..),
    DefaultSem (..),
    pureUnaryOp,
    unaryOp,
    pureBinaryOp,
    binaryOp,
    pureTernaryOp,
    ternaryOp,
  )
where

import Control.DeepSeq (NFData (rnf))
import qualified Data.Text as T
import Grisette (Mergeable, MonadUnion, Union, liftUnion, mrgReturn, tryMerge)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Util.Show (showText)

class (MonadContext ctx) => OpSemantics semObj op val ctx where
  applyOp :: semObj -> op -> [val] -> ctx [val]

pureUnaryOp ::
  (MonadContext ctx, Mergeable val) =>
  T.Text ->
  (val -> val) ->
  [val] ->
  ctx [val]
pureUnaryOp name f = unaryOp name (mrgReturn . f)

unaryOp ::
  (MonadContext ctx, Mergeable val) =>
  T.Text ->
  (val -> ctx val) ->
  [val] ->
  ctx [val]
unaryOp _ f [a] = do
  v <- f a
  mrgReturn [v]
unaryOp name _ l =
  mrgThrowError $
    "Expected 1 arguments for " <> name <> ", but got " <> showText (length l)

pureBinaryOp ::
  (MonadContext ctx, Mergeable val) =>
  T.Text ->
  (val -> val -> val) ->
  [val] ->
  ctx [val]
pureBinaryOp name f = binaryOp name (\a b -> mrgReturn $ f a b)

binaryOp ::
  (MonadContext ctx, Mergeable val) =>
  T.Text ->
  (val -> val -> ctx val) ->
  [val] ->
  ctx [val]
binaryOp _ f [a, b] = do
  v <- f a b
  mrgReturn [v]
binaryOp name _ l =
  mrgThrowError $
    "Expected 2 arguments for " <> name <> ", but got " <> showText (length l)

pureTernaryOp ::
  (MonadContext ctx, Mergeable val) =>
  T.Text ->
  (val -> val -> val -> val) ->
  [val] ->
  ctx [val]
pureTernaryOp name f = ternaryOp name (\a b c -> mrgReturn $ f a b c)

ternaryOp ::
  (MonadContext ctx, Mergeable val) =>
  T.Text ->
  (val -> val -> val -> ctx val) ->
  [val] ->
  ctx [val]
ternaryOp _ f [a, b, c] = do
  v <- f a b c
  mrgReturn [v]
ternaryOp name _ l =
  mrgThrowError $
    "Expected 3 arguments for " <> name <> ", but got " <> showText (length l)

instance
  ( MonadUnion ctx,
    OpSemantics semObj op val ctx,
    Mergeable op,
    Mergeable val
  ) =>
  OpSemantics semObj (Union op) val ctx
  where
  applyOp semObj op args = tryMerge $ do
    op' <- liftUnion op
    applyOp semObj op' args

data DefaultSem = DefaultSem

instance NFData DefaultSem where
  rnf DefaultSem = ()
