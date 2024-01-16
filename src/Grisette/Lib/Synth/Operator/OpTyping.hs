{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Operator.OpTyping
  ( OpTyping (..),
    GenIntermediate (..),
    genIntermediates,
    genOpIntermediates,
  )
where

import Grisette (Mergeable, MonadFresh)
import Grisette.Lib.Synth.Context (MonadContext, traverseC)

class (MonadContext ctx) => OpTyping semObj op ty ctx where
  typeOp :: semObj -> op -> Int -> ctx ([ty], [ty])

class
  (MonadFresh ctx, MonadContext ctx, Mergeable val) =>
  GenIntermediate sem ty val ctx
  where
  genIntermediate :: sem -> ty -> ctx val

genIntermediates ::
  (GenIntermediate sem ty val ctx) => sem -> [ty] -> ctx [val]
genIntermediates sem = traverseC (genIntermediate sem)

genOpIntermediates ::
  forall semObj op ty val ctx p.
  (OpTyping semObj op ty ctx, GenIntermediate semObj ty val ctx) =>
  p ty ->
  semObj ->
  op ->
  Int ->
  ctx ([val], [val])
genOpIntermediates _ sem op argNum = do
  (argTypes :: [ty], resTypes) <- typeOp sem op argNum
  arg <- genIntermediates sem argTypes
  res <- genIntermediates sem resTypes
  return (arg, res)
