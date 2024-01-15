{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :    Grisette.Lib.Synth.Context
-- Copyright   :    (c) Sirui Lu 2024
-- License     :    BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :    siruilu@cs.washington.edu
-- Stability   :    experimental
-- Portability :    GHC only
module Grisette.Lib.Synth.Context
  ( MonadContext (..),
    ConcreteContext,
    SymbolicContext,
    AngelicContext,
    traverseC,
    traverseC_,
    sequenceC,
    sequenceC_,
  )
where

import Control.Monad (void)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans (MonadTrans (lift))
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Grisette
  ( FreshT (FreshT),
    Mergeable (rootStrategy),
    Mergeable1,
    Mergeable2 (liftRootStrategy2),
    MergingStrategy,
    MonadUnion,
    UnionLike (mergeWithStrategy),
    UnionM,
    mrgReturn,
    rootStrategy1,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)

-- | A `MonadContext` is the monad type we use in the synthesizer. It unifies
-- the concrete and symbolic contexts, and allows us to use the same code for
-- both and merge correctly.
class (MonadError T.Text ctx) => MonadContext ctx where
  -- | `result` is the same as `return` but it should merge the value if used
  -- with a symbolic context.
  result :: (Mergeable a) => a -> ctx a
  default result ::
    (MonadTrans t, ctx ~ t m, MonadContext m, Mergeable a) => a -> ctx a
  result = mergeIfNeeded . lift . result

  -- | `raiseError` is the same as `throwError` but it should merge the value if
  -- used with a symbolic context.
  raiseError :: (Mergeable a) => T.Text -> ctx a
  default raiseError ::
    (MonadTrans t, ctx ~ t m, MonadContext m, Mergeable a) => T.Text -> ctx a
  raiseError = mergeIfNeeded . lift . raiseError

  -- | `mergeIfNeeded` should merge the value if used with a symbolic context.
  mergeIfNeeded :: (Mergeable a) => ctx a -> ctx a
  mergeIfNeeded = mergeIfNeededWithStrategy rootStrategy

  -- | `mergeIfNeededWithStrategy` should merge the value if used with a
  -- symbolic context based on the merging strategy.
  mergeIfNeededWithStrategy :: MergingStrategy a -> ctx a -> ctx a

-- | A concrete context is a context that does not do multi-path symbolic
-- execution.
type ConcreteContext = Either T.Text

instance MonadContext ConcreteContext where
  result = return
  raiseError = throwError
  mergeIfNeededWithStrategy _ = id

-- | A symbolic context is a context that does multi-path symbolic execution.
type SymbolicContext = ExceptT T.Text UnionM

instance MonadContext SymbolicContext where
  result = mrgReturn
  raiseError = mrgThrowError
  mergeIfNeededWithStrategy = mergeWithStrategy

-- | An angelic context is a context that does multi-path symbolic execution
-- with angelic choices.
type AngelicContext = FreshT (ExceptT T.Text UnionM)

instance (MonadContext ctx, MonadUnion ctx) => MonadContext (FreshT ctx) where
  mergeIfNeededWithStrategy strategy (FreshT f) =
    FreshT $ \ident index ->
      mergeIfNeededWithStrategy (liftRootStrategy2 strategy rootStrategy) $
        f ident index

instance
  (MonadContext ctx, Mergeable st) =>
  MonadContext (Lazy.StateT st ctx)
  where
  mergeIfNeededWithStrategy strategy (Lazy.StateT f) =
    Lazy.StateT $ \st ->
      mergeIfNeededWithStrategy (liftRootStrategy2 strategy rootStrategy) $ f st

instance
  (MonadContext ctx, Mergeable st) =>
  MonadContext (Strict.StateT st ctx)
  where
  mergeIfNeededWithStrategy strategy (Strict.StateT f) =
    Strict.StateT $ \st ->
      mergeIfNeededWithStrategy (liftRootStrategy2 strategy rootStrategy) $ f st

traverseC ::
  (MonadContext ctx, Mergeable1 t, Mergeable b, Traversable t) =>
  (a -> ctx b) ->
  t a ->
  ctx (t b)
traverseC f =
  mergeIfNeededWithStrategy rootStrategy1 . traverse (mergeIfNeeded . f)

traverseC_ ::
  (MonadContext ctx, Mergeable1 t, Traversable t) =>
  (a -> ctx b) ->
  t a ->
  ctx ()
traverseC_ f =
  mergeIfNeededWithStrategy rootStrategy
    . void
    . traverse_ (mergeIfNeeded . void . f)

sequenceC ::
  (MonadContext ctx, Mergeable1 t, Mergeable a, Traversable t) =>
  t (ctx a) ->
  ctx (t a)
sequenceC = traverseC id

sequenceC_ ::
  (MonadContext ctx, Mergeable1 t, Traversable t) =>
  t (ctx a) ->
  ctx ()
sequenceC_ = traverseC_ id
