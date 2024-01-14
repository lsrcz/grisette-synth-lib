{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
  )
where

import Control.Monad.Except (ExceptT, MonadError (throwError))
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans (MonadTrans (lift))
import qualified Data.Text as T
import Grisette
  ( FreshT (FreshT),
    Mergeable,
    MonadUnion,
    UnionM,
    merge,
    mrgReturn,
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

-- | A concrete context is a context that does not do multi-path symbolic
-- execution.
type ConcreteContext = Either T.Text

instance MonadContext ConcreteContext where
  result = return
  raiseError = throwError
  mergeIfNeeded = id

-- | A symbolic context is a context that does multi-path symbolic execution.
type SymbolicContext = ExceptT T.Text UnionM

instance MonadContext SymbolicContext where
  result = mrgReturn
  raiseError = mrgThrowError
  mergeIfNeeded = merge

-- | An angelic context is a context that does multi-path symbolic execution
-- with angelic choices.
type AngelicContext = FreshT (ExceptT T.Text UnionM)

instance (MonadContext ctx, MonadUnion ctx) => MonadContext (FreshT ctx) where
  mergeIfNeeded (FreshT f) =
    FreshT $ \ident index -> mergeIfNeeded $ f ident index

instance
  (MonadContext ctx, Mergeable st) =>
  MonadContext (Lazy.StateT st ctx)
  where
  mergeIfNeeded (Lazy.StateT f) =
    Lazy.StateT $ \st -> mergeIfNeeded $ f st

instance
  (MonadContext ctx, Mergeable st) =>
  MonadContext (Strict.StateT st ctx)
  where
  mergeIfNeeded (Strict.StateT f) =
    Strict.StateT $ \st -> mergeIfNeeded $ f st
