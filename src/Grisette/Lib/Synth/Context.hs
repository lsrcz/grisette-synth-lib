{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :    Grisette.Lib.Synth.Context
-- Copyright   :    (c) Sirui Lu 2024
-- License     :    BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :    siruilu@cs.washington.edu
-- Stability   :    experimental
-- Portability :    GHC only
module Grisette.Lib.Synth.Context
  ( MonadContext,
    ConcreteContext,
    SymbolicContext,
    AngelicContext,
  )
where

import Control.Monad.Except (ExceptT, MonadError)
import qualified Data.Text as T
import Grisette (FreshT, TryMerge, UnionM)

type MonadContext ctx = (MonadError T.Text ctx, TryMerge ctx)

-- | A concrete context is a context that does not do multi-path symbolic
-- execution.
type ConcreteContext = Either T.Text

-- | A symbolic context is a context that does multi-path symbolic execution.
type SymbolicContext = ExceptT T.Text UnionM

-- | An angelic context is a context that does multi-path symbolic execution
-- with angelic choices.
type AngelicContext = FreshT (ExceptT T.Text UnionM)
