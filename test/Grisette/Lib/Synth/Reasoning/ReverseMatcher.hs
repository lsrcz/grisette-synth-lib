{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.Reasoning.ReverseMatcher (ReverseMatcher (..)) where

import Grisette (SEq ((.==)), SymBool)
import Grisette.Lib.Synth.Reasoning.Matcher (Matcher (match))

data ReverseMatcher = ReverseMatcher

instance (Eq a) => Matcher ReverseMatcher Bool a where
  match _ actual expected = actual == reverse expected

instance (SEq a) => Matcher ReverseMatcher SymBool a where
  match _ actual expected = actual .== reverse expected
