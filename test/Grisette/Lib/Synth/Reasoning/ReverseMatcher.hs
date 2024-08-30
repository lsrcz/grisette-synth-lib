{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.Reasoning.ReverseMatcher (ReverseMatcher (..)) where

import Control.DeepSeq (NFData (rnf))
import Grisette (SymBool, SymEq ((.==)))
import Grisette.Lib.Synth.Reasoning.Matcher (Matcher (match))

data ReverseMatcher = ReverseMatcher

instance (Eq a) => Matcher ReverseMatcher Bool a where
  match _ actual expected = actual == reverse expected

instance (SymEq a) => Matcher ReverseMatcher SymBool a where
  match _ actual expected = actual .== reverse expected

instance NFData ReverseMatcher where
  rnf ReverseMatcher = ()
