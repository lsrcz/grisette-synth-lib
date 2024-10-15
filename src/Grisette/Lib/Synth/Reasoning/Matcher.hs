{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.Reasoning.Matcher
  ( Matcher (..),
    EqMatcher (..),
  )
where

import Control.DeepSeq (NFData (rnf))
import GHC.Generics (Generic)
import Grisette (Default (Default), EvalSym, Mergeable, SymBool, SymEq ((.==)))

class Matcher matcher bool a where
  match :: matcher -> [a] -> [a] -> bool

data EqMatcher = EqMatcher
  deriving (Eq, Show, Generic)
  deriving (Mergeable, EvalSym) via (Default EqMatcher)

instance NFData EqMatcher where
  rnf EqMatcher = ()

instance (SymEq a) => Matcher EqMatcher SymBool a where
  match _ = (.==)

instance (Eq a) => Matcher EqMatcher Bool a where
  match _ = (==)
