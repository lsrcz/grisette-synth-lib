{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Lib.Synth.Combinator.Embed ((:<:) (..)) where

class sub :<: sup where
  inj :: sub -> sup
  prj :: sup -> Maybe sub

instance {-# OVERLAPPING #-} a :<: a where
  inj = id
  prj = Just
