{-# LANGUAGE FunctionalDependencies #-}

module Grisette.Lib.Synth.Program.SubProg (HasSubProg (..)) where

class HasSubProg op subProg ctx | op -> subProg where
  getSubProg :: op -> ctx [subProg]
