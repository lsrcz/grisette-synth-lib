{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Lib.Synth.Reasoning.IOPair (IOPair (..)) where

import Grisette (allClasses01, deriveGADT)

data IOPair val = IOPair {ioPairInputs :: [val], ioPairOutputs :: [val]}

deriveGADT [''IOPair] allClasses01
