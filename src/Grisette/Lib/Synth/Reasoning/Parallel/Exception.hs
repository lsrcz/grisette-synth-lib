{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Lib.Synth.Reasoning.Parallel.Exception
  ( SynthesisTaskException (..),
  )
where

import qualified Control.Exception as C
import GHC.Generics (Generic)
import Grisette (allClasses0, deriveGADT)

data SynthesisTaskException
  = SynthesisTaskCancelled
  | SynthesisTaskTimeout
  | SynthesisTaskSolverDead
  | SynthesisTaskIndexOutOfBounds
  deriving (Generic)

deriveGADT [''SynthesisTaskException] allClasses0

instance C.Exception SynthesisTaskException
