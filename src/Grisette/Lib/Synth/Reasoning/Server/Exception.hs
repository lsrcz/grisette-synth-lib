module Grisette.Lib.Synth.Reasoning.Server.Exception
  ( SynthesisTaskException (..),
  )
where

import qualified Control.Exception as C

data SynthesisTaskException
  = SynthesisTaskCancelled
  | SynthesisTaskTimeout
  | SynthesisTaskSolverDead
  | SynthesisTaskIndexOutOfBounds
  deriving (Eq, Show)

instance C.Exception SynthesisTaskException
