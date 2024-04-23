module Grisette.Lib.Synth.Reasoning.SynthesisServerTest
  ( synthesisServerTest,
  )
where

import Grisette.Lib.Synth.Reasoning.SynthesisServer.TaskQueueTest
  ( taskQueueTest,
  )
import Test.Framework (Test, testGroup)

synthesisServerTest :: Test
synthesisServerTest = testGroup "SynthesisServer" [taskQueueTest]
