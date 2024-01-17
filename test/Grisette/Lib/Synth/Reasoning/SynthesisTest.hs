module Grisette.Lib.Synth.Reasoning.SynthesisTest (synthesisTest) where

import Grisette.Lib.Synth.Reasoning.Synthesis.ByteCodeSketchTest
  ( byteCodeSketchTest,
  )
import Grisette.Lib.Synth.Reasoning.Synthesis.ComponentSketchTest
  ( componentSketchTest,
  )
import Test.Framework (Test, testGroup)

synthesisTest :: Test
synthesisTest =
  testGroup
    "Grisette.Lib.Synth.Reasoning.Synthesis"
    [ byteCodeSketchTest,
      componentSketchTest
    ]
