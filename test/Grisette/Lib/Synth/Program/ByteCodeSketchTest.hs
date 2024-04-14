{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.ByteCodeSketchTest (byteCodeSketchTest) where

import Grisette.Lib.Synth.Program.ByteCodeSketch.ProgUtilTest (progUtilTest)
import Grisette.Lib.Synth.Program.ByteCodeSketch.SemanticsTest (semanticsTest)
import Grisette.Lib.Synth.Program.ByteCodeSketch.ToConTest (toConTest)
import Grisette.Lib.Synth.Program.ByteCodeSketch.TypingTest (typingTest)
import Test.Framework (Test, testGroup)

byteCodeSketchTest :: Test
byteCodeSketchTest =
  testGroup
    "Grisette.Lib.Synth.Program.ByteCodeSketch"
    [ toConTest,
      semanticsTest,
      typingTest,
      progUtilTest
    ]
