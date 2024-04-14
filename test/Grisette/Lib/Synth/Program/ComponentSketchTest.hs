{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.ComponentSketchTest
  ( componentSketchTest,
  )
where

import Grisette.Lib.Synth.Program.ComponentSketch (Prog (Prog))
import Grisette.Lib.Synth.Program.ComponentSketch.BuilderTest (builderTest)
import Grisette.Lib.Synth.Program.ComponentSketch.GenIntermediateTest
  ( genIntermediateTest,
  )
import Grisette.Lib.Synth.Program.ComponentSketch.ProgUtilTest (progUtilTest)
import Grisette.Lib.Synth.Program.ComponentSketch.SemanticsTest (semanticsTest)
import Grisette.Lib.Synth.Program.ComponentSketch.ToConTest (toConTest)
import Grisette.Lib.Synth.Program.ComponentSketch.ToSymTest (toSymTest)
import Grisette.Lib.Synth.Program.ComponentSketch.TypingTest (typingTest)
import Grisette.Lib.Synth.Program.ProgNaming (ProgNaming (nameProg))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

componentSketchTest :: Test
componentSketchTest =
  testGroup
    "Grisette.Lib.Synth.Program.ComponentSketch"
    [ genIntermediateTest,
      toConTest,
      toSymTest,
      semanticsTest,
      typingTest,
      builderTest,
      progUtilTest,
      testGroup
        "Misc"
        [ testCase "ProgNaming" $
            nameProg (Prog "test" [] [] []) @?= "test"
        ]
    ]
