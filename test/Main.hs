module Main (main) where

import Grisette.Lib.Synth.ContextTest (contextTest)
import Grisette.Lib.Synth.Operator.OpPrettyTest (opPrettyTest)
import Grisette.Lib.Synth.Util.PrettyTest (prettyTest)
import Grisette.Lib.Synth.Util.ShowTest (showTest)
import Test.Framework (Test, defaultMain)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ contextTest,
    prettyTest,
    showTest,
    opPrettyTest
  ]
