module Main (main) where

import Grisette.Lib.Synth.ContextTest (contextTest)
import Grisette.Lib.Synth.Util.PrettyTest (prettyTest)
import Test.Framework (Test, defaultMain)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ contextTest,
    prettyTest
  ]
