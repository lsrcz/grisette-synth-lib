module Grisette.Lib.Synth.Program.ConcreteTest (concreteTest) where

import Grisette.Lib.Synth.Program.Concrete.PrettyTest (prettyTest)
import Test.Framework (Test, testGroup)

concreteTest :: Test
concreteTest =
  testGroup
    "Grisette.Lib.Synth.Program.Concrete"
    [ prettyTest
    ]
