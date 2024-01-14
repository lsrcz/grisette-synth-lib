module Grisette.Lib.Synth.Program.ConcreteTest (concreteTest) where

import Grisette.Lib.Synth.Program.Concrete.PrettyTest (prettyTest)
import Grisette.Lib.Synth.Program.Concrete.SemanticsTest (semanticsTest)
import Grisette.Lib.Synth.Program.Concrete.TopologicalSortTest
  ( topologicalSortTest,
  )
import Test.Framework (Test, testGroup)

concreteTest :: Test
concreteTest =
  testGroup
    "Grisette.Lib.Synth.Program.Concrete"
    [ prettyTest,
      topologicalSortTest,
      semanticsTest
    ]
