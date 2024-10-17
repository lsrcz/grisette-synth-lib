module Grisette.Lib.Synth.Program.ConcreteTest (concreteTest) where

import Grisette.Lib.Synth.Program.Concrete.BuilderTest (builderTest)
import Grisette.Lib.Synth.Program.Concrete.EliminateDeadCodeTest
  ( eliminateDeadCodeTest,
  )
import Grisette.Lib.Synth.Program.Concrete.FlattenTest (flattenTest)
import Grisette.Lib.Synth.Program.Concrete.MayMultiPathTest (mayMultiPathTest)
import Grisette.Lib.Synth.Program.Concrete.PrettyTest (prettyTest)
import Grisette.Lib.Synth.Program.Concrete.ProgCostTest (progCostTest)
import Grisette.Lib.Synth.Program.Concrete.ProgUtilTest (progUtilTest)
import Grisette.Lib.Synth.Program.Concrete.SemanticsTest (semanticsTest)
import Grisette.Lib.Synth.Program.Concrete.ToDotTest (toDotTest)
import Grisette.Lib.Synth.Program.Concrete.TypingTest (typingTest)
import Test.Framework (Test, testGroup)

concreteTest :: Test
concreteTest =
  testGroup
    "Grisette.Lib.Synth.Program.Concrete"
    [ prettyTest,
      toDotTest,
      -- topologicalSortTest,
      semanticsTest,
      typingTest,
      mayMultiPathTest,
      builderTest,
      progUtilTest,
      flattenTest,
      progCostTest,
      eliminateDeadCodeTest
    ]
