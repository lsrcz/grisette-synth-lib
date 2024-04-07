module Main (main) where

import Grisette.Lib.Synth.Operator.OpPrettyTest (opPrettyTest)
import Grisette.Lib.Synth.Operator.OpSemanticsTest (opSemanticsTest)
import Grisette.Lib.Synth.Operator.OpToDotTest (opToDotTest)
import Grisette.Lib.Synth.Operator.OpTypingTest (opTypingTest)
import Grisette.Lib.Synth.Program.BuiltinProgConstraintsTest
  ( builtinProgConstraintsTest,
  )
import Grisette.Lib.Synth.Program.ByteCodeSketchTest (byteCodeSketchTest)
import Grisette.Lib.Synth.Program.ComponentSketchTest (componentSketchTest)
import Grisette.Lib.Synth.Program.ConcreteTest (concreteTest)
import Grisette.Lib.Synth.Reasoning.FuzzingTest (fuzzingTest)
import Grisette.Lib.Synth.Reasoning.SynthesisTest (synthesisTest)
import Grisette.Lib.Synth.Util.PrettyTest (prettyTest)
import Grisette.Lib.Synth.Util.ShowTest (showTest)
import Test.Framework (Test, defaultMain)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ prettyTest,
    showTest,
    opPrettyTest,
    opToDotTest,
    opTypingTest,
    opSemanticsTest,
    concreteTest,
    byteCodeSketchTest,
    componentSketchTest,
    fuzzingTest,
    synthesisTest,
    builtinProgConstraintsTest
  ]
