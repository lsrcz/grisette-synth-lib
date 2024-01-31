{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Arith (OpCode (Minus, Mul, Plus), OpType (IntegerType), Sem (Sem))
import Grisette (GPretty (gpretty), SymInteger, precise, z3)
import Grisette.Lib.Synth.Context (AngelicContext)
import qualified Grisette.Lib.Synth.Program.ComponentSketch as Component
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Reasoning.Fuzzing (SynthesisWithFuzzerTask (SynthesisWithFuzzerTask, synthesisWithFuzzerTaskGenerators, synthesisWithFuzzerTaskMaxTests, synthesisWithFuzzerTaskSemantics, synthesisWithFuzzerTaskSolverConfig, synthesisWithFuzzerTaskSpec, synthesisWithFuzzerTaskSymProg))
import Grisette.Lib.Synth.Reasoning.Synthesis (SynthesisResult (SynthesisSuccess), synthesizeProgWithVerifier)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, vectorOf)

type ConProg = Concrete.Prog OpCode Integer OpType

type Sketch = Component.Prog OpCode SymInteger OpType

sketch :: Sketch
sketch =
  Component.Prog
    "test"
    [ Component.ProgArg IntegerType "x",
      Component.ProgArg IntegerType "y"
    ]
    [ Component.Stmt
        Plus
        ["stmt0'arg0", "stmt0'arg1"]
        ["stmt0'ret0"]
        "stmt0'dis",
      Component.Stmt
        Mul
        ["stmt1'arg0", "stmt1'arg1"]
        ["stmt1'ret0"]
        "stmt1'dis",
      Component.Stmt
        Minus
        ["stmt2'arg0", "stmt2'arg1"]
        ["stmt2'ret0"]
        "stmt2'dis"
    ]
    [Component.ProgRes IntegerType "r"]

spec :: [Integer] -> [Integer]
spec [a, b] = [a * (a + b) - b]
spec _ = undefined

gen :: Gen [Integer]
gen = vectorOf 2 arbitrary

main :: IO ()
main = do
  let task :: SynthesisWithFuzzerTask Integer SymInteger ConProg Sketch AngelicContext
      task =
        SynthesisWithFuzzerTask
          { synthesisWithFuzzerTaskSymProg = sketch,
            synthesisWithFuzzerTaskSpec = spec,
            synthesisWithFuzzerTaskSolverConfig = precise z3,
            synthesisWithFuzzerTaskSemantics = Sem,
            synthesisWithFuzzerTaskMaxTests = 100,
            synthesisWithFuzzerTaskGenerators = [gen]
          }
  (_, r) <- synthesizeProgWithVerifier task
  case r of
    SynthesisSuccess prog -> print $ gpretty prog
    _ -> print r
