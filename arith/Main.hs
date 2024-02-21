{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Arith (OpCode (Minus, Mul, Plus), OpType (IntegerType), Sem (Sem))
import Grisette (GPretty (gpretty), SymInteger, precise, runFresh, z3)
import Grisette.Lib.Synth.Context (AngelicContext, ConcreteContext)
import qualified Grisette.Lib.Synth.Program.ComponentSketch as Component
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Reasoning.Fuzzing
  ( SynthesisWithFuzzerTask
      ( SynthesisWithFuzzerTask,
        synthesisWithFuzzerTaskGenerators,
        synthesisWithFuzzerTaskMaxTests,
        synthesisWithFuzzerTaskSemantics,
        synthesisWithFuzzerTaskSolverConfig,
        synthesisWithFuzzerTaskSpec,
        synthesisWithFuzzerTaskSymProg
      ),
  )
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( SynthesisResult (SynthesisSuccess),
    synthesizeProgWithVerifier,
  )
import Test.QuickCheck (Arbitrary (arbitrary), Gen, vectorOf)

type ConProg = Concrete.Prog OpCode Integer OpType

type Sketch = Component.Prog OpCode SymInteger OpType

-- The sketch is currently all manually constructed for the purpose of showing
-- the structure of a sketch. The symbolic constants are assigned with unique
-- names manually (e.g. @stmt1'arg0@).
--
-- A generator for a sketch with components is under development.
sketch :: Sketch
sketch =
  flip runFresh "x" $
    Component.mkFreshProg
      -- The name of the program. If your program supports procedure calls, you
      -- should make sure that the programs has a unique name.
      "test"
      -- The types of the arguments to the program.
      [IntegerType, IntegerType]
      -- The components of the program. Each component is a statement.
      --
      -- The synthesizer could
      -- \* reorder the components, and
      -- \* choose whether or now to disable a component, and
      -- \* choose the arguments of a component.
      [ Component.mkFreshStmt (return Minus) 2 1, -- 2 inputs, 1 output
        Component.mkFreshStmt (return Mul) 2 1,
        Component.mkFreshStmt (return Plus) 2 1
      ]
      -- The program result type.
      [IntegerType]

-- The specification specifies the expected behavior. Here, we want to synthesis
-- a program that computes the expression a * (a + b) - b.
spec :: [Integer] -> [Integer]
spec [a, b] = [a * (a + b) - b]
spec _ = undefined

-- The generator generates concrete inputs to fuzz the synthesized program
-- against the specification.
gen :: Gen [Integer]
gen = vectorOf 2 arbitrary

main :: IO ()
main = do
  let task ::
        SynthesisWithFuzzerTask Integer SymInteger ConProg Sketch AngelicContext
      task =
        SynthesisWithFuzzerTask
          { synthesisWithFuzzerTaskSymProg = sketch,
            synthesisWithFuzzerTaskSpec = spec,
            -- You need a working z3 installation available in your PATH.
            synthesisWithFuzzerTaskSolverConfig = precise z3,
            synthesisWithFuzzerTaskSemantics = Sem,
            synthesisWithFuzzerTaskMaxTests = 100,
            synthesisWithFuzzerTaskGenerators = [gen]
          }
  (_, r) <- synthesizeProgWithVerifier task
  case r of
    SynthesisSuccess prog -> do
      -- def test(x: int, y: int):
      --   r2 = plus(lhs=y, rhs=x)
      --   r3 = mul(lhs=r2, rhs=x)
      --   r4 = minus(lhs=r3, rhs=y)
      --   return r4
      print $ gpretty prog
      let input = [5, 20]
      print $ spec input
      print (runProg Sem prog input :: ConcreteContext [Integer])
    _ -> print r
