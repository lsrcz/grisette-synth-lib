{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Arith (OpCode (Minus, Mul, Plus), OpType (IntegerType), Sem (Sem))
import Grisette (GPretty (gpretty), SymInteger, precise, z3)
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
  Component.Prog
    -- The name of the program. If your program supports procedure calls, you
    -- should make sure that the programs has a unique name.
    "test"
    -- The arguments to the program with types. "x" and "y" are names of the
    -- arguments and is only used for pretty printing purpose for now.
    [ Component.ProgArg IntegerType "x",
      Component.ProgArg IntegerType "y"
    ]
    -- The components of the program. Each component is a statement.
    --
    -- The synthesizer could
    -- \* reorder the components, and
    -- \* choose whether or now to disable a component, and
    -- \* choose the arguments of a component.
    [ Component.Stmt
        Minus
        -- These are the symbolic constants controlling the arguments of the
        -- statement.
        ["stmt0'arg0", "stmt0'arg1"]
        -- This is a symbolic constants controling the reordering of the
        -- components.
        --
        -- The components would be sorted based on them. Each component can only
        -- refer to the outputs of the components defined before.
        ["stmt0'ret0"]
        -- This is a symbolic boolean constant controlling whether a statement
        -- is disabled.
        "stmt0'dis",
      Component.Stmt
        Mul
        ["stmt1'arg0", "stmt1'arg1"]
        ["stmt1'ret0"]
        "stmt1'dis",
      Component.Stmt
        Plus
        ["stmt2'arg0", "stmt2'arg1"]
        ["stmt2'ret0"]
        "stmt2'dis"
    ]
    -- The program result. Use the symbolic constant @r@ to choose which
    -- statement is used as the result.
    [Component.ProgRes IntegerType "r"]

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
