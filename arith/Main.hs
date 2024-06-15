{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Arith (OpCode (Minus, Mul, Plus))
import Data.GraphViz.Printing (PrintDot (toDot), renderDot)
import qualified Data.Text.Lazy as TL
import Grisette (GPretty (gpretty), SymInteger, precise, runFresh, z3)
import Grisette.Lib.Synth.Context (AngelicContext, ConcreteContext)
import Grisette.Lib.Synth.Operator.OpSemantics (DefaultSem (DefaultSem))
import Grisette.Lib.Synth.Operator.OpTyping (DefaultType (DefaultType))
import qualified Grisette.Lib.Synth.Program.ComponentSketch as Component
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.ProgConstraints
  ( WithConstraints (WithConstraints),
  )
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Reasoning.Fuzzing
  ( QuickCheckFuzzer
      ( QuickCheckFuzzer,
        quickCheckFuzzerConSemantics,
        quickCheckFuzzerGenerators,
        quickCheckFuzzerMaxTests,
        quickCheckFuzzerSpec,
        quickCheckFuzzerSymSemantics
      ),
  )
import Grisette.Lib.Synth.Reasoning.Matcher (EqMatcher (EqMatcher))
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( SomeVerifier (SomeVerifier),
    SynthesisResult (SynthesisSuccess),
    SynthesisTask
      ( SynthesisTask,
        synthesisTaskSymProg,
        synthesisTaskVerifiers
      ),
    runSynthesisTask,
  )
import Test.QuickCheck (Arbitrary (arbitrary), Gen, vectorOf)

type ConProg = Concrete.Prog OpCode Integer DefaultType

type Sketch = Component.Prog OpCode SymInteger DefaultType

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
      [DefaultType, DefaultType]
      -- The components of the program. Each component is a statement.
      --
      -- The synthesizer could
      -- \* reorder the components, and
      -- \* choose whether or now to disable a component, and
      -- \* choose the arguments of a component.
      [ Component.simpleFreshStmt Minus,
        Component.simpleFreshStmt Mul,
        Component.simpleFreshStmt Plus
      ]
      -- The program result type.
      [DefaultType]

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
  let verifier =
        QuickCheckFuzzer
          { quickCheckFuzzerSymSemantics = WithConstraints DefaultSem (),
            quickCheckFuzzerConSemantics = DefaultSem,
            quickCheckFuzzerMaxTests = 100,
            quickCheckFuzzerGenerators = [gen],
            quickCheckFuzzerSpec = (,EqMatcher) . spec
          } ::
          QuickCheckFuzzer Sketch ConProg SymInteger Integer AngelicContext
  let task =
        SynthesisTask
          { synthesisTaskVerifiers = [SomeVerifier verifier],
            synthesisTaskSymProg = sketch
          }
  print sketch
  r <- runSynthesisTask (precise z3) task
  case r of
    SynthesisSuccess (prog :: ConProg) -> do
      -- def test(x: int, y: int):
      --   r2 = plus(lhs=y, rhs=x)
      --   r3 = mul(lhs=r2, rhs=x)
      --   r4 = minus(lhs=r3, rhs=y)
      --   return r4
      print $ gpretty prog
      writeFile "/tmp/arith.dot" $ TL.unpack $ renderDot $ toDot prog
      let input = [5, 20]
      print $ spec input
      print (runProg DefaultSem prog input :: ConcreteContext [Integer])
    _ -> print r
