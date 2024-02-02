{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified ConProg as C
import Grisette (GPretty (gpretty), SymBool, SymInteger, precise, z3)
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
import Semantics (Sem (Sem))
import qualified Sketch as S
import Test.QuickCheck (Arbitrary (arbitrary), Gen, vectorOf)
import Typing (Type (IntType))
import Value (SymValue, Value (IntValue))

type ConVal = Value Integer Bool

type ConResult = ConcreteContext [ConVal]

type ConProg = C.Prog Integer Integer

type SymVal = SymValue SymInteger SymBool

type Sketch = S.Prog SymInteger SymInteger

trueBranch :: ConProg
trueBranch =
  Concrete.Prog
    "trueBranch"
    [ Concrete.ProgArg IntType "a" 0,
      Concrete.ProgArg IntType "b" 1
    ]
    [ Concrete.Stmt C.Plus [0, 1] [2]
    ]
    [Concrete.ProgRes IntType 2]

falseBranch :: ConProg
falseBranch =
  Concrete.Prog
    "falseBranch"
    [ Concrete.ProgArg IntType "a" 0,
      Concrete.ProgArg IntType "b" 1
    ]
    [ Concrete.Stmt C.Minus [0, 1] [2]
    ]
    [Concrete.ProgRes IntType 2]

-- We can use the following encoding:
--
-- def trueBranch(a: int, b: int):
--   r2 = plus(a, b)
--   return r2
-- def falseBranch(a: int, b: int):
--   r2 = Minus(a, b)
--   return r2
-- def prog(a: int, b: int):
--   r2 = equals(a, b)
--   r3 = if(trueBranch, falseBranch)(cond=r2, a, b)
--   return r3
--
-- to model the following program:
--
-- def prog(a: int, b: int):
--   r2 = equals(a, b)
--   if r2:
--     r3 = plus(a, b)
--   else:
--     r3 = Minus(a, b)
--   return r3
conProg :: ConProg
conProg =
  Concrete.Prog
    "prog"
    [ Concrete.ProgArg IntType "a" 0,
      Concrete.ProgArg IntType "b" 1
    ]
    [ Concrete.Stmt C.Equals [0, 1] [2],
      Concrete.Stmt
        (C.If trueBranch falseBranch)
        [2, 0, 1]
        [3]
    ]
    [Concrete.ProgRes IntType 3]

trueBranchSketch :: Sketch
trueBranchSketch =
  Component.Prog
    "trueBranch"
    [ Component.ProgArg IntType "a",
      Component.ProgArg IntType "b"
    ]
    [ Component.Stmt
        S.Plus
        ["true'stmt0'arg0", "true'stmt0'arg1"]
        ["true'stmt0'ret0"]
        "true'stmt0'dis",
      Component.Stmt
        S.Minus
        ["true'stmt1'arg0", "true'stmt1'arg1"]
        ["true'stmt1'ret0"]
        "true'stmt1'dis"
    ]
    [Component.ProgRes IntType "true'ret0"]

falseBranchSketch :: Sketch
falseBranchSketch =
  Component.Prog
    "falseBranch"
    [ Component.ProgArg IntType "a",
      Component.ProgArg IntType "b"
    ]
    [ Component.Stmt
        S.Plus
        ["false'stmt0'arg0", "false'stmt0'arg1"]
        ["false'stmt0'ret0"]
        "false'stmt0'dis",
      Component.Stmt
        S.Minus
        ["false'stmt1'arg0", "false'stmt1'arg1"]
        ["false'stmt1'ret0"]
        "false'stmt1'dis"
    ]
    [Component.ProgRes IntType "false'ret0"]

sketch :: Sketch
sketch =
  Component.Prog
    "prog"
    [ Component.ProgArg IntType "a",
      Component.ProgArg IntType "b"
    ]
    [ Component.Stmt
        S.Plus
        ["stmt0'arg0", "stmt0'arg1"]
        ["stmt0'ret0"]
        "stmt0'dis",
      Component.Stmt
        S.Plus
        ["stmt1'arg0", "stmt1'arg1"]
        ["stmt1'ret0"]
        "stmt1'dis",
      Component.Stmt
        S.Equals
        ["stmt2'arg0", "stmt2'arg1"]
        ["stmt2'ret0"]
        "stmt2'dis",
      Component.Stmt
        (S.If trueBranchSketch falseBranchSketch)
        ["stmt3'arg0", "stmt3'arg1", "stmt3'arg2"]
        ["stmt3'ret0"]
        "stmt3'dis"
    ]
    [Component.ProgRes IntType "ret0"]

spec :: [ConVal] -> [ConVal]
spec [IntValue a, IntValue b] = [IntValue $ if a == b then a + b else a - b]
spec _ = undefined

gen :: Gen [ConVal]
gen = vectorOf 2 $ IntValue <$> arbitrary

main :: IO ()
main = do
  print $ gpretty conProg
  let a = runProg Sem conProg [IntValue 2, IntValue 2] :: ConResult
  print a
  let b = runProg Sem conProg [IntValue 1, IntValue 2] :: ConResult
  print b

  let task ::
        SynthesisWithFuzzerTask ConVal SymVal ConProg Sketch AngelicContext
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
      print $ gpretty prog
      print $ spec [IntValue 5, IntValue 5]
      print (runProg Sem prog [IntValue 5, IntValue 5] :: ConResult)
      print $ spec [IntValue 5, IntValue 4]
      print (runProg Sem prog [IntValue 5, IntValue 4] :: ConResult)
    _ -> print r
