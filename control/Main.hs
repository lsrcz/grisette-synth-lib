{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified ConProg as C
import Data.GraphViz.Printing (PrintDot (toDot), renderDot)
import qualified Data.Text.Lazy as TL
import Grisette
  ( Fresh,
    PPrint (pformat),
    SymBool,
    SymInteger,
    z3,
  )
import Grisette.Lib.Synth.Context (ConcreteContext)
import Grisette.Lib.Synth.Operator.OpSemantics (DefaultSem (DefaultSem))
import qualified Grisette.Lib.Synth.Program.ComponentSketch as Component
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Reasoning.Fuzzing
  ( defaultSemQuickCheckFuzzer,
  )
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( SynthesisResult (SynthesisSuccess),
    SynthesisTask
      ( SynthesisTask,
        synthesisSketch,
        synthesisVerifiers
      ),
    runSynthesisTask,
  )
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
trueBranch = Concrete.buildProg "trueBranch" [("a", IntType), ("b", IntType)] $
  \[a, b] ->
    let [plus] = Concrete.node C.Plus 1 [a, b]
     in [(plus, IntType)]

falseBranch :: ConProg
falseBranch = Concrete.buildProg "trueBranch" [("a", IntType), ("b", IntType)] $
  \[a, b] ->
    let [minus] = Concrete.node C.Minus 1 [a, b]
     in [(minus, IntType)]

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
conProg = Concrete.buildProg "trueBranch" [("a", IntType), ("b", IntType)] $
  \[a, b] ->
    let [equals] = Concrete.node C.Equals 1 [a, b]
        [res] = Concrete.node (C.If trueBranch falseBranch) 1 [equals, a, b]
     in [(res, IntType)]

trueBranchSketch :: Fresh Sketch
trueBranchSketch =
  Component.mkSimpleFreshProg
    "trueBranch"
    [IntType, IntType]
    [ S.Plus,
      S.Minus
    ]
    [IntType]

falseBranchSketch :: Fresh Sketch
falseBranchSketch =
  Component.mkSimpleFreshProg
    "falseBranch"
    [IntType, IntType]
    [S.Plus, S.Minus]
    [IntType]

sketch :: Sketch
sketch =
  Component.mkSketch
    "prog"
    [IntType, IntType]
    [ Component.simpleFreshStmt S.Plus,
      Component.simpleFreshStmt S.Plus,
      Component.simpleFreshStmt S.Equals,
      Component.freshStmt $ do
        trueBranch <- trueBranchSketch
        S.If trueBranch <$> falseBranchSketch
    ]
    [IntType]

spec :: [ConVal] -> [ConVal]
spec [IntValue a, IntValue b] = [IntValue $ if a == b then a + b else a - b]
spec _ = undefined

gen :: Gen [ConVal]
gen = vectorOf 2 $ IntValue <$> arbitrary

main :: IO ()
main = do
  print $ pformat conProg
  let a = runProg DefaultSem conProg [IntValue 2, IntValue 2] :: ConResult
  print a
  let b = runProg DefaultSem conProg [IntValue 1, IntValue 2] :: ConResult
  print b
  let task =
        SynthesisTask
          { synthesisVerifiers = [defaultSemQuickCheckFuzzer @SymVal gen spec],
            synthesisSketch = sketch
          }
  r <- runSynthesisTask z3 task
  case r of
    SynthesisSuccess (prog :: ConProg) -> do
      print $ pformat prog
      writeFile "/tmp/control.dot" $ TL.unpack $ renderDot $ toDot prog
      print $ spec [IntValue 5, IntValue 5]
      print (runProg DefaultSem prog [IntValue 5, IntValue 5] :: ConResult)
      print $ spec [IntValue 5, IntValue 4]
      print (runProg DefaultSem prog [IntValue 5, IntValue 4] :: ConResult)
    _ -> print r
