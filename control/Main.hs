{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified ConProg as C
import Data.GraphViz.Printing (PrintDot (toDot), renderDot)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Grisette
  ( Fresh,
    PPrint (pformat),
    Solvable (con),
    SymBool,
    SymInteger,
    runFresh,
    z3,
  )
import Grisette.Lib.Synth.Context (ConcreteContext)
import Grisette.Lib.Synth.Operator.OpSemantics (DefaultSem (DefaultSem))
import qualified Grisette.Lib.Synth.Program.ComponentSketch as Component
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.ProgSemantics (runSymbol)
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable (SymbolTable))
import Grisette.Lib.Synth.Reasoning.Fuzzing
  ( defaultSemQuickCheckFuzzer,
  )
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( SynthesisResult (SynthesisSuccess),
    SynthesisTask
      ( SynthesisTask,
        synthesisExtraConstraints,
        synthesisInitialExamples,
        synthesisPrecondition,
        synthesisSketchSymbol,
        synthesisSketchTable,
        synthesisVerifiers
      ),
    runSynthesisTask,
  )
import Grisette.Lib.Synth.TypeSignature (TypeSignature (TypeSignature))
import qualified Sketch as S
import Test.QuickCheck (Arbitrary (arbitrary), Gen, vectorOf)
import Typing (Type (IntType))
import Value (SymValue, Value (IntValue))

type ConVal = Value Integer Bool

type ConResult = ConcreteContext [ConVal]

type ConProg = C.Prog Integer Integer

type SymVal = SymValue SymInteger SymBool

type Sketch = S.Prog SymInteger SymInteger

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
conProg :: SymbolTable ConProg
conProg =
  SymbolTable
    [ ( "trueBranch",
        Concrete.buildProg [("a", IntType), ("b", IntType)] $
          \[a, b] ->
            let [plus] = Concrete.node C.Plus 1 [a, b]
             in [(plus, IntType)]
      ),
      ( "falseBranch",
        Concrete.buildProg [("a", IntType), ("b", IntType)] $
          \[a, b] ->
            let [minus] = Concrete.node C.Minus 1 [a, b]
             in [(minus, IntType)]
      ),
      ( "prog",
        Concrete.buildProg [("a", IntType), ("b", IntType)] $
          \[a, b] ->
            let [equals] = Concrete.node C.Equals 1 [a, b]
                [res] =
                  Concrete.node
                    ( C.If
                        (TypeSignature [IntType, IntType] [IntType])
                        "trueBranch"
                        "falseBranch"
                    )
                    1
                    [equals, a, b]
             in [(res, IntType)]
      )
    ]

trueBranchSketch :: Fresh (T.Text, Sketch)
trueBranchSketch =
  ("trueBranch",)
    <$> Component.mkSimpleFreshProg
      [IntType, IntType]
      [ S.Plus,
        S.Minus
      ]
      [IntType]

falseBranchSketch :: Fresh (T.Text, Sketch)
falseBranchSketch =
  ("falseBranch",)
    <$> Component.mkSimpleFreshProg
      [IntType, IntType]
      [S.Plus, S.Minus]
      [IntType]

sketch :: T.Text -> T.Text -> Fresh (T.Text, Sketch)
sketch t f = do
  sk <-
    Component.mkFreshProg
      [IntType, IntType]
      [ Component.simpleFreshStmt S.Plus,
        Component.simpleFreshStmt S.Plus,
        Component.simpleFreshStmt S.Equals,
        Component.simpleFreshStmt $
          S.If (TypeSignature [IntType, IntType] [IntType]) t f
      ]
      [IntType]
  return ("sketch", sk)

sketchSymbol :: T.Text
sketchSpace :: SymbolTable Sketch
(sketchSymbol, sketchSpace) = flip runFresh "sketch" $ do
  (ts, t) <- trueBranchSketch
  (fs, f) <- falseBranchSketch
  (ps, p) <- sketch ts fs
  return (ps, SymbolTable [(ts, t), (fs, f), (ps, p)])

spec :: [ConVal] -> ConcreteContext [ConVal]
spec [IntValue a, IntValue b] = Right [IntValue $ if a == b then a + b else a - b]
spec _ = undefined

gen :: Gen [ConVal]
gen = vectorOf 2 $ IntValue <$> arbitrary

main :: IO ()
main = do
  print $ pformat conProg
  let a = runSymbol DefaultSem conProg "prog" [IntValue 2, IntValue 2] :: ConResult
  print a
  let b = runSymbol DefaultSem conProg "prog" [IntValue 1, IntValue 2] :: ConResult
  print b
  let task =
        SynthesisTask
          { synthesisVerifiers = [defaultSemQuickCheckFuzzer @SymVal gen spec],
            synthesisInitialExamples = [],
            synthesisSketchTable = sketchSpace,
            synthesisSketchSymbol = sketchSymbol,
            synthesisPrecondition = con True,
            synthesisExtraConstraints = const $ return $ con True
          }
  r <- runSynthesisTask z3 task
  case r of
    SynthesisSuccess (table :: SymbolTable ConProg) -> do
      print $ pformat table
      writeFile "/tmp/control.dot" $ TL.unpack $ renderDot $ toDot table
      print $ spec [IntValue 5, IntValue 5]
      print (runSymbol DefaultSem table sketchSymbol [IntValue 5, IntValue 5] :: ConResult)
      print $ spec [IntValue 5, IntValue 4]
      print (runSymbol DefaultSem table sketchSymbol [IntValue 5, IntValue 4] :: ConResult)
    _ -> print r
