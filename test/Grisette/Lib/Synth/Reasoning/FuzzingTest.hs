{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Reasoning.FuzzingTest (fuzzingTest) where

import Data.Data (Proxy (Proxy))
import Data.Either (fromRight)
import Grisette
  ( Model,
    ModelRep (buildModel),
    ModelValuePair ((::=)),
    SymInteger,
  )
import Grisette.Lib.Synth.Context (ConcreteContext)
import qualified Grisette.Lib.Synth.Program.ByteCodeSketch as ByteCodeSketch
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg), evalSymbolTable)
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable (SymbolTable))
import Grisette.Lib.Synth.Reasoning.Fuzzing
  ( fuzzingTestProg,
    fuzzingTestSymProgWithModel,
  )
import Grisette.Lib.Synth.Reasoning.IOPair (IOPair (IOPair))
import Grisette.Lib.Synth.Reasoning.Matcher (EqMatcher (EqMatcher))
import Grisette.Lib.Synth.Reasoning.ReverseMatcher
  ( ReverseMatcher (ReverseMatcher),
  )
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsObj (TestSemanticsObj),
    TestSemanticsOp (Add, DivMod),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?), (@?=))
import Test.QuickCheck (Arbitrary (arbitrary), Gen, NonZero (NonZero))

type ConProgType = Concrete.Prog TestSemanticsOp Integer TestSemanticsType

conProg :: ConProgType
conProg =
  Concrete.Prog
    [ Concrete.ProgArg "x" 0 IntType,
      Concrete.ProgArg "y" 1 IntType
    ]
    [ Concrete.Stmt Add [0, 1] [3],
      Concrete.Stmt DivMod [3, 0] [4, 5]
    ]
    [Concrete.ProgRes 4 IntType, Concrete.ProgRes 5 IntType]

conProgTable :: SymbolTable ConProgType
conProgTable = SymbolTable [("test", conProg)]

type SymProgType =
  ByteCodeSketch.Prog TestSemanticsOp Integer SymInteger TestSemanticsType

symProg :: SymProgType
symProg =
  ByteCodeSketch.Prog
    [ ByteCodeSketch.ProgArg "x" 0 IntType,
      ByteCodeSketch.ProgArg "y" 1 IntType
    ]
    [ ByteCodeSketch.Stmt Add [0, 1] 2 [3] 1,
      ByteCodeSketch.Stmt DivMod [3, "x"] 2 [4, 5] 2
    ]
    [ByteCodeSketch.ProgRes 4 IntType, ByteCodeSketch.ProgRes 5 IntType]

symProgTable :: SymbolTable SymProgType
symProgTable = SymbolTable [("test", symProg)]

model :: Model
model = buildModel ("x" ::= (0 :: Integer))

gen :: Gen [Integer]
gen = do
  NonZero x <- arbitrary
  y <- arbitrary
  return [x, y]

spec :: [Integer] -> ConcreteContext ([Integer], EqMatcher)
spec [x, y] | x /= 0 = return ([(x + y) `div` x, (x + y) `mod` x], EqMatcher)
spec _ = error "Error"

badSpec :: [Integer] -> ConcreteContext ([Integer], EqMatcher)
badSpec [_, _] = return ([0, 0], EqMatcher)
badSpec _ = error "Error"

reverseSpec :: [Integer] -> ConcreteContext ([Integer], ReverseMatcher)
reverseSpec [x, y]
  | x /= 0 = return ([(x + y) `mod` x, (x + y) `div` x], ReverseMatcher)
reverseSpec _ = error "Error"

fuzzingTest :: Test
fuzzingTest =
  testGroup
    "Grisette.Lib.Synth.Reasoning.Fuzzing"
    [ testGroup
        "fuzzingTestProg"
        [ testCase "goodSpec" $ do
            result <-
              fuzzingTestProg
                gen
                spec
                100
                (evalSymbolTable TestSemanticsObj conProgTable)
                "test"
            fst <$> result @?= Nothing,
          testCase "reverseSpec" $ do
            result <-
              fuzzingTestProg
                gen
                reverseSpec
                100
                (evalSymbolTable TestSemanticsObj conProgTable)
                "test"
            fst <$> result @?= Nothing,
          testCase "badSpec" $ do
            Just (IOPair i o, _) <-
              fuzzingTestProg
                gen
                badSpec
                100
                (evalSymbolTable TestSemanticsObj conProgTable)
                "test"
            fst (fromRight (error "err") $ badSpec i) @?= o
            (runProg TestSemanticsObj mempty conProg i /= Right o)
              @? "Should fail the test."
        ],
      testGroup
        "fuzzingTestSymProgWithModel"
        [ testCase "goodSpec" $ do
            result <-
              fuzzingTestSymProgWithModel
                gen
                spec
                100
                (Proxy :: Proxy ConProgType)
                TestSemanticsObj
                symProgTable
                "test"
                model
            fst <$> result @?= Nothing,
          testCase "badSpec" $ do
            Just (IOPair i o, _) <-
              fuzzingTestSymProgWithModel
                gen
                badSpec
                100
                (Proxy :: Proxy ConProgType)
                TestSemanticsObj
                symProgTable
                "test"
                model
            fst (fromRight (error "err") $ badSpec i) @?= o
            (runProg TestSemanticsObj mempty conProg i /= Right o)
              @? "Should fail the test."
        ]
    ]
