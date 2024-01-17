{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Reasoning.FuzzingTest (fuzzingTest) where

import Data.Data (Proxy (Proxy))
import Grisette
  ( Model,
    ModelRep (buildModel),
    ModelValuePair ((::=)),
    SymInteger,
    VerifierResult (CEGISVerifierFoundCex),
    mrgReturn,
  )
import qualified Grisette.Lib.Synth.Program.ByteCodeSketch as ByteCodeSketch
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Reasoning.Fuzzing
  ( fuzzingTestProg,
    fuzzingTestStatefulVerifierFun,
    fuzzingTestSymProgWithModel,
  )
import Grisette.Lib.Synth.Reasoning.IOPair (IOPair (IOPair))
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
    "test"
    [ Concrete.ProgArg IntType "x" 0,
      Concrete.ProgArg IntType "y" 1
    ]
    [ Concrete.Stmt Add [0, 1] [3],
      Concrete.Stmt DivMod [3, 0] [4, 5]
    ]
    [Concrete.ProgRes IntType 4, Concrete.ProgRes IntType 5]

type SymProgType =
  ByteCodeSketch.Prog TestSemanticsOp Integer SymInteger TestSemanticsType

symProg :: SymProgType
symProg =
  ByteCodeSketch.Prog
    "test"
    [ ByteCodeSketch.ProgArg IntType "x" 0,
      ByteCodeSketch.ProgArg IntType "y" 1
    ]
    [ ByteCodeSketch.Stmt (mrgReturn Add) [0, 1] 2 [3] 1,
      ByteCodeSketch.Stmt (mrgReturn DivMod) [3, "x"] 2 [4, 5] 2
    ]
    [ByteCodeSketch.ProgRes IntType 4, ByteCodeSketch.ProgRes IntType 5]

model :: Model
model = buildModel ("x" ::= (0 :: Integer))

gen :: Gen [Integer]
gen = do
  NonZero x <- arbitrary
  y <- arbitrary
  return [x, y]

spec :: [Integer] -> [Integer]
spec [x, y] | x /= 0 = [(x + y) `div` x, (x + y) `mod` x]
spec _ = error "Error"

badSpec :: [Integer] -> [Integer]
badSpec [_, _] = [0, 0]
badSpec _ = error "Error"

fuzzingTest :: Test
fuzzingTest =
  testGroup
    "Grisette.Lib.Synth.Reasoning.Fuzzing"
    [ testGroup
        "fuzzingTestProg"
        [ testCase "goodSpec" $ do
            result <- fuzzingTestProg gen spec 100 TestSemanticsObj conProg
            result @?= Nothing,
          testCase "badSpec" $ do
            Just (IOPair i o) <-
              fuzzingTestProg gen badSpec 100 TestSemanticsObj conProg
            badSpec i @?= o
            (runProg TestSemanticsObj conProg i /= Right o)
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
                symProg
                model
            result @?= Nothing,
          testCase "badSpec" $ do
            Just (IOPair i o) <-
              fuzzingTestSymProgWithModel
                gen
                badSpec
                100
                (Proxy :: Proxy ConProgType)
                TestSemanticsObj
                symProg
                model
            badSpec i @?= o
            (runProg TestSemanticsObj conProg i /= Right o)
              @? "Should fail the test."
        ],
      testCase "fuzzingTestStatefulVerifierFun" $ do
        (newState, CEGISVerifierFoundCex (IOPair i o)) <-
          fuzzingTestStatefulVerifierFun
            badSpec
            100
            (Proxy :: Proxy ConProgType)
            TestSemanticsObj
            symProg
            [return [1, -1], gen]
            model
        length newState @?= 1
        badSpec i @?= o
        (runProg TestSemanticsObj conProg i /= Right o)
          @? "Should fail the test."
    ]
