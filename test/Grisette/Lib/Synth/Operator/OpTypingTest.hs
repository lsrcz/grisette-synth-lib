{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Operator.OpTypingTest (opTypingTest) where

import Data.Data (Proxy (Proxy))
import Grisette (Solvable (isym), SymInteger, mrgReturn, runFreshT)
import Grisette.Lib.Synth.Context (SymbolicContext)
import Grisette.Lib.Synth.Program.ComponentSketch
  ( Intermediates (Intermediates),
    OpTypingSimple (typeOpSimple),
    genIntermediates,
    genOpIntermediates,
  )
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsObj (TestSemanticsObj),
    TestSemanticsOp (DivMod),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

opTypingTest :: Test
opTypingTest =
  testGroup
    "Grisette.Lib.Synth.Operator.OpTyping"
    [ testCase "genIntermediates" $ do
        let actual =
              flip runFreshT "x" $
                genIntermediates TestSemanticsObj [IntType, IntType] ::
                SymbolicContext [SymInteger]
        let expected = mrgReturn [isym "x" 0, isym "x" 1]
        actual @?= expected,
      testCase "genOpIntermediates" $ do
        let actual =
              flip runFreshT "x" $
                typeOpSimple DivMod
                  >>= genOpIntermediates
                    (Proxy :: Proxy TestSemanticsType)
                    TestSemanticsObj ::
                SymbolicContext (Intermediates SymInteger)
        let expected =
              mrgReturn $
                Intermediates [isym "x" 0, isym "x" 1] [isym "x" 2, isym "x" 3]
        actual @?= expected
    ]
