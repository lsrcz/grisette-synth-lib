{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.ComponentSketch.GenIntermediateTest
  ( genIntermediateTest,
  )
where

import Data.Proxy (Proxy (Proxy))
import Grisette
  ( Solvable (isym),
    SymInteger,
    liftUnionM,
    mrgReturn,
    runFreshT,
  )
import Grisette.Lib.Synth.Context (SymbolicContext)
import Grisette.Lib.Synth.Operator.OpTyping
  ( SymOpTyping (typeSymOp),
  )
import Grisette.Lib.Synth.Program.ComponentSketch
  ( Intermediates (Intermediates),
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

genIntermediateTest :: Test
genIntermediateTest =
  testGroup
    "Grisette.Lib.Synth.Program.ComponentSketch.GenIntermediateTest"
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
                typeSymOp DivMod
                  >>= liftUnionM
                  >>= genOpIntermediates
                    (Proxy :: Proxy TestSemanticsType)
                    TestSemanticsObj ::
                SymbolicContext (Intermediates SymInteger)
        let expected =
              mrgReturn $
                Intermediates [isym "x" 0, isym "x" 1] [isym "x" 2, isym "x" 3]
        actual @?= expected
    ]
