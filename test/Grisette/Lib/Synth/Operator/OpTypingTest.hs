{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Operator.OpTypingTest (opTypingTest) where

import Grisette.Lib.Synth.Operator.OpTyping
  ( OpTyping (typeOp),
    SymOpLimits (symOpMaximumArgNum, symOpMaximumResNum),
  )
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add),
    TestSemanticsType (IntType),
  )
import Grisette.Lib.Synth.TypeSignature (TypeSignature (TypeSignature))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

opTypingTest :: Test
opTypingTest =
  testGroup
    "Grisette.Lib.Synth.Operator.OpTyping"
    [ testCase "Default OpTyping" $ do
        let expected =
              Right $ TypeSignature [IntType, IntType] [IntType]
        typeOp Add @?= expected,
      testCase "Default SymOpLimits" $ do
        symOpMaximumArgNum Add @?= 2
        symOpMaximumResNum Add @?= 1
    ]
