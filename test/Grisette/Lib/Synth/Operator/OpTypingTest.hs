{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Operator.OpTypingTest (opTypingTest) where

import Grisette (UnionM, mrgIf, mrgReturn)
import Grisette.Lib.Synth.Context (SymbolicContext)
import Grisette.Lib.Synth.Operator.OpTyping
  ( OpTyping (typeOp),
    SymOpLimits (symOpMaximumArgNum, symOpMaximumResNum),
  )
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add, DivMod, Inc),
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
      testCase "UnionM OpTyping" $ do
        let op =
              mrgIf "a" (mrgReturn Add) (mrgReturn DivMod) ::
                UnionM TestSemanticsOp
        let actual =
              typeOp op :: SymbolicContext (TypeSignature TestSemanticsType)
        let expected =
              mrgIf
                "a"
                (mrgReturn $ TypeSignature [IntType, IntType] [IntType])
                ( mrgReturn $
                    TypeSignature [IntType, IntType] [IntType, IntType]
                )
        actual @?= expected,
      testCase "Default SymOpLimits" $ do
        symOpMaximumArgNum Add @?= 2
        symOpMaximumResNum Add @?= 1,
      testCase "UnionM SymOpLimits" $ do
        symOpMaximumArgNum
          ( mrgIf "a" (mrgReturn Add) (mrgReturn Inc) ::
              UnionM TestSemanticsOp
          )
          @?= 2
        symOpMaximumResNum
          ( mrgIf "a" (mrgReturn Add) (mrgReturn DivMod) ::
              UnionM TestSemanticsOp
          )
          @?= 2
    ]
