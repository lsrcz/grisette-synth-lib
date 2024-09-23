{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Operator.OpTypingTest (opTypingTest) where

import Grisette (Union, mrgIf, mrgReturn)
import Grisette.Lib.Synth.Context (SymbolicContext)
import Grisette.Lib.Synth.Operator.OpTyping
  ( OpTyping (typeOp),
    symOpMaximumArgNum,
    symOpMaximumResNum,
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
        typeOp mempty Add @?= expected,
      testCase "Union OpTyping" $ do
        let op =
              mrgIf "a" (mrgReturn Add) (mrgReturn DivMod) ::
                Union TestSemanticsOp
        let actual =
              typeOp mempty op ::
                SymbolicContext (TypeSignature TestSemanticsType)
        let expected =
              mrgIf
                "a"
                (mrgReturn $ TypeSignature [IntType, IntType] [IntType])
                ( mrgReturn $
                    TypeSignature [IntType, IntType] [IntType, IntType]
                )
        actual @?= expected,
      testCase "Default SymOpLimits" $ do
        symOpMaximumArgNum mempty Add @?= 2
        symOpMaximumResNum mempty Add @?= 1,
      testCase "Union SymOpLimits" $ do
        symOpMaximumArgNum
          mempty
          ( mrgIf "a" (mrgReturn Add) (mrgReturn Inc) ::
              Union TestSemanticsOp
          )
          @?= 2
        symOpMaximumResNum
          mempty
          ( mrgIf "a" (mrgReturn Add) (mrgReturn DivMod) ::
              Union TestSemanticsOp
          )
          @?= 2
    ]
