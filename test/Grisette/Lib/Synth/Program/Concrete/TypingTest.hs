{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.Concrete.TypingTest (typingTest) where

import Grisette.Lib.Synth.Operator.OpTyping (TypeSignature (TypeSignature))
import Grisette.Lib.Synth.Program.Concrete
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import Grisette.Lib.Synth.Program.ProgTyping (ProgTyping (typeProg))
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsObj (TestSemanticsObj),
    TestSemanticsOp (Add, DivMod),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

typingTest :: Test
typingTest = testCase "typing" $ do
  let prog =
        Prog
          "test"
          [ProgArg IntType "x" 0, ProgArg IntType "y" 1]
          [ Stmt Add [0, 1] [3],
            Stmt DivMod [3, 0] [4, 5]
          ]
          [ProgRes IntType 4, ProgRes IntType 5] ::
          Prog TestSemanticsOp Integer TestSemanticsType
  typeProg TestSemanticsObj prog
    @?= Right (TypeSignature [IntType, IntType] [IntType, IntType])
