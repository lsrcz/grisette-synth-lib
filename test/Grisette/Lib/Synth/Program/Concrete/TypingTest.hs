{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.Concrete.TypingTest (typingTest) where

import Grisette.Lib.Synth.Program.Concrete
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import Grisette.Lib.Synth.Program.ProgTyping (ProgTyping (typeProg))
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add, DivMod),
    TestSemanticsType (IntType),
  )
import Grisette.Lib.Synth.TypeSignature (TypeSignature (TypeSignature))
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

typingTest :: Test
typingTest = testCase "typing" $ do
  let prog =
        Prog
          "test"
          [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
          [ Stmt Add [0, 1] [3],
            Stmt DivMod [3, 0] [4, 5]
          ]
          [ProgRes 4 IntType, ProgRes 5 IntType] ::
          Prog TestSemanticsOp Integer TestSemanticsType
  typeProg prog @?= TypeSignature [IntType, IntType] [IntType, IntType]
