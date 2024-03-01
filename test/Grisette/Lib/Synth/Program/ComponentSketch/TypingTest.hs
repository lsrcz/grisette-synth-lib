{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.ComponentSketch.TypingTest (typingTest) where

import Grisette
  ( SymInteger,
  )
import Grisette.Lib.Synth.Program.ComponentSketch
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
import Grisette.Lib.Synth.TypeSignature
  ( TypeSignature (TypeSignature),
  )
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

typingTest :: Test
typingTest = testCase "Typing" $ do
  let prog =
        Prog
          "test"
          [ProgArg "x" IntType, ProgArg "y" IntType]
          [ Stmt Add ["a", "b"] "c" ["d"] "e" "f",
            Stmt DivMod ["g", "h"] "i" ["j", "k"] "l" "m"
          ]
          [ProgRes 4 IntType, ProgRes 5 IntType] ::
          Prog TestSemanticsOp SymInteger TestSemanticsType
  typeProg prog @?= Right (TypeSignature [IntType, IntType] [IntType, IntType])
