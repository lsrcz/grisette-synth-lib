{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.ByteCodeSketch.TypingTest
  ( typingTest,
  )
where

import Grisette (SymInteger)
import Grisette.Lib.Synth.Program.ByteCodeSketch
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
typingTest = testCase "Typing" $ do
  let prog =
        Prog
          "test"
          [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
          [ Stmt Add [0, 1] 2 [3] 1,
            Stmt DivMod [3, 0] 2 [4, 5] 2
          ]
          [ProgRes 4 IntType, ProgRes 5 IntType] ::
          Prog TestSemanticsOp Integer SymInteger TestSemanticsType
  typeProg prog @?= TypeSignature [IntType, IntType] [IntType, IntType]
