{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.Concrete.BuilderTest (builderTest) where

import Grisette.Lib.Synth.Program.Concrete
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
    buildProg,
    node,
  )
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add, DivMod),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

concreteProg :: Prog TestSemanticsOp Integer TestSemanticsType
concreteProg =
  Prog
    "test"
    [ProgArg IntType "x" 0, ProgArg IntType "y" 1]
    [ Stmt Add [0, 1] [2],
      Stmt DivMod [2, 0] [3, 4]
    ]
    [ ProgRes IntType 3,
      ProgRes IntType 4,
      ProgRes IntType 2
    ]

builderTest :: Test
builderTest =
  testGroup
    "Builder"
    [ testCase "buildProg" $ do
        let actual =
              buildProg "test" [(IntType, "x"), (IntType, "y")] $
                \[argxRef, argyRef] ->
                  let [addRef] = node Add 1 [argxRef, argyRef]
                      [divRef, modRef] = node DivMod 2 [addRef, argxRef]
                   in [ (divRef, IntType),
                        (modRef, IntType),
                        (addRef, IntType)
                      ]
        actual @?= concreteProg
    ]
