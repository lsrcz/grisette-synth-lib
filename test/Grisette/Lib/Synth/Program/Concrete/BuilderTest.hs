{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.Concrete.BuilderTest (builderTest) where

import Grisette.Lib.Synth.Program.Concrete
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
    buildProg,
    node1,
    node2,
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
    [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
    [ Stmt Add [0, 1] [2],
      Stmt DivMod [2, 0] [3, 4]
    ]
    [ ProgRes 3 IntType,
      ProgRes 4 IntType,
      ProgRes 2 IntType
    ]

builderTest :: Test
builderTest =
  testGroup
    "Builder"
    [ testGroup
        "buildProg"
        [ testCase "simple" $ do
            let actual =
                  buildProg [("x", IntType), ("y", IntType)] $
                    \[argxRef, argyRef] -> do
                      addRef <- node1 Add [argxRef, argyRef]
                      (divRef, modRef) <- node2 DivMod [addRef, argxRef]
                      return
                        [ (divRef, IntType),
                          (modRef, IntType),
                          (addRef, IntType)
                        ]
            actual @?= concreteProg,
          testCase "pseudo dep 1" $ do
            let actual = buildProg [("x", IntType), ("y", IntType)] $
                  \[argxRef, argyRef] -> do
                    addRef <- node1 Add [argxRef, argyRef]
                    (divRef, modRef) <- node2 DivMod [addRef, argxRef]
                    return
                      [ (divRef, IntType),
                        (modRef, IntType),
                        (addRef, IntType)
                      ]
            let expected =
                  Prog
                    [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
                    [Stmt Add [0, 1] [2], Stmt DivMod [0, 1] [3, 4]]
                    [ProgRes 3 IntType, ProgRes 4 IntType, ProgRes 2 IntType] ::
                    Prog TestSemanticsOp Integer TestSemanticsType
            actual @?= expected,
          testCase "pseudo dep 2" $ do
            let actual = buildProg [("x", IntType), ("y", IntType)] $
                  \[argxRef, argyRef] -> do
                    (divRef, modRef) <- node2 DivMod [argxRef, argyRef]
                    addRef <- node1 Add [argxRef, argyRef]
                    return
                      [ (divRef, IntType),
                        (modRef, IntType),
                        (addRef, IntType)
                      ]
            let expected =
                  Prog
                    [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
                    [Stmt DivMod [0, 1] [2, 3], Stmt Add [0, 1] [4]]
                    [ProgRes 2 IntType, ProgRes 3 IntType, ProgRes 4 IntType] ::
                    Prog TestSemanticsOp Integer TestSemanticsType
            actual @?= expected
        ]
    ]
