{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.ComponentSketch.BuilderTest
  ( builderTest,
  )
where

import Grisette
  ( Fresh,
    GenSymSimple (simpleFresh),
    Solvable (isym),
    SymInteger,
    runFresh,
  )
import Grisette.Lib.Synth.Program.ComponentSketch
  ( MkFreshProg (mkFreshProg),
    MkProg (mkProg),
    Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt
      ( Stmt,
        stmtArgIds,
        stmtArgNum,
        stmtDisabled,
        stmtOp,
        stmtResIds,
        stmtResNum
      ),
    freshStmt,
  )
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add, DivMod),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

builderTest :: Test
builderTest =
  testGroup
    "Builder"
    [ testCase "freshStmt" $ do
        let actual =
              freshStmt (return Add) ::
                Fresh (Stmt TestSemanticsOp SymInteger TestSemanticsType)
        let expected =
              Stmt
                { stmtOp = Add,
                  stmtArgIds = [isym "x" 0, isym "x" 1],
                  stmtArgNum = isym "x" 2,
                  stmtResIds = [isym "x" 3],
                  stmtResNum = isym "x" 4,
                  stmtDisabled = isym "x" 5
                }
        runFresh actual "x" @?= expected,
      testGroup
        "MkProg"
        [ testCase "Non-fresh" $ do
            let actual =
                  mkProg
                    "test"
                    [("x", IntType), ("y", IntType)]
                    [ Stmt Add ["a", "b"] "c" ["d"] "e" "f",
                      Stmt DivMod ["g", "h"] "i" ["j", "k"] "l" "m"
                    ]
                    [("n", IntType), ("o", IntType)] ::
                    Prog TestSemanticsOp SymInteger TestSemanticsType
            let expected =
                  Prog
                    "test"
                    [ProgArg "x" IntType, ProgArg "y" IntType]
                    [ Stmt Add ["a", "b"] "c" ["d"] "e" "f",
                      Stmt DivMod ["g", "h"] "i" ["j", "k"] "l" "m"
                    ]
                    [ProgRes "n" IntType, ProgRes "o" IntType]
            actual @?= expected,
          testCase "fresh" $ do
            let actual =
                  mkProg
                    "test"
                    [("x", IntType), ("y", IntType)]
                    [ freshStmt (return Add),
                      freshStmt (return DivMod)
                    ]
                    [ (simpleFresh (), IntType),
                      (simpleFresh (), IntType)
                    ] ::
                    Fresh
                      (Prog TestSemanticsOp SymInteger TestSemanticsType)
            let expected =
                  Prog
                    "test"
                    [ProgArg "x" IntType, ProgArg "y" IntType]
                    [ Stmt
                        Add
                        [isym "x" 0, isym "x" 1]
                        (isym "x" 2)
                        [isym "x" 3]
                        (isym "x" 4)
                        (isym "x" 5),
                      Stmt
                        DivMod
                        [isym "x" 6, isym "x" 7]
                        (isym "x" 8)
                        [isym "x" 9, isym "x" 10]
                        (isym "x" 11)
                        (isym "x" 12)
                    ]
                    [ ProgRes (isym "x" 13) IntType,
                      ProgRes (isym "x" 14) IntType
                    ]
            runFresh actual "x" @?= expected
        ],
      testCase "MkFreshProg" $ do
        let actual =
              mkFreshProg
                "test"
                [IntType, IntType]
                [freshStmt (return Add), freshStmt (return DivMod)]
                [IntType, IntType] ::
                Fresh (Prog TestSemanticsOp SymInteger TestSemanticsType)
        let expected =
              Prog
                "test"
                [ProgArg "arg0" IntType, ProgArg "arg1" IntType]
                [ Stmt
                    Add
                    [isym "x" 0, isym "x" 1]
                    (isym "x" 2)
                    [isym "x" 3]
                    (isym "x" 4)
                    (isym "x" 5),
                  Stmt
                    DivMod
                    [isym "x" 6, isym "x" 7]
                    (isym "x" 8)
                    [isym "x" 9, isym "x" 10]
                    (isym "x" 11)
                    (isym "x" 12)
                ]
                [ ProgRes (isym "x" 13) IntType,
                  ProgRes (isym "x" 14) IntType
                ]
        runFresh actual "x" @?= expected
    ]
