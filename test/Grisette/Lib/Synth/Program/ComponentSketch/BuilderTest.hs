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
    mrgIf,
    mrgReturn,
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
import Grisette.Lib.Synth.Program.ComponentSketch.Builder (fromConcrete, simpleFreshStmt)
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
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
    [ testCase "simpleFreshStmt" $ do
        let actual =
              simpleFreshStmt Add :: Fresh (Stmt TestSemanticsOp SymInteger)
        let expected =
              Stmt
                { stmtOp = mrgReturn Add,
                  stmtArgIds = [isym "x" 0, isym "x" 1],
                  stmtArgNum = isym "x" 2,
                  stmtResIds = [isym "x" 3],
                  stmtResNum = isym "x" 4,
                  stmtDisabled = isym "x" 5
                }
        runFresh actual "x" @?= expected,
      testCase "freshStmt" $ do
        let actual =
              freshStmt (return [Add, DivMod]) ::
                Fresh (Stmt TestSemanticsOp SymInteger)
        let expected =
              Stmt
                { stmtOp = mrgIf (isym "x" 0) (return Add) (return DivMod),
                  stmtArgIds = [isym "x" 1, isym "x" 2],
                  stmtArgNum = isym "x" 3,
                  stmtResIds = [isym "x" 4, isym "x" 5],
                  stmtResNum = isym "x" 6,
                  stmtDisabled = isym "x" 7
                }
        runFresh actual "x" @?= expected,
      testGroup
        "MkProg"
        [ testCase "Non-fresh" $ do
            let actual =
                  mkProg
                    "test"
                    [("x", IntType), ("y", IntType)]
                    [ Stmt (mrgReturn Add) ["a", "b"] "c" ["d"] "e" "f",
                      Stmt (mrgReturn DivMod) ["g", "h"] "i" ["j", "k"] "l" "m"
                    ]
                    [("n", IntType), ("o", IntType)] ::
                    Prog TestSemanticsOp SymInteger TestSemanticsType
            let expected =
                  Prog
                    "test"
                    [ProgArg "x" IntType, ProgArg "y" IntType]
                    [ Stmt (mrgReturn Add) ["a", "b"] "c" ["d"] "e" "f",
                      Stmt (mrgReturn DivMod) ["g", "h"] "i" ["j", "k"] "l" "m"
                    ]
                    [ProgRes "n" IntType, ProgRes "o" IntType]
            actual @?= expected,
          testCase "fresh" $ do
            let actual =
                  mkProg
                    "test"
                    [("x", IntType), ("y", IntType)]
                    [simpleFreshStmt Add, simpleFreshStmt DivMod]
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
                        (mrgReturn Add)
                        [isym "x" 0, isym "x" 1]
                        (isym "x" 2)
                        [isym "x" 3]
                        (isym "x" 4)
                        (isym "x" 5),
                      Stmt
                        (mrgReturn DivMod)
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
                [simpleFreshStmt Add, simpleFreshStmt DivMod]
                [IntType, IntType] ::
                Fresh (Prog TestSemanticsOp SymInteger TestSemanticsType)
        let expected =
              Prog
                "test"
                [ProgArg "arg0" IntType, ProgArg "arg1" IntType]
                [ Stmt
                    (mrgReturn Add)
                    [isym "x" 0, isym "x" 1]
                    (isym "x" 2)
                    [isym "x" 3]
                    (isym "x" 4)
                    (isym "x" 5),
                  Stmt
                    (mrgReturn DivMod)
                    [isym "x" 6, isym "x" 7]
                    (isym "x" 8)
                    [isym "x" 9, isym "x" 10]
                    (isym "x" 11)
                    (isym "x" 12)
                ]
                [ ProgRes (isym "x" 13) IntType,
                  ProgRes (isym "x" 14) IntType
                ]
        runFresh actual "x" @?= expected,
      testCase "fromConcrete" $ do
        let conProg =
              Concrete.Prog
                "test"
                [Concrete.ProgArg "x" 0 IntType, Concrete.ProgArg "y" 1 IntType]
                [ Concrete.Stmt Add [0, 1] [2],
                  Concrete.Stmt DivMod [2, 0] [3, 4]
                ]
                [ Concrete.ProgRes 3 IntType,
                  Concrete.ProgRes 4 IntType,
                  Concrete.ProgRes 2 IntType
                ]
        let actual =
              fromConcrete conProg ::
                Fresh (Prog TestSemanticsOp SymInteger TestSemanticsType)
        let expected =
              Prog
                "test"
                [ProgArg "x" IntType, ProgArg "y" IntType]
                [ Stmt
                    (mrgReturn Add)
                    [isym "x" 0, isym "x" 1]
                    (isym "x" 2)
                    [isym "x" 3]
                    (isym "x" 4)
                    (isym "x" 5),
                  Stmt
                    (mrgReturn DivMod)
                    [isym "x" 6, isym "x" 7]
                    (isym "x" 8)
                    [isym "x" 9, isym "x" 10]
                    (isym "x" 11)
                    (isym "x" 12)
                ]
                [ ProgRes (isym "x" 13) IntType,
                  ProgRes (isym "x" 14) IntType,
                  ProgRes (isym "x" 15) IntType
                ]
        runFresh actual "x" @?= expected
    ]
