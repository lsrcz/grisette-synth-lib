{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.ComponentSketchTest
  ( componentSketchTest,
  )
where

import Control.Monad.Error.Class (MonadError (catchError))
import Grisette
  ( EvaluateSym (evaluateSym),
    Fresh,
    FreshIdent,
    GenSymSimple (simpleFresh),
    ITEOp (symIte),
    LogicalOp (symImplies, symNot, (.&&), (.||)),
    SEq ((./=), (.==)),
    SOrd ((.<), (.>=)),
    Solvable (con, isym),
    SymBool,
    SymInteger,
    ToCon (toCon),
    mrgIf,
    runFresh,
    runFreshT,
  )
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Synth.Context (SymbolicContext)
import Grisette.Lib.Synth.Operator.OpTyping (TypeSignature (TypeSignature))
import Grisette.Lib.Synth.Program.ComponentSketch
  ( MkFreshProg (mkFreshProg),
    MkFreshStmt (mkFreshStmt),
    MkProg (mkProg),
    MkStmt (mkStmt),
    Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt, stmtArgIds, stmtDisabled, stmtOp, stmtResIds),
  )
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Program.ProgTyping (ProgTyping (typeProg))
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsObj (TestSemanticsObj),
    TestSemanticsOp (Add, DivMod, Double, Inc),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Test.SymbolicAssertion (symShouldEq, (.@?=))

data ToConTestCase = ToConTestCase
  { toConTestCaseName :: String,
    toConTestCaseProg :: Prog TestSemanticsOp SymInteger TestSemanticsType,
    toConTestCaseExpected ::
      Maybe (Concrete.Prog TestSemanticsOp Integer TestSemanticsType)
  }

data ExpectedResult
  = ErrorResult
  | Result SymBool [SymInteger]

data SemanticsTestCase = SemanticsTestCase
  { semanticsTestCaseName :: String,
    semanticsTestCaseProg ::
      Prog TestSemanticsOp SymInteger TestSemanticsType,
    semanticsTestCaseArgs :: [SymInteger],
    semanticsTestCaseExpected :: ExpectedResult,
    semanticsTestCaseFreshIdent :: FreshIdent
  }

goodConcreteProg :: Prog TestSemanticsOp SymInteger TestSemanticsType
goodConcreteProg =
  Prog
    "test"
    [ProgArg "x" IntType, ProgArg "y" IntType]
    [ Stmt Add [0, 1] [2] $ con False,
      Stmt DivMod [2, 0] [3, 4] $ con False
    ]
    [ProgRes 3 IntType, ProgRes 4 IntType]

componentSketchTest :: Test
componentSketchTest =
  testGroup
    "Grisette.Lib.Synth.Program.ComponentSketch"
    [ testGroup "ToCon" $ do
        ToConTestCase name prog expected <-
          [ ToConTestCase
              { toConTestCaseName = "goodConcreteProg",
                toConTestCaseProg = goodConcreteProg,
                toConTestCaseExpected =
                  Just $
                    Concrete.Prog
                      "test"
                      [ Concrete.ProgArg "x" 0 IntType,
                        Concrete.ProgArg "y" 1 IntType
                      ]
                      [ Concrete.Stmt Add [0, 1] [2],
                        Concrete.Stmt DivMod [2, 0] [3, 4]
                      ]
                      [Concrete.ProgRes 3 IntType, Concrete.ProgRes 4 IntType]
              },
            ToConTestCase
              { toConTestCaseName = "reorder",
                toConTestCaseProg =
                  Prog
                    "test"
                    [ProgArg "x" IntType, ProgArg "y" IntType]
                    [ Stmt DivMod [2, 0] [3, 4] $ con False,
                      Stmt Add [0, 1] [2] $ con False
                    ]
                    [ProgRes 3 IntType, ProgRes 4 IntType],
                toConTestCaseExpected =
                  Just $
                    Concrete.Prog
                      "test"
                      [ Concrete.ProgArg "x" 0 IntType,
                        Concrete.ProgArg "y" 1 IntType
                      ]
                      [ Concrete.Stmt Add [0, 1] [2],
                        Concrete.Stmt DivMod [2, 0] [3, 4]
                      ]
                      [Concrete.ProgRes 3 IntType, Concrete.ProgRes 4 IntType]
              }
            ]
        return $ testCase name $ toCon prog @?= expected,
      testGroup "Semantics" $ do
        SemanticsTestCase name prog args expected ident <-
          [ SemanticsTestCase
              { semanticsTestCaseName = "concrete program",
                semanticsTestCaseProg = goodConcreteProg,
                semanticsTestCaseArgs = [13, 20],
                semanticsTestCaseExpected =
                  let addArg0Val = isym "x" 0 :: SymInteger
                      addArg1Val = isym "x" 1 :: SymInteger
                      addRes0Val = isym "x" 2 :: SymInteger
                      divModArg0Val = isym "x" 3 :: SymInteger
                      divModArg1Val = isym "x" 4 :: SymInteger
                      divModRes0Val = isym "x" 5 :: SymInteger
                      divModRes1Val = isym "x" 6 :: SymInteger
                      progRes0Val = isym "x" 7 :: SymInteger
                      progRes1Val = isym "x" 8 :: SymInteger
                   in Result
                        ( (addArg0Val .== 13)
                            .&& (addArg1Val .== 20)
                            .&& (addRes0Val .== 33)
                            .&& (divModArg0Val .== 33)
                            .&& (divModArg1Val .== 13)
                            .&& (divModRes0Val .== 2)
                            .&& (divModRes1Val .== 7)
                            .&& (progRes0Val .== 2)
                            .&& (progRes1Val .== 7)
                        )
                        [2, 7],
                semanticsTestCaseFreshIdent = "x"
              },
            SemanticsTestCase
              { semanticsTestCaseName = "symbolic disable",
                semanticsTestCaseProg =
                  Prog
                    "test"
                    [ProgArg "x" IntType, ProgArg "y" IntType]
                    [ Stmt Add [0, 1] [2] "dis0",
                      Stmt DivMod [0, 1] [3, 4] "dis1"
                    ]
                    [ProgRes 1 IntType, ProgRes 2 IntType],
                semanticsTestCaseArgs = [1, 0],
                semanticsTestCaseExpected =
                  let addArg0Val = isym "x" 0 :: SymInteger
                      addArg1Val = isym "x" 1 :: SymInteger
                      addRes0Val = isym "x" 2 :: SymInteger
                      progRes0Val = isym "x" 7 :: SymInteger
                      progRes1Val = isym "x" 8 :: SymInteger
                   in Result
                        ( (addArg0Val .== 1)
                            .&& (addArg1Val .== 0)
                            .&& (addRes0Val .== 1)
                            .&& (progRes0Val .== 0)
                            .&& (progRes1Val .== 1)
                            .&& "dis1"
                            .&& symNot "dis0"
                        )
                        [0, 1],
                semanticsTestCaseFreshIdent = "x"
              },
            SemanticsTestCase
              { semanticsTestCaseName = "symbolic result",
                semanticsTestCaseProg =
                  Prog
                    "test"
                    [ProgArg "x" IntType, ProgArg "y" IntType]
                    [Stmt Add [0, 1] [2] $ con False]
                    [ProgRes "res" IntType],
                semanticsTestCaseArgs = [13, 20],
                semanticsTestCaseExpected =
                  let addArg0Val = isym "x" 0 :: SymInteger
                      addArg1Val = isym "x" 1 :: SymInteger
                      addRes0Val = isym "x" 2 :: SymInteger
                      progRes0Val = isym "x" 3 :: SymInteger
                      resId = "res" :: SymInteger
                   in Result
                        ( (addArg0Val .== 13)
                            .&& (addArg1Val .== 20)
                            .&& (addRes0Val .== 33)
                            .&& symImplies (resId .== 0) (progRes0Val .== 13)
                            .&& symImplies (resId .== 1) (progRes0Val .== 20)
                            .&& symImplies (resId .== 2) (progRes0Val .== 33)
                            .&& (resId .>= 0)
                            .&& (resId .< 3)
                        )
                        [symIte (resId .== 0) 13 (symIte (resId .== 1) 20 33)],
                semanticsTestCaseFreshIdent = "x"
              },
            let ident = "x"
                argInc = "argInc" :: SymInteger
                resInc = "resInc" :: SymInteger
                argDouble = "argDouble" :: SymInteger
                resDouble = "resDouble" :: SymInteger
                argIncVal = isym ident 0 :: SymInteger
                resIncVal = isym ident 1 :: SymInteger
                argDoubleVal = isym ident 2 :: SymInteger
                resDoubleVal = isym ident 3 :: SymInteger
                progResVal = isym ident 4 :: SymInteger
             in SemanticsTestCase
                  { semanticsTestCaseName = "symbolic instructions",
                    semanticsTestCaseProg =
                      Prog
                        "test"
                        [ProgArg "x" IntType]
                        [ Stmt Inc [argInc] [resInc] $ con False,
                          Stmt Double [argDouble] [resDouble] $ con False
                        ]
                        [ProgRes 2 IntType],
                    semanticsTestCaseArgs = [13],
                    semanticsTestCaseExpected =
                      Result
                        ( (argInc .>= 0 .&& argInc .< resInc)
                            .&& (argDouble .>= 0 .&& argDouble .< resDouble)
                            .&& (resInc .== 1 .|| resInc .== 2)
                            .&& (resDouble .== 1 .|| resDouble .== 2)
                            .&& (resInc ./= resDouble)
                            .&& (resIncVal .== argIncVal + 1)
                            .&& (resDoubleVal .== argDoubleVal + argDoubleVal)
                            .&& symImplies
                              (resDouble .== 2)
                              (progResVal .== resDoubleVal)
                            .&& symImplies
                              (resInc .== 2)
                              (progResVal .== resIncVal)
                            .&& symImplies
                              (resInc .== 1)
                              ( (argIncVal .== 13)
                                  .&& symImplies
                                    (argDouble .== 0)
                                    (argDoubleVal .== 13)
                                  .&& symImplies
                                    (argDouble .== 1)
                                    (argDoubleVal .== 14)
                              )
                            .&& symImplies
                              (resDouble .== 1)
                              ( (argDoubleVal .== 13)
                                  .&& symImplies
                                    (argInc .== 0)
                                    (argIncVal .== 13)
                                  .&& symImplies
                                    (argInc .== 1)
                                    (argIncVal .== 26)
                              )
                        )
                        [ symIte
                            (resInc .== 1)
                            (symIte (argDouble .== 0) 26 28)
                            (symIte (argInc .== 0) 14 27)
                        ],
                    semanticsTestCaseFreshIdent = "x"
                  },
            let res00 = "res00"
                res01 = "res01"
                res10 = "res10"
                res11 = "res11"
                arg00Val = isym "x" 0 :: SymInteger
                arg01Val = isym "x" 1 :: SymInteger
                res00Val = isym "x" 2 :: SymInteger
                res01Val = isym "x" 3 :: SymInteger
                arg10Val = isym "x" 4 :: SymInteger
                arg11Val = isym "x" 5 :: SymInteger
                res10Val = isym "x" 6 :: SymInteger
                res11Val = isym "x" 7 :: SymInteger
                progRes0Val = isym "x" 8 :: SymInteger
                progRes1Val = isym "x" 9 :: SymInteger
             in SemanticsTestCase
                  { semanticsTestCaseName = "multi-result statements",
                    semanticsTestCaseProg =
                      Prog
                        "test"
                        [ProgArg "x" IntType, ProgArg "y" IntType]
                        [ Stmt DivMod [0, 1] [res00, res01] $ con False,
                          Stmt DivMod [0, 1] [res10, res11] $ con False
                        ]
                        [ProgRes 4 IntType, ProgRes 5 IntType],
                    semanticsTestCaseArgs = [20, 13],
                    semanticsTestCaseExpected =
                      Result
                        ( (res01 .== res00 + 1)
                            .&& (res11 .== res10 + 1)
                            .&& (res00 .== 2 .|| res00 .== 4)
                            .&& symImplies (res00 .== 2) (res10 .== 4)
                            .&& symImplies (res00 .== 4) (res10 .== 2)
                            .&& (arg00Val .== 20)
                            .&& (arg01Val .== 13)
                            .&& (arg10Val .== 20)
                            .&& (arg11Val .== 13)
                            .&& (res00Val .== 1)
                            .&& (res01Val .== 7)
                            .&& (res10Val .== 1)
                            .&& (res11Val .== 7)
                            .&& (progRes0Val .== 1)
                            .&& (progRes1Val .== 7)
                        )
                        [1, 7],
                    semanticsTestCaseFreshIdent = "x"
                  },
            SemanticsTestCase
              { semanticsTestCaseName = "divByZero",
                semanticsTestCaseProg = goodConcreteProg,
                semanticsTestCaseArgs = [0, 20],
                semanticsTestCaseExpected = ErrorResult,
                semanticsTestCaseFreshIdent = "x"
              },
            SemanticsTestCase
              { semanticsTestCaseName = "incorrect number of arguments",
                semanticsTestCaseProg = goodConcreteProg,
                semanticsTestCaseArgs = [20],
                semanticsTestCaseExpected = ErrorResult,
                semanticsTestCaseFreshIdent = "x"
              },
            SemanticsTestCase
              { semanticsTestCaseName = "incorrect number of statement results",
                semanticsTestCaseProg =
                  Prog
                    "test"
                    [ProgArg "x" IntType, ProgArg "y" IntType]
                    [Stmt Add [0, 1] [2, 3] $ con False]
                    [ProgRes 2 IntType],
                semanticsTestCaseArgs = [1, 2],
                semanticsTestCaseExpected = ErrorResult,
                semanticsTestCaseFreshIdent = "x"
              },
            SemanticsTestCase
              { semanticsTestCaseName = "use disabled values",
                semanticsTestCaseProg =
                  Prog
                    "test"
                    [ProgArg "x" IntType, ProgArg "y" IntType]
                    [ Stmt Add [0, 1] [2] $ con True,
                      Stmt Add [0, 2] [3] $ con False
                    ]
                    [ProgRes 0 IntType],
                semanticsTestCaseArgs = [1, 2],
                semanticsTestCaseExpected = ErrorResult,
                semanticsTestCaseFreshIdent = "x"
              },
            SemanticsTestCase
              { semanticsTestCaseName = "use disabled values in results",
                semanticsTestCaseProg =
                  Prog
                    "test"
                    [ProgArg "x" IntType, ProgArg "y" IntType]
                    [ Stmt Add [0, 1] [2] $ con True,
                      Stmt Add [0, 2] [3] $ con True
                    ]
                    [ProgRes 2 IntType],
                semanticsTestCaseArgs = [1, 2],
                semanticsTestCaseExpected = ErrorResult,
                semanticsTestCaseFreshIdent = "x"
              },
            SemanticsTestCase
              { semanticsTestCaseName =
                  "disabled statement may use disabled values",
                semanticsTestCaseProg =
                  Prog
                    "test"
                    [ProgArg "x" IntType, ProgArg "y" IntType]
                    [ Stmt Add [0, 1] [2] $ con True,
                      Stmt Add [0, 2] [3] $ con True
                    ]
                    [ProgRes 0 IntType],
                semanticsTestCaseArgs = [1, 2],
                semanticsTestCaseExpected =
                  let progRes0Val = isym "x" 6 :: SymInteger
                   in Result (progRes0Val .== 1) [1],
                semanticsTestCaseFreshIdent = "x"
              }
            ]
        return $ testCase name $ do
          let actual =
                flip runFreshT ident $ runProg TestSemanticsObj prog args ::
                  SymbolicContext [SymInteger]
          let processedActual = actual `catchError` const (mrgThrowError "Error")
          case expected of
            ErrorResult -> processedActual .@?= mrgThrowError "Error"
            Result preCond expectedIntegers -> do
              let expected =
                    mrgIf preCond (mrgReturn expectedIntegers) (mrgThrowError "Error")
              symShouldEq
                processedActual
                expected
                ( \model ->
                    "Can be not equal, debug info:\n"
                      <> "-- (debug) actual value --\n"
                      <> show (evaluateSym False model actual)
                      <> "\n-- (debug) pre condition --\n"
                      <> show (evaluateSym False model preCond)
                      <> "\n-- (debug) expected integers --\n"
                      <> show (evaluateSym False model expectedIntegers)
                ),
      testCase "Typing" $ do
        let prog =
              Prog
                "test"
                [ProgArg "x" IntType, ProgArg "y" IntType]
                [ Stmt Add ["a", "b"] ["c"] "d",
                  Stmt DivMod ["e", "f"] ["g", "h"] "i"
                ]
                [ProgRes 4 IntType, ProgRes 5 IntType] ::
                Prog TestSemanticsOp SymInteger TestSemanticsType
        typeProg TestSemanticsObj prog
          @?= Right (TypeSignature [IntType, IntType] [IntType, IntType]),
      testGroup
        "Builder"
        [ testGroup
            "MkStmt"
            [ testCase "Non-fresh" $ do
                let actual =
                      mkStmt Add ["a", "b"] ["c"] "d" ::
                        Stmt TestSemanticsOp SymInteger
                let expected =
                      Stmt
                        { stmtOp = Add,
                          stmtArgIds = ["a", "b"],
                          stmtResIds = ["c"],
                          stmtDisabled = "d"
                        }
                actual @?= expected,
              testCase "fresh" $ do
                let actual =
                      mkStmt
                        (return Add)
                        [simpleFresh (), simpleFresh ()]
                        [simpleFresh ()]
                        (simpleFresh ()) ::
                        Fresh (Stmt TestSemanticsOp SymInteger)
                let expected =
                      Stmt
                        { stmtOp = Add,
                          stmtArgIds = [isym "x" 0, isym "x" 1],
                          stmtResIds = [isym "x" 2],
                          stmtDisabled = isym "x" 3
                        }
                runFresh actual "x" @?= expected
            ],
          testCase "MkFreshStmt" $ do
            let actual =
                  mkFreshStmt
                    (return Add)
                    2
                    1 ::
                    Fresh (Stmt TestSemanticsOp SymInteger)
            let expected =
                  Stmt
                    { stmtOp = Add,
                      stmtArgIds = [isym "x" 0, isym "x" 1],
                      stmtResIds = [isym "x" 2],
                      stmtDisabled = isym "x" 3
                    }
            runFresh actual "x" @?= expected,
          testGroup
            "MkProg"
            [ testCase "Non-fresh" $ do
                let actual =
                      mkProg
                        "test"
                        [("x", IntType), ("y", IntType)]
                        [ Stmt Add ["a", "b"] ["c"] "d",
                          Stmt DivMod ["e", "f"] ["g", "h"] "i"
                        ]
                        [("j", IntType), ("k", IntType)] ::
                        Prog TestSemanticsOp SymInteger TestSemanticsType
                let expected =
                      Prog
                        "test"
                        [ProgArg "x" IntType, ProgArg "y" IntType]
                        [ Stmt Add ["a", "b"] ["c"] "d",
                          Stmt DivMod ["e", "f"] ["g", "h"] "i"
                        ]
                        [ProgRes "j" IntType, ProgRes "k" IntType]
                actual @?= expected,
              testCase "fresh" $ do
                let actual =
                      mkProg
                        "test"
                        [("x", IntType), ("y", IntType)]
                        [ mkStmt
                            (return Add)
                            [simpleFresh (), simpleFresh ()]
                            [simpleFresh ()]
                            (simpleFresh ()),
                          mkStmt
                            (return DivMod)
                            [simpleFresh (), simpleFresh ()]
                            [simpleFresh (), simpleFresh ()]
                            (simpleFresh ())
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
                            [isym "x" 2]
                            (isym "x" 3),
                          Stmt
                            DivMod
                            [isym "x" 4, isym "x" 5]
                            [isym "x" 6, isym "x" 7]
                            (isym "x" 8)
                        ]
                        [ ProgRes (isym "x" 9) IntType,
                          ProgRes (isym "x" 10) IntType
                        ]
                runFresh actual "x" @?= expected
            ],
          testCase "MkFreshProg" $ do
            let actual =
                  mkFreshProg
                    "test"
                    [IntType, IntType]
                    [ mkFreshStmt (return Add) 2 1,
                      mkFreshStmt (return DivMod) 2 2
                    ]
                    [IntType, IntType] ::
                    Fresh
                      (Prog TestSemanticsOp SymInteger TestSemanticsType)
            let expected =
                  Prog
                    "test"
                    [ProgArg "arg0" IntType, ProgArg "arg1" IntType]
                    [ Stmt
                        Add
                        [isym "x" 0, isym "x" 1]
                        [isym "x" 2]
                        (isym "x" 3),
                      Stmt
                        DivMod
                        [isym "x" 4, isym "x" 5]
                        [isym "x" 6, isym "x" 7]
                        (isym "x" 8)
                    ]
                    [ ProgRes (isym "x" 9) IntType,
                      ProgRes (isym "x" 10) IntType
                    ]
            runFresh actual "x" @?= expected
        ]
    ]
