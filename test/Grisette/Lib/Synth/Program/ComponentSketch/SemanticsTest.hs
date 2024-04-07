{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.ComponentSketch.SemanticsTest
  ( semanticsTest,
  )
where

import Control.Monad.Error.Class (MonadError (catchError))
import Grisette
  ( EvaluateSym (evaluateSym),
    FreshIdent,
    ITEOp (symIte),
    LogicalOp (symImplies, symNot, (.&&), (.||)),
    SEq ((./=), (.==)),
    SOrd ((.<), (.>=)),
    Solvable (con, isym),
    SymBool,
    SymInteger,
    UnionM,
    mrgIf,
    runFreshT,
  )
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Synth.Context (SymbolicContext)
import Grisette.Lib.Synth.Program.ComponentSketch
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import Grisette.Lib.Synth.Program.ComponentSketch.TestProgram (goodConcreteProg)
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsObj (TestSemanticsObj),
    TestSemanticsOp (Add, DivMod, Double, Inc),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.SymbolicAssertion (symShouldEq, (.@?=))

data ExpectedResult
  = ErrorResult
  | Result SymBool [SymInteger]

data SemanticsTestCase = SemanticsTestCase
  { semanticsTestCaseName :: String,
    semanticsTestCaseProg ::
      Prog (UnionM TestSemanticsOp) SymInteger TestSemanticsType,
    semanticsTestCaseArgs :: [SymInteger],
    semanticsTestCaseExpected :: ExpectedResult,
    semanticsTestCaseFreshIdent :: FreshIdent
  }

semanticsTest :: Test
semanticsTest = testGroup "Semantics" $ do
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
        { semanticsTestCaseName = "concrete excluded by num arg",
          semanticsTestCaseProg =
            Prog
              "test"
              [ProgArg "x" IntType, ProgArg "y" IntType]
              [Stmt (mrgReturn Add) [0, 1, 5] 2 [2] 1 (con False) []]
              [ProgRes 2 IntType],
          semanticsTestCaseArgs = [13, 20],
          semanticsTestCaseExpected =
            let addArg0Val = isym "x" 0 :: SymInteger
                addArg1Val = isym "x" 1 :: SymInteger
                addRes0Val = isym "x" 2 :: SymInteger
                progRes0Val = isym "x" 3 :: SymInteger
             in Result
                  ( (addArg0Val .== 13)
                      .&& (addArg1Val .== 20)
                      .&& (addRes0Val .== 33)
                      .&& (progRes0Val .== 33)
                  )
                  [33],
          semanticsTestCaseFreshIdent = "x"
        },
      SemanticsTestCase
        { semanticsTestCaseName = "concrete excluded by num res",
          semanticsTestCaseProg =
            Prog
              "test"
              [ProgArg "x" IntType, ProgArg "y" IntType]
              [ Stmt (mrgReturn Add) [0, 1] 2 [2, 3] 1 (con False) []
              ]
              [ProgRes 2 IntType],
          semanticsTestCaseArgs = [13, 20],
          semanticsTestCaseExpected =
            let addArg0Val = isym "x" 0 :: SymInteger
                addArg1Val = isym "x" 1 :: SymInteger
                addRes0Val = isym "x" 2 :: SymInteger
                progRes0Val = isym "x" 3 :: SymInteger
             in Result
                  ( (addArg0Val .== 13)
                      .&& (addArg1Val .== 20)
                      .&& (addRes0Val .== 33)
                      .&& (progRes0Val .== 33)
                  )
                  [33],
          semanticsTestCaseFreshIdent = "x"
        },
      SemanticsTestCase
        { semanticsTestCaseName =
            "concrete excluded by num res must still be in bound",
          semanticsTestCaseProg =
            Prog
              "test"
              [ProgArg "x" IntType, ProgArg "y" IntType]
              [ Stmt (mrgReturn Add) [0, 1] 2 [2, 4] 1 (con False) []
              ]
              [ProgRes 2 IntType],
          semanticsTestCaseArgs = [13, 20],
          semanticsTestCaseExpected = ErrorResult,
          semanticsTestCaseFreshIdent = "x"
        },
      SemanticsTestCase
        { semanticsTestCaseName =
            "concrete excluded by num res must still be unique",
          semanticsTestCaseProg =
            Prog
              "test"
              [ProgArg "x" IntType, ProgArg "y" IntType]
              [ Stmt (mrgReturn Add) [0, 1] 2 [2, 2] 1 (con False) []
              ]
              [ProgRes 2 IntType],
          semanticsTestCaseArgs = [13, 20],
          semanticsTestCaseExpected = ErrorResult,
          semanticsTestCaseFreshIdent = "x"
        },
      SemanticsTestCase
        { semanticsTestCaseName =
            "concrete excluded by num res must still be canonical",
          semanticsTestCaseProg =
            Prog
              "test"
              [ProgArg "x" IntType, ProgArg "y" IntType]
              [ Stmt (mrgReturn Add) [0, 1] 2 [2, 5] 1 (con False) [],
                Stmt (mrgReturn Add) [0, 2] 2 [4, 3] 1 (con False) []
              ]
              [ProgRes 4 IntType],
          semanticsTestCaseArgs = [13, 20],
          semanticsTestCaseExpected = ErrorResult,
          semanticsTestCaseFreshIdent = "x"
        },
      SemanticsTestCase
        { semanticsTestCaseName =
            "excluded res cannot be used by non-excluded arg",
          semanticsTestCaseProg =
            Prog
              "test"
              [ProgArg "x" IntType, ProgArg "y" IntType]
              [ Stmt (mrgReturn Add) [0, 1] 2 [2, 3] 1 (con False) [],
                Stmt (mrgReturn Add) [2, 3] 2 [4, 5] 1 (con False) []
              ]
              [ProgRes 4 IntType],
          semanticsTestCaseArgs = [13, 20],
          semanticsTestCaseExpected = ErrorResult,
          semanticsTestCaseFreshIdent = "x"
        },
      SemanticsTestCase
        { semanticsTestCaseName =
            "failed must be after constraint",
          semanticsTestCaseProg =
            Prog
              "test"
              [ProgArg "x" IntType, ProgArg "y" IntType]
              [ Stmt (mrgReturn Add) [0, 1] 2 [2, 3] 1 (con False) [4],
                Stmt (mrgReturn Add) [0, 1] 2 [4, 5] 1 (con False) []
              ]
              [ProgRes 4 IntType],
          semanticsTestCaseArgs = [13, 20],
          semanticsTestCaseExpected = ErrorResult,
          semanticsTestCaseFreshIdent = "x"
        },
      SemanticsTestCase
        { semanticsTestCaseName = "symbolic disable",
          semanticsTestCaseProg =
            Prog
              "test"
              [ProgArg "x" IntType, ProgArg "y" IntType]
              [ Stmt (mrgReturn Add) [0, 1] 2 [2] 1 "dis0" [],
                Stmt (mrgReturn DivMod) [0, 1] 2 [3, 4] 2 "dis1" []
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
        { semanticsTestCaseName = "symbolic operator and arg/res num",
          semanticsTestCaseProg =
            Prog
              "test"
              [ProgArg "x" IntType, ProgArg "y" IntType]
              [ Stmt
                  ( mrgIf
                      "add"
                      (return Add)
                      (mrgIf "divmod" (return DivMod) (return Inc))
                  )
                  [0, 1]
                  "argNum"
                  [2, 3]
                  "resNum"
                  "dis0"
                  []
              ]
              [ProgRes 1 IntType, ProgRes 2 IntType],
          semanticsTestCaseArgs = [10, 4],
          semanticsTestCaseExpected =
            let addArg0Val = isym "x" 0 :: SymInteger
                addArg1Val = isym "x" 1 :: SymInteger
                addRes0Val = isym "x" 2 :: SymInteger
                divModArg0Val = isym "x" 0 :: SymInteger
                divModArg1Val = isym "x" 1 :: SymInteger
                divModRes0Val = isym "x" 2 :: SymInteger
                divModRes1Val = isym "x" 3 :: SymInteger
                incArg0Val = isym "x" 0 :: SymInteger
                incRes0Val = isym "x" 1 :: SymInteger
                res0Val = isym "x" 4 :: SymInteger
                res1Val = isym "x" 5 :: SymInteger
                argNum = "argNum" :: SymInteger
                resNum = "resNum" :: SymInteger
             in Result
                  ( symIte
                      "add"
                      ( (addArg0Val .== 10)
                          .&& (addArg1Val .== 4)
                          .&& (addRes0Val .== 14)
                          .&& (res0Val .== 4)
                          .&& (res1Val .== 14)
                          .&& symNot "dis0"
                          .&& (argNum .== 2)
                          .&& (resNum .== 1)
                      )
                      $ symIte
                        "divmod"
                        ( (divModArg0Val .== 10)
                            .&& (divModArg1Val .== 4)
                            .&& (divModRes0Val .== 2)
                            .&& (divModRes1Val .== 2)
                            .&& (res0Val .== 4)
                            .&& (res1Val .== 2)
                            .&& symNot "dis0"
                            .&& (argNum .== 2)
                            .&& (resNum .== 2)
                        )
                        ( (incArg0Val .== 10)
                            .&& (incRes0Val .== 11)
                            .&& (res0Val .== 4)
                            .&& (res1Val .== 11)
                            .&& symNot "dis0"
                            .&& (argNum .== 1)
                            .&& (resNum .== 1)
                        )
                  )
                  [4, symIte "add" 14 (symIte "divmod" 2 11)],
          semanticsTestCaseFreshIdent = "x"
        },
      SemanticsTestCase
        { semanticsTestCaseName = "symbolic result",
          semanticsTestCaseProg =
            Prog
              "test"
              [ProgArg "x" IntType, ProgArg "y" IntType]
              [Stmt (mrgReturn Add) [0, 1] 2 [2] 1 (con False) []]
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
                  [ Stmt (mrgReturn Inc) [argInc] 1 [resInc] 1 (con False) [],
                    Stmt
                      (mrgReturn Double)
                      [argDouble]
                      1
                      [resDouble]
                      1
                      (con False)
                      []
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
                  [ Stmt
                      (mrgReturn DivMod)
                      [0, 1]
                      2
                      [res00, res01]
                      2
                      (con False)
                      [],
                    Stmt
                      (mrgReturn DivMod)
                      [0, 1]
                      2
                      [res10, res11]
                      2
                      (con False)
                      []
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
              [Stmt (mrgReturn Add) [0, 1] 2 [2, 3] 2 (con False) []]
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
              [ Stmt (mrgReturn Add) [0, 1] 2 [2] 1 (con True) [],
                Stmt (mrgReturn Add) [0, 2] 2 [3] 1 (con False) []
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
              [ Stmt (mrgReturn Add) [0, 1] 2 [2] 1 (con True) [],
                Stmt (mrgReturn Add) [0, 2] 2 [3] 1 (con True) []
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
              [ Stmt (mrgReturn Add) [0, 1] 2 [2] 1 (con True) [],
                Stmt (mrgReturn Add) [0, 2] 2 [3] 1 (con True) []
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
    let processedActual =
          actual `catchError` const (mrgThrowError "Error")
    case expected of
      ErrorResult -> processedActual .@?= mrgThrowError "Error"
      Result preCond expectedIntegers -> do
        let expected =
              mrgIf
                preCond
                (mrgReturn expectedIntegers)
                (mrgThrowError "Error")
        symShouldEq
          processedActual
          expected
          ( \model ->
              "Can be not equal, debug info:\n"
                <> "-- (debug) model --\n"
                <> show model
                <> "-- (debug) actual value --\n"
                <> show (evaluateSym False model actual)
                <> "\n-- (debug) pre condition --\n"
                <> show (evaluateSym False model preCond)
                <> "\n-- (debug) expected integers --\n"
                <> show (evaluateSym False model expectedIntegers)
          )
