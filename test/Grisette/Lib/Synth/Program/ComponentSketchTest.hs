{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.ComponentSketchTest
  ( componentSketchTest,
  )
where

import Control.Monad.Error.Class (MonadError (catchError))
import Grisette
  ( EvaluateSym (evaluateSym),
    FreshIdent,
    ITEOp (symIte),
    LogicalOp (symImplies, (.&&), (.||)),
    SEq ((./=), (.==)),
    SOrd ((.<), (.>=)),
    Solvable (isym),
    SymBool,
    SymInteger,
    mrgIf,
    runFreshT,
  )
import Grisette.Lib.Synth.Context
  ( MonadContext (raiseError, result),
    SymbolicContext,
  )
import Grisette.Lib.Synth.Program.ComponentSketch
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
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
      Prog TestSemanticsOp SymInteger TestSemanticsType,
    semanticsTestCaseArgs :: [SymInteger],
    semanticsTestCaseExpected :: ExpectedResult,
    semanticsTestCaseFreshIdent :: FreshIdent
  }

componentSketchTest :: Test
componentSketchTest =
  testGroup "Grisette.Lib.Synth.Program.ComponentSketch" $ do
    let goodConcreteProg =
          Prog
            "test"
            [ProgArg IntType "x", ProgArg IntType "y"]
            [ Stmt Add [0, 1] [2],
              Stmt DivMod [2, 0] [3, 4]
            ]
            [ProgRes IntType 3, ProgRes IntType 4]

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
          { semanticsTestCaseName = "symbolic result",
            semanticsTestCaseProg =
              Prog
                "test"
                [ProgArg IntType "x", ProgArg IntType "y"]
                [Stmt Add [0, 1] [2]]
                [ProgRes IntType "res"],
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
                        .&& ((resId .== 0) `symImplies` (progRes0Val .== 13))
                        .&& ((resId .== 1) `symImplies` (progRes0Val .== 20))
                        .&& ((resId .== 2) `symImplies` (progRes0Val .== 33))
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
                    [ProgArg IntType "x"]
                    [ Stmt Inc [argInc] [resInc],
                      Stmt Double [argDouble] [resDouble]
                    ]
                    [ProgRes IntType 2],
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
                        .&& symImplies (resInc .== 2) (progResVal .== resIncVal)
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
                    [ProgArg IntType "x", ProgArg IntType "y"]
                    [ Stmt DivMod [0, 1] [res00, res01],
                      Stmt DivMod [0, 1] [res10, res11]
                    ]
                    [ProgRes IntType 4, ProgRes IntType 5],
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
                [ProgArg IntType "x", ProgArg IntType "y"]
                [ Stmt Add [0, 1] [2, 3]
                ]
                [ProgRes IntType 2],
            semanticsTestCaseArgs = [1, 2],
            semanticsTestCaseExpected = ErrorResult,
            semanticsTestCaseFreshIdent = "x"
          }
        ]
    return $ testCase name $ do
      let actual =
            flip runFreshT ident $ runProg TestSemanticsObj prog args ::
              SymbolicContext [SymInteger]
      let processedActual = actual `catchError` const (raiseError "Error")
      case expected of
        ErrorResult -> processedActual .@?= raiseError "Error"
        Result preCond expectedIntegers -> do
          let expected =
                mrgIf preCond (result expectedIntegers) (raiseError "Error")
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
            )