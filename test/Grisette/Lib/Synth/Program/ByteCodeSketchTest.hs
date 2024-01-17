{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.ByteCodeSketchTest (byteCodeSketchTest) where

import Control.Monad.Error.Class (MonadError (catchError))
import qualified Data.Text as T
import Grisette
  ( ITEOp (symIte),
    LogicalOp ((.||)),
    SEq ((.==)),
    Solvable (con),
    SymBool,
    SymInteger,
    ToCon (toCon),
    mrgIf,
    mrgReturn,
  )
import Grisette.Lib.Synth.Context
  ( MonadContext (raiseError, result),
    SymbolicContext,
  )
import Grisette.Lib.Synth.Program.ByteCodeSketch
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsObj (TestSemanticsObj),
    TestSemanticsOp (Add, DivMod, Inc),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Test.SymbolicAssertion ((.@?=))

data ToConTestCase = ToConTestCase
  { toConTestCaseName :: String,
    toConTestCaseProg ::
      Prog TestSemanticsOp Integer SymInteger TestSemanticsType,
    toConTestCaseExpected ::
      Maybe (Concrete.Prog TestSemanticsOp Integer TestSemanticsType)
  }

data ExpectedResult
  = ErrorResult T.Text
  | Result SymBool [SymInteger]

data SemanticsTestCase = SemanticsTestCase
  { semanticsTestCaseName :: String,
    semanticsTestCaseProg ::
      Prog TestSemanticsOp Integer SymInteger TestSemanticsType,
    semanticsTestCaseArgs :: [SymInteger],
    semanticsTestCaseExpected :: ExpectedResult
  }

goodConcreteProg :: Prog TestSemanticsOp Integer SymInteger TestSemanticsType
goodConcreteProg =
  Prog
    "test"
    [ProgArg IntType "x" 0, ProgArg IntType "y" 1]
    [ Stmt (mrgReturn Add) [0, 1] 2 [3] 1,
      Stmt (mrgReturn DivMod) [3, 0] 2 [4, 5] 2
    ]
    [ProgRes IntType 4, ProgRes IntType 5]

byteCodeSketchTest :: Test
byteCodeSketchTest =
  testGroup
    "Grisette.Lib.Synth.Program.ByteCodeSketch"
    [ testGroup "ToCon" $ do
        ToConTestCase name prog expected <-
          [ ToConTestCase
              { toConTestCaseName = "goodConcreteProg",
                toConTestCaseProg = goodConcreteProg,
                toConTestCaseExpected =
                  Just $
                    Concrete.Prog
                      "test"
                      [ Concrete.ProgArg IntType "x" 0,
                        Concrete.ProgArg IntType "y" 1
                      ]
                      [ Concrete.Stmt Add [0, 1] [3],
                        Concrete.Stmt DivMod [3, 0] [4, 5]
                      ]
                      [Concrete.ProgRes IntType 4, Concrete.ProgRes IntType 5]
              },
            ToConTestCase
              { toConTestCaseName = "argNum is less than number of args",
                toConTestCaseProg =
                  Prog
                    "test"
                    [ProgArg IntType "x" 0, ProgArg IntType "y" 1]
                    [ Stmt (mrgReturn Inc) [0, 1] 1 [2, 3] 1
                    ]
                    [ProgRes IntType 2],
                toConTestCaseExpected =
                  Just $
                    Concrete.Prog
                      "test"
                      [ Concrete.ProgArg IntType "x" 0,
                        Concrete.ProgArg IntType "y" 1
                      ]
                      [Concrete.Stmt Inc [0] [2]]
                      [Concrete.ProgRes IntType 2]
              }
            ]
        return $ testCase name $ toCon prog @?= expected,
      testGroup "Semantics" $ do
        SemanticsTestCase name prog args expected <-
          [ SemanticsTestCase
              { semanticsTestCaseName = "concrete program",
                semanticsTestCaseProg = goodConcreteProg,
                semanticsTestCaseArgs = [13, 20],
                semanticsTestCaseExpected = Result (con True) [2, 7]
              },
            SemanticsTestCase
              { semanticsTestCaseName =
                  "symbolic number of statement arguments",
                semanticsTestCaseProg =
                  Prog
                    "test"
                    [ProgArg IntType "x" 0, ProgArg IntType "y" 1]
                    [Stmt (mrgReturn Add) [0, 1] "a" [2] 1]
                    [ProgRes IntType 2],
                semanticsTestCaseArgs = [13, 20],
                semanticsTestCaseExpected =
                  Result (("a" :: SymInteger) .== 2) [33]
              },
            SemanticsTestCase
              { semanticsTestCaseName = "symbolic statement argument",
                semanticsTestCaseProg =
                  Prog
                    "test"
                    [ProgArg IntType "x" 0, ProgArg IntType "y" 1]
                    [Stmt (mrgReturn Add) ["a", 1] 2 [2] 1]
                    [ProgRes IntType 2],
                semanticsTestCaseArgs = [13, 20],
                semanticsTestCaseExpected =
                  let a = "a" :: SymInteger
                   in Result (a .== 0 .|| a .== 1) [symIte (a .== 0) 33 40]
              },
            SemanticsTestCase
              { semanticsTestCaseName =
                  "symbolic number of statement results",
                semanticsTestCaseProg =
                  Prog
                    "test"
                    [ProgArg IntType "x" 0, ProgArg IntType "y" 1]
                    [Stmt (mrgReturn Add) [0, 1] 2 [2] "a"]
                    [ProgRes IntType 2],
                semanticsTestCaseArgs = [13, 20],
                semanticsTestCaseExpected =
                  Result (("a" :: SymInteger) .== 1) [33]
              },
            SemanticsTestCase
              { semanticsTestCaseName = "symbolic result",
                semanticsTestCaseProg =
                  Prog
                    "test"
                    [ProgArg IntType "x" 0, ProgArg IntType "y" 1]
                    [Stmt (mrgReturn Add) [0, 1] 2 [2] 1]
                    [ProgRes IntType "a"],
                semanticsTestCaseArgs = [13, 20],
                semanticsTestCaseExpected =
                  let a = "a" :: SymInteger
                   in Result
                        (a .== 0 .|| a .== 1 .|| a .== 2)
                        [symIte (a .== 0) 13 $ symIte (a .== 1) 20 33]
              },
            SemanticsTestCase
              { semanticsTestCaseName = "divByZero",
                semanticsTestCaseProg = goodConcreteProg,
                semanticsTestCaseArgs = [0, 20],
                semanticsTestCaseExpected =
                  ErrorResult "ArithException: divide by zero"
              },
            SemanticsTestCase
              { semanticsTestCaseName = "incorrect number of arguments",
                semanticsTestCaseProg = goodConcreteProg,
                semanticsTestCaseArgs = [0],
                semanticsTestCaseExpected =
                  ErrorResult "Expected 2 arguments, but got 1 arguments."
              },
            SemanticsTestCase
              { semanticsTestCaseName = "incorrect number of statement results",
                semanticsTestCaseProg =
                  Prog
                    "test"
                    [ProgArg IntType "x" 0, ProgArg IntType "y" 1]
                    [Stmt (mrgReturn Add) [0, 1] 2 [2, 3] 2]
                    [ProgRes IntType 2],
                semanticsTestCaseArgs = [1, 2],
                semanticsTestCaseExpected =
                  ErrorResult "Incorrect number of results."
              },
            SemanticsTestCase
              { semanticsTestCaseName =
                  "incorrect number of statement arguments",
                semanticsTestCaseProg =
                  Prog
                    "test"
                    [ProgArg IntType "x" 0, ProgArg IntType "y" 1]
                    [Stmt (mrgReturn Add) [0] 1 [2] 1]
                    [ProgRes IntType 2],
                semanticsTestCaseArgs = [1, 2],
                semanticsTestCaseExpected =
                  ErrorResult $
                    "Incorrect number of arguments for add, expected 2 "
                      <> "arguments, but got 1 arguments."
              },
            SemanticsTestCase
              { semanticsTestCaseName = "Redefinition of variable",
                semanticsTestCaseProg =
                  Prog
                    "test"
                    [ProgArg IntType "x" 0, ProgArg IntType "x" 1]
                    [Stmt (mrgReturn Add) [0, 1] 2 [1] 1]
                    [ProgRes IntType 1],
                semanticsTestCaseArgs = [1, 2],
                semanticsTestCaseExpected =
                  ErrorResult "Variable 1 is already defined."
              },
            SemanticsTestCase
              { semanticsTestCaseName = "Undefined variable",
                semanticsTestCaseProg =
                  Prog
                    "test"
                    [ProgArg IntType "x" 0]
                    [Stmt (mrgReturn Add) [0, 1] 2 [2] 1]
                    [ProgRes IntType 2],
                semanticsTestCaseArgs = [1],
                semanticsTestCaseExpected =
                  ErrorResult "Variable is undefined."
              },
            SemanticsTestCase
              { semanticsTestCaseName = "Undefined result",
                semanticsTestCaseProg =
                  Prog
                    "test"
                    [ProgArg IntType "x" 0, ProgArg IntType "y" 1]
                    [Stmt (mrgReturn Add) [0, 1] 2 [2] 1]
                    [ProgRes IntType 3],
                semanticsTestCaseArgs = [1, 2],
                semanticsTestCaseExpected =
                  ErrorResult "Variable is undefined."
              },
            SemanticsTestCase
              { semanticsTestCaseName = "Extra statement arg num",
                semanticsTestCaseProg =
                  Prog
                    "test"
                    [ProgArg IntType "x" 0, ProgArg IntType "y" 1]
                    [Stmt (mrgReturn Add) [0, 1, 1] 2 [2] 1]
                    [ProgRes IntType 2],
                semanticsTestCaseArgs = [1, 2],
                semanticsTestCaseExpected = Result (con True) [3]
              },
            SemanticsTestCase
              { semanticsTestCaseName = "Insufficient statement args",
                semanticsTestCaseProg =
                  Prog
                    "test"
                    [ProgArg IntType "x" 0, ProgArg IntType "y" 1]
                    [Stmt (mrgReturn Add) [0] 2 [2] 1]
                    [ProgRes IntType 2],
                semanticsTestCaseArgs = [1, 2],
                semanticsTestCaseExpected =
                  ErrorResult "The specified argument number is too large."
              },
            SemanticsTestCase
              { semanticsTestCaseName = "Insufficient result ids",
                semanticsTestCaseProg =
                  Prog
                    "test"
                    [ProgArg IntType "x" 0, ProgArg IntType "y" 1]
                    [Stmt (mrgReturn DivMod) [0, 1] 2 [2] 2]
                    [ProgRes IntType 2],
                semanticsTestCaseArgs = [1, 2],
                semanticsTestCaseExpected =
                  ErrorResult "Insufficient result IDs."
              }
            ]
        return $ testCase name $ do
          let actual =
                runProg TestSemanticsObj prog args ::
                  SymbolicContext [SymInteger]
          case expected of
            ErrorResult expectedError -> actual .@?= raiseError expectedError
            Result preCond expectedIntegers -> do
              let processedActual =
                    actual `catchError` const (raiseError "Error")
              let expected =
                    mrgIf preCond (result expectedIntegers) (raiseError "Error")
              processedActual .@?= expected
    ]
