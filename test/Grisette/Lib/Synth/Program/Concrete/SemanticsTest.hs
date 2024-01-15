{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.Concrete.SemanticsTest (semanticsTest) where

import Control.Monad.Error.Class (liftEither)
import qualified Data.Text as T
import Grisette (SymInteger, ToSym (toSym))
import Grisette.Lib.Synth.Context (ConcreteContext, SymbolicContext)
import Grisette.Lib.Synth.Program.Concrete
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgMayMultiPath (ProgMayMultiPath),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsObj (TestSemanticsObj),
    TestSemanticsOp (Add, DivMod),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

data SemanticsTestCase = SemanticsTestCase
  { semanticsTestCaseName :: String,
    semanticsTestCaseProg :: Prog TestSemanticsOp Integer TestSemanticsType,
    semanticsTestCaseArgs :: [Integer],
    semanticsTestCaseExpected :: Either T.Text [Integer]
  }

semanticsTest :: Test
semanticsTest =
  testGroup "semantics" $ do
    let goodProg =
          Prog
            "test"
            [ProgArg IntType "x" 0, ProgArg IntType "y" 1]
            [ Stmt Add [0, 1] [3],
              Stmt DivMod [3, 0] [4, 5]
            ]
            [ProgRes IntType 4, ProgRes IntType 5] ::
            Prog TestSemanticsOp Integer TestSemanticsType
    SemanticsTestCase name prog args expected <-
      [ SemanticsTestCase
          { semanticsTestCaseName = "runProg",
            semanticsTestCaseProg = goodProg,
            semanticsTestCaseArgs = [13, 20],
            semanticsTestCaseExpected = Right [2, 7]
          },
        SemanticsTestCase
          { semanticsTestCaseName = "divByZero",
            semanticsTestCaseProg = goodProg,
            semanticsTestCaseArgs = [0, 20],
            semanticsTestCaseExpected = Left "ArithException: divide by zero"
          },
        SemanticsTestCase
          { semanticsTestCaseName = "incorrect number of arguments",
            semanticsTestCaseProg = goodProg,
            semanticsTestCaseArgs = [0],
            semanticsTestCaseExpected =
              Left "Expected 2 arguments, but got 1 arguments."
          },
        SemanticsTestCase
          { semanticsTestCaseName = "Redefinition of variable",
            semanticsTestCaseProg =
              Prog
                "test"
                [ProgArg IntType "x" 0, ProgArg IntType "x" 1]
                [Stmt Add [0, 1] [1]]
                [ProgRes IntType 1] ::
                Prog TestSemanticsOp Integer TestSemanticsType,
            semanticsTestCaseArgs = [1, 2],
            semanticsTestCaseExpected = Left "Variable 1 is already defined."
          },
        SemanticsTestCase
          { semanticsTestCaseName = "Undefined variable",
            semanticsTestCaseProg =
              Prog
                "test"
                [ProgArg IntType "x" 0]
                [Stmt Add [0, 1] [2]]
                [ProgRes IntType 2] ::
                Prog TestSemanticsOp Integer TestSemanticsType,
            semanticsTestCaseArgs = [1],
            semanticsTestCaseExpected = Left "Variable 1 is undefined."
          },
        SemanticsTestCase
          { semanticsTestCaseName = "Undefined result",
            semanticsTestCaseProg =
              Prog
                "test"
                [ProgArg IntType "x" 0, ProgArg IntType "y" 1]
                [Stmt Add [0, 1] [2]]
                [ProgRes IntType 3] ::
                Prog TestSemanticsOp Integer TestSemanticsType,
            semanticsTestCaseArgs = [1, 2],
            semanticsTestCaseExpected = Left "Variable 3 is undefined."
          }
        ]
    [ testCase name $ do
        let actual =
              runProg TestSemanticsObj prog args :: ConcreteContext [Integer]
        actual @?= expected,
      testCase (name <> "-ProgMayMultiPath") $ do
        let actual =
              runProg TestSemanticsObj (ProgMayMultiPath prog) (toSym args) ::
                SymbolicContext [SymInteger]
        actual @?= toSym (liftEither expected :: SymbolicContext [Integer])
      ]
