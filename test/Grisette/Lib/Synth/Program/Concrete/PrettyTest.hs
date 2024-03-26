{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Grisette.Lib.Synth.Program.Concrete.PrettyTest (prettyTest) where

import Control.Arrow (Arrow (first))
import Control.Monad.State (StateT (runStateT))
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Grisette (GPretty (gpretty))
import Grisette.Lib.Synth.Program.Concrete
  ( OpPrettyError (RedefinedResult, UndefinedArgument),
    Prog (Prog),
    ProgArg (ProgArg),
    ProgPrettyError (ResultUndefined, StmtPrettyError),
    ProgRes (ProgRes),
    Stmt (Stmt),
    VarIdMap,
    prettyProg,
    prettyStmt,
  )
import Grisette.Lib.Synth.TestOperator.TestPrettyOperator
  ( TestPrettyExtOp (TestPrettyExtOp),
    TestPrettyOp (PrettyInvokeExtOp, PrettyInvokeOp, PrettyOp1, PrettyOp2),
    TestPrettyType (PrettyType1, PrettyType2),
  )
import Grisette.Lib.Synth.Util.Pretty (renderDoc)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

env :: VarIdMap Int
env = HM.fromList [(0, "x"), (1, "y")]

data PrettyStmtTestCase = PrettyStmtTestCase
  { testStmtGroupName :: String,
    testStmt :: Stmt TestPrettyOp Int,
    testStmtIndex :: Int,
    testStmtLooseExpectedResult ::
      Either (ProgPrettyError Int TestPrettyOp) T.Text,
    testStmtCompactExpectedResult ::
      Either (ProgPrettyError Int TestPrettyOp) T.Text,
    testStmtNewMap :: VarIdMap Int
  }

data PrettyProgTestCase = PrettyProgTestCase
  { testProgGroupName :: String,
    testProg :: Prog TestPrettyOp Int TestPrettyType,
    testProgLooseExpectedResult ::
      Either (ProgPrettyError Int TestPrettyOp) T.Text,
    testProgCompactExpectedResult ::
      Either (ProgPrettyError Int TestPrettyOp) T.Text
  }

prettyTest :: Test
prettyTest =
  testGroup
    "pretty printing"
    [ testGroup "prettyStmt" $ do
        (PrettyStmtTestCase groupName stmt index loose compact newMap) <-
          [ PrettyStmtTestCase
              { testStmtGroupName = "1 ret, 1 arg",
                testStmt = Stmt PrettyOp1 [0] [2],
                testStmtIndex = 3,
                testStmtLooseExpectedResult = Right "t1_2 = op1(op1=x)",
                testStmtCompactExpectedResult =
                  Right "t1_2 = op1(\n  op1=x\n)",
                testStmtNewMap = HM.union env $ HM.fromList [(2, "t1_2")]
              },
            PrettyStmtTestCase
              { testStmtGroupName = "2 ret, 2 arg",
                testStmt = Stmt PrettyOp2 [0, 1] [2, 3],
                testStmtIndex = 3,
                testStmtLooseExpectedResult =
                  Right "(op2_2, op2'_3) = op2(op2'2'0'arg=x, y)",
                testStmtCompactExpectedResult =
                  Right $
                    T.intercalate
                      "\n"
                      [ "(",
                        "  op2_2,",
                        "  op2'_3",
                        ") = op2(",
                        "  op2'2'0'arg=x,",
                        "  y",
                        ")"
                      ],
                testStmtNewMap =
                  HM.union env $ HM.fromList [(2, "op2_2"), (3, "op2'_3")]
              },
            PrettyStmtTestCase
              { testStmtGroupName = "arg error",
                testStmt = Stmt PrettyOp2 [2] [3],
                testStmtIndex = 3,
                testStmtLooseExpectedResult =
                  Left . StmtPrettyError (Stmt PrettyOp2 [2] [3]) 3 $
                    UndefinedArgument 0 2,
                testStmtCompactExpectedResult =
                  Left . StmtPrettyError (Stmt PrettyOp2 [2] [3]) 3 $
                    UndefinedArgument 0 2,
                testStmtNewMap = env
              },
            PrettyStmtTestCase
              { testStmtGroupName = "res error",
                testStmt = Stmt PrettyOp2 [0, 1] [1, 2],
                testStmtIndex = 3,
                testStmtLooseExpectedResult =
                  Left . StmtPrettyError (Stmt PrettyOp2 [0, 1] [1, 2]) 3 $
                    RedefinedResult 0 1,
                testStmtCompactExpectedResult =
                  Left . StmtPrettyError (Stmt PrettyOp2 [0, 1] [1, 2]) 3 $
                    RedefinedResult 0 1,
                testStmtNewMap = env
              }
            ]
        return $ testGroup groupName $ do
          let doc = flip runStateT env $ prettyStmt index stmt
          [ testCase "loose" $ do
              first (renderDoc 80) <$> doc @?= ((,newMap) <$> loose),
            testCase "compact" $
              first (renderDoc 1) <$> doc @?= ((,newMap) <$> compact)
            ],
      testGroup "prettyProg" $ do
        (PrettyProgTestCase groupName prog loose compact) <-
          [ PrettyProgTestCase
              { testProgGroupName = "0 stmt",
                testProg = Prog "prog1" [] [] [],
                testProgLooseExpectedResult = Right "def prog1():\n  return ()",
                testProgCompactExpectedResult =
                  Right "def prog1():\n  return ()"
              },
            PrettyProgTestCase
              { testProgGroupName = "1 stmt",
                testProg =
                  Prog
                    "prog2"
                    [ProgArg "x" 0 PrettyType1]
                    [Stmt PrettyOp1 [0] [1]]
                    [ProgRes 1 PrettyType2],
                testProgLooseExpectedResult =
                  Right $
                    T.intercalate
                      "\n"
                      [ "def prog2(x: PrettyType1):",
                        "  t1_1 = op1(op1=x)",
                        "  return t1_1"
                      ],
                testProgCompactExpectedResult =
                  Right $
                    T.intercalate
                      "\n"
                      [ "def prog2(",
                        "  x: PrettyType1",
                        "):",
                        "  t1_1 = op1(",
                        "    op1=x",
                        "  )",
                        "  return t1_1"
                      ]
              },
            PrettyProgTestCase
              { testProgGroupName = "2 stmts",
                testProg =
                  Prog
                    "prog3"
                    [ProgArg "x" 0 PrettyType1, ProgArg "y" 1 PrettyType2]
                    [ Stmt PrettyOp2 [0, 1] [2, 3],
                      Stmt PrettyOp1 [3] [4]
                    ]
                    [ProgRes 4 PrettyType1, ProgRes 2 PrettyType2],
                testProgLooseExpectedResult =
                  Right $
                    T.intercalate
                      "\n"
                      [ "def prog3(x: PrettyType1, y: PrettyType2):",
                        "  (op2_2, op2'_3) = op2(op2'2'0'arg=x, y)",
                        "  t1_4 = op1(op1=op2'_3)",
                        "  return (t1_4, op2_2)"
                      ],
                testProgCompactExpectedResult =
                  Right $
                    T.intercalate
                      "\n"
                      [ "def prog3(",
                        "  x: PrettyType1,",
                        "  y: PrettyType2",
                        "):",
                        "  (",
                        "    op2_2,",
                        "    op2'_3",
                        "  ) = op2(",
                        "    op2'2'0'arg=x,",
                        "    y",
                        "  )",
                        "  t1_4 = op1(",
                        "    op1=op2'_3",
                        "  )",
                        "  return (",
                        "    t1_4,",
                        "    op2_2",
                        "  )"
                      ]
              },
            PrettyProgTestCase
              { testProgGroupName = "stmt error",
                testProg =
                  Prog
                    "prog4"
                    [ProgArg "x" 0 PrettyType1, ProgArg "y" 1 PrettyType2]
                    [Stmt PrettyOp2 [0, 1] [1, 2]]
                    [ProgRes 0 PrettyType1],
                testProgLooseExpectedResult =
                  Left . StmtPrettyError (Stmt PrettyOp2 [0, 1] [1, 2]) 0 $
                    RedefinedResult 0 1,
                testProgCompactExpectedResult =
                  Left . StmtPrettyError (Stmt PrettyOp2 [0, 1] [1, 2]) 0 $
                    RedefinedResult 0 1
              },
            PrettyProgTestCase
              { testProgGroupName = "result undefined",
                testProg =
                  Prog
                    "prog5"
                    [ProgArg "x" 0 PrettyType1]
                    [Stmt PrettyOp1 [0] [1]]
                    [ProgRes 2 PrettyType1],
                testProgLooseExpectedResult = Left $ ResultUndefined 0 2,
                testProgCompactExpectedResult = Left $ ResultUndefined 0 2
              }
            ]
        return $ testGroup groupName $ do
          let doc = prettyProg prog
          [ testCase "loose" $ renderDoc 80 <$> doc @?= loose,
            testCase "compact" $ renderDoc 1 <$> doc @?= compact
            ],
      testGroup "gpretty" $ do
        let progExt =
              Prog
                "ext"
                [ProgArg "x" 0 PrettyType1]
                [Stmt TestPrettyExtOp [0] [1, 2]]
                [ProgRes 1 PrettyType1]
        let prog1 =
              Prog
                "prog1"
                [ProgArg "x" 0 PrettyType1]
                [Stmt (PrettyInvokeExtOp progExt) [0] [1]]
                [ProgRes 1 PrettyType1]
        let prog2 =
              Prog
                "prog2"
                [ProgArg "x" (0 :: Int) PrettyType1]
                [ Stmt (PrettyInvokeExtOp progExt) [0] [1],
                  Stmt (PrettyInvokeOp prog1) [1] [2]
                ]
                [ProgRes 2 PrettyType1]
        let doc = gpretty prog2
        [ testCase "loose" $ do
            let actual = renderDoc 80 doc
            let expected =
                  T.intercalate
                    "\n"
                    [ "def ext(x: PrettyType1):",
                      "  (t1_1, t2_2) = ext(x)",
                      "  return t1_1",
                      "def prog1(x: PrettyType1):",
                      "  t1_1 = invoke_ext(ext)(x)",
                      "  return t1_1",
                      "def prog2(x: PrettyType1):",
                      "  t1_1 = invoke_ext(ext)(x)",
                      "  t1_2 = invoke(prog1)(t1_1)",
                      "  return t1_2"
                    ]
            actual @?= expected
          ]
    ]
