{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Grisette.Lib.Synth.Program.Concrete.PrettyTest (prettyTest) where

import Control.Arrow (Arrow (first))
import Control.Monad.State (StateT (runStateT))
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Grisette (GPretty (gpretty))
import Grisette.Lib.Synth.Operator.OpPretty
  ( OpPrettyError (RedefinedResult, UndefinedArgument),
    VarIdMap,
  )
import Grisette.Lib.Synth.Program.Concrete
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgPrettyError (ResultUndefined, StmtPrettyError),
    ProgRes (ProgRes),
    Stmt (Stmt),
    prettyProg,
    prettyStmt,
  )
import Grisette.Lib.Synth.TestOperator.TestPrettyOperator
  ( TestPrettyExtOp (TestPrettyExtOp),
    TestPrettyOp (PrettyInvokeExtOp, PrettyInvokeOp, PrettyOp2),
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
      Either (ProgPrettyError TestPrettyOp Int) T.Text,
    testStmtCompactExpectedResult ::
      Either (ProgPrettyError TestPrettyOp Int) T.Text,
    testStmtNewMap :: VarIdMap Int
  }

data PrettyProgTestCase = PrettyProgTestCase
  { testProgGroupName :: String,
    testProg :: Prog TestPrettyOp Int TestPrettyType,
    testProgLooseExpectedResult ::
      Either (ProgPrettyError TestPrettyOp Int) T.Text,
    testProgCompactExpectedResult ::
      Either (ProgPrettyError TestPrettyOp Int) T.Text
  }

prettyTest :: Test
prettyTest =
  testGroup
    "pretty printing"
    [ testGroup "prettyStmt" $ do
        (PrettyStmtTestCase groupName stmt index loose compact newMap) <-
          [ PrettyStmtTestCase
              { testStmtGroupName = "0 ret, 0 arg",
                testStmt = Stmt PrettyOp2 [] [],
                testStmtIndex = 3,
                testStmtLooseExpectedResult = Right "() = op2()",
                testStmtCompactExpectedResult = Right "() = op2()",
                testStmtNewMap = env
              },
            PrettyStmtTestCase
              { testStmtGroupName = "1 ret, 1 arg",
                testStmt = Stmt PrettyOp2 [0] [2],
                testStmtIndex = 3,
                testStmtLooseExpectedResult = Right "op2'1'res2 = op2(x)",
                testStmtCompactExpectedResult =
                  Right "op2'1'res2 = op2(\n  x\n)",
                testStmtNewMap = HM.union env $ HM.fromList [(2, "op2'1'res2")]
              },
            PrettyStmtTestCase
              { testStmtGroupName = "2 ret, 2 arg",
                testStmt = Stmt PrettyOp2 [0, 1] [2, 3],
                testStmtIndex = 3,
                testStmtLooseExpectedResult =
                  Right "(op2'2'0'res2, op2'2'1'res3) = op2(op2'2'0'arg=x, y)",
                testStmtCompactExpectedResult =
                  Right $
                    T.intercalate
                      "\n"
                      [ "(",
                        "  op2'2'0'res2,",
                        "  op2'2'1'res3",
                        ") = op2(",
                        "  op2'2'0'arg=x,",
                        "  y",
                        ")"
                      ],
                testStmtNewMap =
                  HM.union env $
                    HM.fromList [(2, "op2'2'0'res2"), (3, "op2'2'1'res3")]
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
                testStmt = Stmt PrettyOp2 [0] [1],
                testStmtIndex = 3,
                testStmtLooseExpectedResult =
                  Left . StmtPrettyError (Stmt PrettyOp2 [0] [1]) 3 $
                    RedefinedResult 0 1,
                testStmtCompactExpectedResult =
                  Left . StmtPrettyError (Stmt PrettyOp2 [0] [1]) 3 $
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
                    [ProgArg PrettyType1 "x" 0]
                    [Stmt PrettyOp2 [0] [1]]
                    [ProgRes PrettyType2 1],
                testProgLooseExpectedResult =
                  Right $
                    T.intercalate
                      "\n"
                      [ "def prog2(x: PrettyType1):",
                        "  op2'1'res1 = op2(x)",
                        "  return op2'1'res1"
                      ],
                testProgCompactExpectedResult =
                  Right $
                    T.intercalate
                      "\n"
                      [ "def prog2(",
                        "  x: PrettyType1",
                        "):",
                        "  op2'1'res1 = op2(",
                        "    x",
                        "  )",
                        "  return op2'1'res1"
                      ]
              },
            PrettyProgTestCase
              { testProgGroupName = "2 stmts",
                testProg =
                  Prog
                    "prog3"
                    [ProgArg PrettyType1 "x" 0, ProgArg PrettyType2 "y" 1]
                    [ Stmt PrettyOp2 [0, 1] [2, 3],
                      Stmt PrettyOp2 [2] [4]
                    ]
                    [ProgRes PrettyType1 4, ProgRes PrettyType2 3],
                testProgLooseExpectedResult =
                  Right $
                    T.intercalate
                      "\n"
                      [ "def prog3(x: PrettyType1, y: PrettyType2):",
                        "  (op2'2'0'res2, op2'2'1'res3) = "
                          <> "op2(op2'2'0'arg=x, y)",
                        "  op2'1'res4 = op2(op2'2'0'res2)",
                        "  return (op2'1'res4, op2'2'1'res3)"
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
                        "    op2'2'0'res2,",
                        "    op2'2'1'res3",
                        "  ) = op2(",
                        "    op2'2'0'arg=x,",
                        "    y",
                        "  )",
                        "  op2'1'res4 = op2(",
                        "    op2'2'0'res2",
                        "  )",
                        "  return (",
                        "    op2'1'res4,",
                        "    op2'2'1'res3",
                        "  )"
                      ]
              },
            PrettyProgTestCase
              { testProgGroupName = "stmt error",
                testProg =
                  Prog
                    "prog4"
                    [ProgArg PrettyType1 "x" 0, ProgArg PrettyType1 "y" 1]
                    [Stmt PrettyOp2 [0] [1]]
                    [ProgRes PrettyType1 1],
                testProgLooseExpectedResult =
                  Left . StmtPrettyError (Stmt PrettyOp2 [0] [1]) 0 $
                    RedefinedResult 0 1,
                testProgCompactExpectedResult =
                  Left . StmtPrettyError (Stmt PrettyOp2 [0] [1]) 0 $
                    RedefinedResult 0 1
              },
            PrettyProgTestCase
              { testProgGroupName = "result undefined",
                testProg =
                  Prog
                    "prog5"
                    [ProgArg PrettyType1 "x" 0]
                    [Stmt PrettyOp2 [0] [1]]
                    [ProgRes PrettyType1 2],
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
                [ProgArg PrettyType1 "x" 0]
                [Stmt TestPrettyExtOp [0] [1]]
                [ProgRes PrettyType1 1]
        let prog1 =
              Prog
                "prog1"
                [ProgArg PrettyType1 "x" 0]
                [Stmt (PrettyInvokeExtOp progExt) [0] [1]]
                [ProgRes PrettyType1 1]
        let prog2 =
              Prog
                "prog2"
                [ProgArg PrettyType1 "x" (0 :: Int)]
                [ Stmt (PrettyInvokeExtOp progExt) [0] [1],
                  Stmt (PrettyInvokeOp prog1) [1] [2]
                ]
                [ProgRes PrettyType1 2]
        let doc = gpretty prog2
        [ testCase "loose" $ do
            let actual = renderDoc 80 doc
            let expected =
                  T.intercalate
                    "\n"
                    [ "def ext(x: PrettyType1):",
                      "  o1 = ext(x)",
                      "  return o1",
                      "def prog1(x: PrettyType1):",
                      "  o1 = invoke_ext(ext)(x)",
                      "  return o1",
                      "def prog2(x: PrettyType1):",
                      "  o1 = invoke_ext(ext)(x)",
                      "  o2 = invoke(prog1)(o1)",
                      "  return o2"
                    ]
            actual @?= expected
          ]
    ]
