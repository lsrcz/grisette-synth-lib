{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.Concrete.FlattenTest (flattenTest) where

import Grisette.Lib.Synth.Program.Concrete
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import Grisette.Lib.Synth.Program.Concrete.Flatten (flattenProg)
import Grisette.Lib.Synth.TestOperator.TestPrettyOperator
  ( TestPrettyOp (PrettyInvokeOp, PrettyOp0, PrettyOp1, PrettyOp2),
    TestPrettyType (PrettyType1, PrettyType2),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

flattenTest :: Test
flattenTest =
  testGroup
    "Flatten"
    [ testCase "No sub program" $ do
        let prog =
              Prog
                "test"
                [ProgArg "a" 20 PrettyType1, ProgArg "b" 10 PrettyType2]
                [ Stmt PrettyOp2 [20, 10] [40, 30 :: Int],
                  Stmt PrettyOp2 [40, 30] [0, 1]
                ]
                [ProgRes 1 PrettyType1, ProgRes 0 PrettyType2]
        let expected =
              Prog
                "test"
                [ProgArg "a" 0 PrettyType1, ProgArg "b" 1 PrettyType2]
                [ Stmt PrettyOp2 [0, 1] [2, 3],
                  Stmt PrettyOp2 [2, 3] [4, 5 :: Int]
                ]
                [ProgRes 5 PrettyType1, ProgRes 4 PrettyType2]
        flattenProg prog @?= return expected,
      testCase "With sub program" $ do
        let subProg =
              Prog
                "subProg"
                [ProgArg "a" 20 PrettyType1, ProgArg "b" 10 PrettyType2]
                [ Stmt PrettyOp2 [20, 10] [40, 30],
                  Stmt PrettyOp2 [40, 30] [0, 1]
                ]
                [ProgRes 1 PrettyType1, ProgRes 0 PrettyType2]
        let prog =
              Prog
                "prog"
                [ProgArg "b" 4 PrettyType2]
                [ Stmt PrettyOp0 [] [20 :: Int],
                  Stmt (PrettyInvokeOp subProg) [20, 4] [0, 1],
                  Stmt PrettyOp1 [0] [2],
                  Stmt (PrettyInvokeOp subProg) [2, 1] [5, 6]
                ]
                [ProgRes 2 PrettyType1, ProgRes 5 PrettyType2]
        let expected =
              Prog
                "prog"
                [ProgArg "b" 0 PrettyType2]
                [ Stmt PrettyOp0 [] [1],
                  Stmt PrettyOp2 [1, 0] [2, 3],
                  Stmt PrettyOp2 [2, 3] [4, 5],
                  Stmt PrettyOp1 [5] [6],
                  Stmt PrettyOp2 [6, 4] [7, 8],
                  Stmt PrettyOp2 [7, 8] [9, 10 :: Int]
                ]
                [ProgRes 6 PrettyType1, ProgRes 10 PrettyType2]
        flattenProg prog @?= return expected
    ]