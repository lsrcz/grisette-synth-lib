{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.Concrete.TopologicalSortTest
  ( topologicalSortTest,
    prog3,
  )
where

import qualified Data.Map.Ordered as OM
import Grisette.Lib.Synth.Program.Concrete
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
    topologicalGPrettyProg,
    topologicalProgToDot,
  )
import Grisette.Lib.Synth.TestOperator.TestPrettyOperator
  ( TestPrettyExtOp (TestPrettyExtOp),
    TestPrettyOp (PrettyInvokeExtOp, PrettyInvokeOp),
    TestPrettyType (PrettyType1),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

progExt :: Prog TestPrettyExtOp Int TestPrettyType
progExt =
  Prog
    "ext"
    [ProgArg "x" 0 PrettyType1]
    [Stmt TestPrettyExtOp [0] [1, 2]]
    [ProgRes 1 PrettyType1]

prog1 :: Prog TestPrettyOp Int TestPrettyType
prog1 =
  Prog
    "prog1"
    [ProgArg "x" 0 PrettyType1]
    [Stmt (PrettyInvokeExtOp progExt) [0] [1]]
    [ProgRes 1 PrettyType1]

prog2 :: Prog TestPrettyOp Int TestPrettyType
prog2 =
  Prog
    "prog2"
    [ProgArg "x" 0 PrettyType1]
    [ Stmt (PrettyInvokeExtOp progExt) [0] [1],
      Stmt (PrettyInvokeOp prog1) [1] [2]
    ]
    [ProgRes 2 PrettyType1]

prog3 :: Prog TestPrettyOp Int TestPrettyType
prog3 =
  Prog
    "prog3"
    [ProgArg "x" 0 PrettyType1]
    [ Stmt (PrettyInvokeExtOp progExt) [0] [1],
      Stmt (PrettyInvokeOp prog1) [1] [2],
      Stmt (PrettyInvokeOp prog2) [2] [3]
    ]
    [ProgRes 3 PrettyType1]

topologicalSortTest :: Test
topologicalSortTest =
  testGroup
    "TopologicalSort"
    [ testCase "topologicalGPrettyProg" $ do
        let sorted = topologicalGPrettyProg prog3 OM.empty
        fst <$> OM.assocs sorted @?= ["ext", "prog1", "prog2", "prog3"],
      testCase "topologicalProgToDot" $ do
        let sorted = topologicalProgToDot prog3 OM.empty
        fst <$> OM.assocs sorted @?= ["ext", "prog1", "prog2", "prog3"]
    ]
