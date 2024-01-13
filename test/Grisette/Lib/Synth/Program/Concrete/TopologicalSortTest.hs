{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.Concrete.TopologicalSortTest
  ( topologicalSortTest,
  )
where

import Grisette.Lib.Synth.Program.Concrete
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    SomeConstrainedProg (someProgName),
    SomePrettyProg (SomePrettyProg),
    Stmt (Stmt),
    topologicalSortSubProg,
  )
import Grisette.Lib.Synth.TestOperator.TestPrettyOperator
  ( TestPrettyExtOp (TestPrettyExtOp),
    TestPrettyOp (PrettyInvokeExtOp, PrettyInvokeOp),
    TestPrettyType (PrettyType1),
  )
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

progExt :: Prog TestPrettyExtOp Int TestPrettyType
progExt =
  Prog
    "ext"
    [ProgArg PrettyType1 "x" 0]
    [Stmt TestPrettyExtOp [0] [1]]
    [ProgRes PrettyType1 1]

prog1 :: Prog TestPrettyOp Int TestPrettyType
prog1 =
  Prog
    "prog1"
    [ProgArg PrettyType1 "x" 0]
    [Stmt (PrettyInvokeExtOp progExt) [0] [1]]
    [ProgRes PrettyType1 1]

prog2 :: Prog TestPrettyOp Int TestPrettyType
prog2 =
  Prog
    "prog2"
    [ProgArg PrettyType1 "x" 0]
    [ Stmt (PrettyInvokeExtOp progExt) [0] [1],
      Stmt (PrettyInvokeOp prog1) [1] [2]
    ]
    [ProgRes PrettyType1 2]

prog3 :: Prog TestPrettyOp Int TestPrettyType
prog3 =
  Prog
    "prog3"
    [ProgArg PrettyType1 "x" 0]
    [ Stmt (PrettyInvokeExtOp progExt) [0] [1],
      Stmt (PrettyInvokeOp prog1) [1] [2],
      Stmt (PrettyInvokeOp prog2) [2] [3]
    ]
    [ProgRes PrettyType1 3]

topologicalSortTest :: Test
topologicalSortTest = testCase "topologicalSort" $ do
  let sorted = topologicalSortSubProg (SomePrettyProg prog3)
  someProgName <$> sorted @?= ["ext", "prog1", "prog2", "prog3"]
