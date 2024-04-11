{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Lib.Synth.Program.BuiltinProgConstraints.ComponentSymmetryReductionTest
  ( componentSymmetryReductionTest,
  )
where

import Data.Either (isLeft, isRight)
import Grisette
  ( Solvable (con),
    SymBool,
    SymInteger,
    mrgReturn,
    precise,
    solveExcept,
    z3,
  )
import Grisette.Lib.Synth.Context (SymbolicContext)
import Grisette.Lib.Synth.Program.BuiltinProgConstraints.ComponentSymmetryReduction
  ( ComponentStatementUnreorderable (componentStatementUnreorderable),
    canonicalOrderConstraint,
    componentStatementUnreorderable',
    statementsDirectDep,
  )
import Grisette.Lib.Synth.Program.BuiltinProgConstraints.Liveliness
  ( Liveliness (Liveliness),
  )
import Grisette.Lib.Synth.Program.BuiltinProgConstraints.LivelinessTest
  ( LivelinessOp (LivelinessOp),
    Op (OpDef, OpDef2, OpNone, OpUse, OpUse2),
    Type (ConstrainedType, ConstrainedType2, OtherType),
  )
import Grisette.Lib.Synth.Program.ComponentSketch
  ( Prog (Prog, progStmtList),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool, (@?=))

--     node0   node5
--       |      /|
--   +---+-----+ |
--   |   v       v
--   | node1   node4
--   |/   \    /
--   v     v  v
-- node2   node3
--   |      /
--   |     v
--   |   node6
--   |   /
--   v  v
--  node7
--
--
-- Correct topological order is
-- node0, node1, node5, node2, node4, node3, node6, node7
--
-- ret ID:
-- node0: 2
-- node1: 3
-- node5: 4
-- node2: 5
-- node4: 6
-- node3: 7
-- node6: 8
-- node7: 9
--
-- reordered:
-- node0: 2
-- node1: 3
-- node2: 5
-- node3: 7
-- node4: 6
-- node5: 4
-- node6: 8
-- node7: 9
--
-- Direct reachability matrix:
--   0 1 2 3 4 5 6 7
-- 0 F T F F F F F F
-- 1 F F T T F F F F
-- 2 F F F F F F F T
-- 3 F F F F F F T F
-- 4 F F F T F F F F
-- 5 F F T F T F F F
-- 6 F F F F F F F T
-- 7 F F F F F F F F
--
-- Reachability matrix:
--   0 1 2 3 4 5 6 7
-- 0 F T T T F F T T
-- 1 F F T T F F T T
-- 2 F F F F F F F T
-- 3 F F F F F F T T
-- 4 F F F T F F T T
-- 5 F F T T T F T T
-- 6 F F F F F F F T
-- 7 F F F F F F F F
goodConcreteProg :: Prog TestSemanticsOp SymInteger TestSemanticsType
goodConcreteProg =
  Prog
    "test"
    [ProgArg "x" IntType, ProgArg "y" IntType]
    [ Stmt Add [0, 1] 2 [2] 1 (con False) [],
      Stmt Add [0, 2] 2 [3] 1 (con False) [],
      Stmt Add [3, 4] 2 [5] 1 (con False) [],
      Stmt Add [3, 6] 2 [7] 1 (con False) [],
      Stmt Add [0, 4] 2 [6] 1 (con False) [],
      Stmt Add [0, 1] 2 [4] 1 (con False) [],
      Stmt Add [0, 7] 2 [8] 1 (con False) [],
      Stmt Add [5, 8] 2 [9] 1 (con False) []
    ]
    [ProgRes 8 IntType, ProgRes 9 IntType]

goodConcreteProgStmtList :: [Stmt TestSemanticsOp SymInteger]
goodConcreteProgStmtList = progStmtList goodConcreteProg

directReachabilityMatrix :: [[SymBool]]
directReachabilityMatrix =
  fmap con
    <$> [ [False, True, False, False, False, False, False, False],
          [False, False, True, True, False, False, False, False],
          [False, False, False, False, False, False, False, True],
          [False, False, False, False, False, False, True, False],
          [False, False, False, True, False, False, False, False],
          [False, False, True, False, True, False, False, False],
          [False, False, False, False, False, False, False, True],
          [False, False, False, False, False, False, False, False]
        ]

data ComponentStatementUnreorderableTest = ComponentStatementUnreorderableTest
  { componentStatementUnreorderableTestName :: String,
    componentStatementUnreorderableTestProg ::
      Prog Op SymInteger Type,
    componentStatementUnreorderableTestFirstIdx :: Int,
    componentStatementUnreorderableTestSecondIdx :: Int,
    componentStatementUnreorderableTestExpected :: SymBool
  }

data CanonicalOrderConstraintTest where
  CanonicalOrderConstraintTest ::
    ( ComponentStatementUnreorderable
        constrObj
        op
        SymInteger
        ty
        SymbolicContext
    ) =>
    { canonicalOrderConstraintTestName :: String,
      canonicalOrderConstraintTestProg :: Prog op SymInteger ty,
      canonicalOrderConstraintTestConstraintObj :: constrObj,
      canonicalOrderConstraintTestExpected :: Bool
    } ->
    CanonicalOrderConstraintTest

componentSymmetryReductionTest :: Test
componentSymmetryReductionTest =
  testGroup
    "ComponentSymmetryReductionTest"
    [ testCase "statementsDirectDep" $ do
        let actual =
              [ [ if i == j
                    then con False
                    else
                      statementsDirectDep
                        (goodConcreteProgStmtList !! i)
                        (goodConcreteProgStmtList !! j)
                  | j <- [0 .. length goodConcreteProgStmtList - 1]
                ]
                | i <- [0 .. length goodConcreteProgStmtList - 1]
              ]
        actual @?= directReachabilityMatrix,
      testGroup "ComponentStatementUnreorderable" $ do
        ComponentStatementUnreorderableTest
          name
          prog
          firstIdx
          secondIdx
          reorderable <-
          [ ComponentStatementUnreorderableTest
              { componentStatementUnreorderableTestName = "good",
                componentStatementUnreorderableTestProg =
                  Prog
                    "test"
                    [ProgArg "x" OtherType, ProgArg "y" OtherType]
                    [ Stmt OpNone [0] 1 [2] 1 (con False) [],
                      Stmt OpNone [0] 1 [3] 1 (con False) []
                    ]
                    [ProgRes 2 OtherType, ProgRes 3 OtherType],
                componentStatementUnreorderableTestFirstIdx = 0,
                componentStatementUnreorderableTestSecondIdx = 1,
                componentStatementUnreorderableTestExpected = con False
              },
            ComponentStatementUnreorderableTest
              { componentStatementUnreorderableTestName =
                  "firstUsedDefInvalidatedBySecond",
                componentStatementUnreorderableTestProg =
                  Prog
                    "test"
                    [ProgArg "x" ConstrainedType, ProgArg "y" OtherType]
                    [ Stmt OpUse [1, 0] 2 [2] 1 (con False) [],
                      Stmt OpDef [1] 1 [3, 4] 2 (con False) []
                    ]
                    [ProgRes 2 OtherType, ProgRes 3 OtherType],
                componentStatementUnreorderableTestFirstIdx = 0,
                componentStatementUnreorderableTestSecondIdx = 1,
                componentStatementUnreorderableTestExpected = con True
              },
            ComponentStatementUnreorderableTest
              { componentStatementUnreorderableTestName =
                  "firstUnusedDefInvalidatedBySecond",
                componentStatementUnreorderableTestProg =
                  Prog
                    "test"
                    [ ProgArg "x" ConstrainedType,
                      ProgArg "y" ConstrainedType2,
                      ProgArg "z" OtherType
                    ]
                    [ Stmt OpUse2 [2, 1] 2 [3] 1 (con False) [],
                      Stmt OpDef [2] 1 [4, 5] 2 (con False) []
                    ]
                    [ProgRes 3 OtherType, ProgRes 5 ConstrainedType],
                componentStatementUnreorderableTestFirstIdx = 0,
                componentStatementUnreorderableTestSecondIdx = 1,
                componentStatementUnreorderableTestExpected = con False
              },
            ComponentStatementUnreorderableTest
              { componentStatementUnreorderableTestName =
                  "firstUsedDefNotInvalidatedBySecond",
                componentStatementUnreorderableTestProg =
                  Prog
                    "test"
                    [ ProgArg "x" ConstrainedType,
                      ProgArg "y" ConstrainedType2,
                      ProgArg "z" OtherType
                    ]
                    [ Stmt OpUse [2, 0] 2 [3] 1 (con False) [],
                      Stmt OpDef2 [2] 1 [4, 5] 2 (con False) []
                    ]
                    [ProgRes 3 OtherType, ProgRes 4 OtherType],
                componentStatementUnreorderableTestFirstIdx = 0,
                componentStatementUnreorderableTestSecondIdx = 1,
                componentStatementUnreorderableTestExpected = con False
              },
            ComponentStatementUnreorderableTest
              { componentStatementUnreorderableTestName =
                  "usedSecondDefInvalidatedByFirst",
                componentStatementUnreorderableTestProg =
                  Prog
                    "test"
                    [ProgArg "x" OtherType, ProgArg "y" OtherType]
                    [ Stmt OpDef [0] 1 [2, 3] 2 (con False) [],
                      Stmt OpDef [0] 1 [4, 5] 2 (con False) []
                    ]
                    [ProgRes 3 ConstrainedType, ProgRes 5 ConstrainedType],
                componentStatementUnreorderableTestFirstIdx = 0,
                componentStatementUnreorderableTestSecondIdx = 1,
                componentStatementUnreorderableTestExpected = con True
              },
            ComponentStatementUnreorderableTest
              { componentStatementUnreorderableTestName =
                  "usedSecondDefInvalidatedByFirst'",
                componentStatementUnreorderableTestProg =
                  Prog
                    "test"
                    [ProgArg "x" OtherType, ProgArg "y" OtherType]
                    [ Stmt OpDef [0] 1 [4, 5] 2 (con False) [],
                      Stmt OpDef [0] 1 [2, 3] 2 (con False) []
                    ]
                    [ProgRes 3 ConstrainedType, ProgRes 5 ConstrainedType],
                componentStatementUnreorderableTestFirstIdx = 1,
                componentStatementUnreorderableTestSecondIdx = 0,
                componentStatementUnreorderableTestExpected = con True
              },
            ComponentStatementUnreorderableTest
              { componentStatementUnreorderableTestName =
                  "usedSecondDefNotInvalidatedByFirst",
                componentStatementUnreorderableTestProg =
                  Prog
                    "test"
                    [ProgArg "x" OtherType, ProgArg "y" OtherType]
                    [ Stmt OpDef2 [0] 1 [2, 3] 2 (con False) [],
                      Stmt OpDef [0] 1 [4, 5] 2 (con False) []
                    ]
                    [ProgRes 3 ConstrainedType2, ProgRes 5 ConstrainedType],
                componentStatementUnreorderableTestFirstIdx = 0,
                componentStatementUnreorderableTestSecondIdx = 1,
                componentStatementUnreorderableTestExpected = con False
              },
            ComponentStatementUnreorderableTest
              { componentStatementUnreorderableTestName =
                  "unusedSecondDefInvalidatedByFirst",
                componentStatementUnreorderableTestProg =
                  Prog
                    "test"
                    [ProgArg "x" OtherType, ProgArg "y" OtherType]
                    [ Stmt OpDef [0] 1 [2, 3] 2 (con False) [],
                      Stmt OpDef [0] 1 [4, 5] 2 (con False) []
                    ]
                    [ProgRes 3 ConstrainedType],
                componentStatementUnreorderableTestFirstIdx = 0,
                componentStatementUnreorderableTestSecondIdx = 1,
                componentStatementUnreorderableTestExpected = con False
              }
            ]
        [ testCase name $ do
            let actual =
                  componentStatementUnreorderable
                    (Liveliness @SymBool (LivelinessOp @SymBool))
                    prog
                    firstIdx
                    secondIdx
            let actual' =
                  componentStatementUnreorderable'
                    (Liveliness @SymBool (LivelinessOp @SymBool))
                    prog
                    firstIdx
                    secondIdx
            let expected = mrgReturn reorderable :: SymbolicContext SymBool
            actual @?= expected
            actual' @?= expected
          ],
      testGroup "ComponentStatementUnreorderable'" $ do
        let simpleProg =
              Prog
                "test"
                [ProgArg "x" OtherType, ProgArg "y" OtherType]
                [ Stmt OpNone [0] 1 [2] 1 (con False) [],
                  Stmt OpNone [0] 1 [3] 1 (con False) []
                ]
                [ProgRes 2 OtherType, ProgRes 3 OtherType]
        ComponentStatementUnreorderableTest
          name
          prog
          firstIdx
          secondIdx
          reorderable <-
          [ ComponentStatementUnreorderableTest
              { componentStatementUnreorderableTestName = "Same index",
                componentStatementUnreorderableTestProg = simpleProg,
                componentStatementUnreorderableTestFirstIdx = 0,
                componentStatementUnreorderableTestSecondIdx = 0,
                componentStatementUnreorderableTestExpected = con True
              },
            ComponentStatementUnreorderableTest
              { componentStatementUnreorderableTestName = "Out of bound index",
                componentStatementUnreorderableTestProg = simpleProg,
                componentStatementUnreorderableTestFirstIdx = 0,
                componentStatementUnreorderableTestSecondIdx = 2,
                componentStatementUnreorderableTestExpected = con True
              },
            ComponentStatementUnreorderableTest
              { componentStatementUnreorderableTestName = "Disabled statement",
                componentStatementUnreorderableTestProg =
                  Prog
                    "test"
                    [ProgArg "x" OtherType, ProgArg "y" OtherType]
                    [ Stmt OpNone [0] 1 [2] 1 (con True) [],
                      Stmt OpNone [0] 1 [3] 1 (con False) []
                    ]
                    [ProgRes 3 OtherType],
                componentStatementUnreorderableTestFirstIdx = 0,
                componentStatementUnreorderableTestSecondIdx = 1,
                componentStatementUnreorderableTestExpected = con False
              },
            ComponentStatementUnreorderableTest
              { componentStatementUnreorderableTestName = "Direct dependency",
                componentStatementUnreorderableTestProg =
                  Prog
                    "test"
                    [ProgArg "x" OtherType, ProgArg "y" OtherType]
                    [ Stmt OpNone [0] 1 [2] 1 (con False) [],
                      Stmt OpNone [2] 1 [3] 1 (con False) []
                    ]
                    [ProgRes 3 OtherType],
                componentStatementUnreorderableTestFirstIdx = 0,
                componentStatementUnreorderableTestSecondIdx = 1,
                componentStatementUnreorderableTestExpected = con True
              }
            ]
        [ testCase name $ do
            let actual =
                  componentStatementUnreorderable'
                    (Liveliness @SymBool (LivelinessOp @SymBool))
                    prog
                    firstIdx
                    secondIdx
            let expected = mrgReturn reorderable :: SymbolicContext SymBool
            actual @?= expected
          ],
      testGroup "CanonicalOrderConstraint" $ do
        CanonicalOrderConstraintTest name prog constrObj expected <-
          [ CanonicalOrderConstraintTest
              { canonicalOrderConstraintTestName = "good",
                canonicalOrderConstraintTestProg = goodConcreteProg,
                canonicalOrderConstraintTestConstraintObj = (),
                canonicalOrderConstraintTestExpected = True
              },
            CanonicalOrderConstraintTest
              { canonicalOrderConstraintTestName = "bad",
                canonicalOrderConstraintTestProg =
                  Prog
                    "test"
                    [ProgArg "x" IntType, ProgArg "y" IntType]
                    [ Stmt Add [0, 1] 2 [3] 1 (con False) [],
                      Stmt Add [0, 1] 2 [2] 1 (con False) []
                    ]
                    [ProgRes 2 IntType, ProgRes 3 IntType],
                canonicalOrderConstraintTestConstraintObj = (),
                canonicalOrderConstraintTestExpected = False
              },
            CanonicalOrderConstraintTest
              { canonicalOrderConstraintTestName = "unreorderable",
                canonicalOrderConstraintTestProg =
                  Prog
                    "test"
                    [ProgArg "x" OtherType, ProgArg "y" OtherType]
                    [ Stmt OpDef [0] 1 [4, 5] 2 (con False) [],
                      Stmt OpDef [0] 1 [2, 3] 2 (con False) []
                    ]
                    [ProgRes 3 ConstrainedType, ProgRes 5 ConstrainedType],
                canonicalOrderConstraintTestConstraintObj =
                  Liveliness @SymBool (LivelinessOp @SymBool),
                canonicalOrderConstraintTestExpected = True
              }
            ]
        [ testCase name $ do
            let actual = do
                  canonicalOrderConstraint constrObj prog ::
                    SymbolicContext ()
            r <- solveExcept (precise z3) (con . isRight) actual
            if expected
              then assertBool "Should be valid" $ isRight r
              else assertBool "Should be invalid" $ isLeft r
          ]
    ]
