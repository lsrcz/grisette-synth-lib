{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.BuiltinProgConstraints.ComponentSymmetryReductionTest
  ( componentSymmetryReductionTest,
  )
where

import Data.Either (isLeft, isRight)
import Data.Foldable (traverse_)
import Grisette
  ( Solvable (con),
    SymBool,
    SymInteger,
    evaluateSymToCon,
    precise,
    runFresh,
    solveExcept,
    solveMultiExcept,
    z3,
  )
import Grisette.Lib.Synth.Context (SymbolicContext)
import Grisette.Lib.Synth.Program.BuiltinProgConstraints.ComponentSymmetryReduction
  ( angelicDistanceMatrix,
    canonicalOrderConstraint,
    directDep,
    distanceMatrixConstraint,
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
--
-- Minimum distance matrix:
--   0 1 2 3 4 5 6 7
-- 0 F 1 2 2 F F 3 3
-- 1 F F 1 1 F F 2 2
-- 2 F F F F F F F 1
-- 3 F F F F F F 1 2
-- 4 F F F 1 F F 2 3
-- 5 F F 1 2 1 F 3 2
-- 6 F F F F F F F 1
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

minimumDistanceMatrix :: [[Integer]]
minimumDistanceMatrix =
  [ [-1, 1, 2, 2, -1, -1, 3, 3],
    [-1, -1, 1, 1, -1, -1, 2, 2],
    [-1, -1, -1, -1, -1, -1, -1, 1],
    [-1, -1, -1, -1, -1, -1, 1, 2],
    [-1, -1, -1, 1, -1, -1, 2, 3],
    [-1, -1, 1, 2, 1, -1, 3, 2],
    [-1, -1, -1, -1, -1, -1, -1, 1],
    [-1, -1, -1, -1, -1, -1, -1, -1]
  ]

isValidDistanceMatrix :: [[Integer]] -> Bool
isValidDistanceMatrix mat =
  and $
    zipWith
      ( \m ref ->
          if m == -1
            then ref == -1
            else m >= ref
      )
      (concat mat)
      (concat minimumDistanceMatrix)

badConcreteProg :: Prog TestSemanticsOp SymInteger TestSemanticsType
badConcreteProg =
  Prog
    "test"
    [ProgArg "x" IntType, ProgArg "y" IntType]
    [ Stmt Add [0, 1] 2 [2] 1 (con False) [],
      Stmt Add [0, 2] 2 [4] 1 (con False) [],
      Stmt Add [4, 3] 2 [5] 1 (con False) [],
      Stmt Add [4, 6] 2 [7] 1 (con False) [],
      Stmt Add [0, 3] 2 [6] 1 (con False) [],
      Stmt Add [0, 1] 2 [3] 1 (con False) [],
      Stmt Add [0, 7] 2 [8] 1 (con False) [],
      Stmt Add [5, 8] 2 [9] 1 (con False) []
    ]
    [ProgRes 8 IntType, ProgRes 9 IntType]

componentSymmetryReductionTest :: Test
componentSymmetryReductionTest =
  testGroup
    "ComponentSymmetryReductionTest"
    [ testCase "directDep" $ do
        let actual =
              [ [ if i == j
                    then con False
                    else
                      directDep
                        (goodConcreteProgStmtList !! i)
                        (goodConcreteProgStmtList !! j)
                  | j <- [0 .. length goodConcreteProgStmtList - 1]
                ]
                | i <- [0 .. length goodConcreteProgStmtList - 1]
              ]
        actual @?= directReachabilityMatrix,
      testCase "distanceMatrixConstraint" $ do
        let mat =
              flip runFresh "a" $
                angelicDistanceMatrix (length goodConcreteProgStmtList)
        let actual =
              distanceMatrixConstraint (progStmtList goodConcreteProg) mat ::
                SymbolicContext ()
        (l, _) <-
          solveMultiExcept (precise z3) 100 (con . isRight) actual
        traverse_
          ( \m ->
              assertBool "Should be valid" $
                isValidDistanceMatrix $
                  evaluateSymToCon m mat
          )
          l,
      testGroup "CanonicalOrderConstraint" $ do
        (name, prog, expected) <-
          [("good", goodConcreteProg, True), ("bad", badConcreteProg, False)]
        [ testCase name $ do
            let stmtList = progStmtList prog
            let mat =
                  flip runFresh "a" $ angelicDistanceMatrix (length stmtList)
            let actual = do
                  distanceMatrixConstraint stmtList mat
                  canonicalOrderConstraint stmtList mat ::
                    SymbolicContext ()
            r <- solveExcept (precise z3) (con . isRight) actual
            if expected
              then assertBool "Should be valid" $ isRight r
              else assertBool "Should be invalid" $ isLeft r
          ]
    ]
