{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.BuiltinProgConstraints.ComponentSymmetryReductionTest
  ( componentSymmetryReductionTest,
  )
where

import Grisette
  ( LogicalOp (symNot, (.||)),
    SEq ((.==)),
    Solvable (con, isym),
    SymBool,
    SymInteger,
    SymWordN,
    mrgIf,
    mrgReturn,
    runFreshT,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Synth.Context (AngelicContext, SymbolicContext)
import Grisette.Lib.Synth.Program.BuiltinProgConstraints.ComponentSymmetryReduction
  ( ComponentStatementUnreorderable (componentStatementUnreorderable),
    ComponentSymmetryReduction (ComponentSymmetryReduction),
    componentStatementUnreorderable',
    statementsDirectDep,
  )
import Grisette.Lib.Synth.Program.BuiltinProgConstraints.Liveliness
  ( Liveliness (Liveliness),
  )
import Grisette.Lib.Synth.Program.BuiltinProgConstraints.LivelinessTest
  ( LivelinessOp (LivelinessOp),
    Op
      ( OpComponentSubProg,
        OpDef,
        OpDef2,
        OpDefAny,
        OpNone,
        OpUse,
        OpUse2,
        OpUseAny,
        OpUseDef,
        OpUseDefAny
      ),
    Type (AnyType, ConstrainedType, ConstrainedType2, OtherType),
    anyResource,
    livelinessIdentifier,
  )
import Grisette.Lib.Synth.Program.ComponentSketch
  ( Prog (Prog, progStmtList),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import Grisette.Lib.Synth.Program.ProgConstraints
  ( ProgConstraints (constrainProg),
  )
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Test.SymbolicAssertion ((.@?=))

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

data ProgConstraintsTest where
  ProgConstraintsTest ::
    (ProgConstraints constrObj prog AngelicContext) =>
    { progConstraintsTestName :: String,
      progConstraintsTestProg :: prog,
      progConstraintsTestConstraintObj :: constrObj,
      progConstraintsTestExpected :: SymbolicContext ()
    } ->
    ProgConstraintsTest

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
                  "firstUsedDefInvalidatedBySecond, invalidated",
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
                  "firstUsesInvalidatedBySecond, not invalidated 1",
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
                  "firstUsesInvalidatedBySecond, not invalidated 2",
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
                  "secondDefsInvalidatedByFirst, invalidated",
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
                  "secondDefsInvalidatedByFirst, invalidated 2",
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
                  "secondDefsInvalidatedByFirst, not invalidated",
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
                  "symbolic",
                componentStatementUnreorderableTestProg =
                  let inner =
                        Prog
                          "inner"
                          [ProgArg "x" OtherType, ProgArg "y" AnyType]
                          [ Stmt OpUseAny [0, 1] 2 [2] 1 (con False) [],
                            Stmt OpDefAny [2] 2 [3, 4] 1 (con False) []
                          ]
                          [ProgRes 3 OtherType, ProgRes 4 AnyType]
                   in Prog
                        "test"
                        [ProgArg "x" OtherType, ProgArg "y" AnyType]
                        [ Stmt
                            (OpComponentSubProg inner)
                            [0, 1]
                            2
                            [2, 3]
                            2
                            (con False)
                            [],
                          Stmt
                            (OpComponentSubProg inner)
                            [0, 1]
                            2
                            [4, 5]
                            2
                            (con False)
                            []
                        ]
                        [ProgRes 3 OtherType, ProgRes 5 AnyType],
                componentStatementUnreorderableTestFirstIdx = 0,
                componentStatementUnreorderableTestSecondIdx = 1,
                componentStatementUnreorderableTestExpected =
                  ( anyResource "x" ["test:stmt0", "inner:arg"] 0
                      .== anyResource "x" ["test:stmt1", "inner:stmt1"] 0
                  )
                    .|| ( anyResource "x" ["test:stmt1", "inner:res"] 0
                            .== anyResource "x" ["test:stmt0", "inner:stmt1"] 0
                        )
              }
            ]
        [ testCase name $ do
            let actual =
                  flip runFreshT "x" $
                    componentStatementUnreorderable
                      (Liveliness LivelinessOp)
                      prog
                      firstIdx
                      secondIdx
            let actual' =
                  flip runFreshT "x" $
                    componentStatementUnreorderable'
                      (Liveliness LivelinessOp)
                      prog
                      firstIdx
                      secondIdx
            let expected = mrgReturn reorderable :: SymbolicContext SymBool
            actual @?= expected
            actual' @?= expected
          ],
      testGroup "ComponentStatementUnreorderable'" $
        do
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
                { componentStatementUnreorderableTestName =
                    "Out of bound index",
                  componentStatementUnreorderableTestProg = simpleProg,
                  componentStatementUnreorderableTestFirstIdx = 0,
                  componentStatementUnreorderableTestSecondIdx = 2,
                  componentStatementUnreorderableTestExpected = con True
                },
              ComponentStatementUnreorderableTest
                { componentStatementUnreorderableTestName =
                    "Disabled statement",
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
                    flip runFreshT "x" $
                      componentStatementUnreorderable'
                        (Liveliness LivelinessOp)
                        prog
                        firstIdx
                        secondIdx
              let expected = mrgReturn reorderable :: SymbolicContext SymBool
              actual @?= expected
            ],
      testGroup
        "CanonicalOrderConstraint"
        $ do
          ProgConstraintsTest name prog constrObj expected <-
            [ ProgConstraintsTest
                { progConstraintsTestName = "good",
                  progConstraintsTestProg = goodConcreteProg,
                  progConstraintsTestConstraintObj =
                    ComponentSymmetryReduction (),
                  progConstraintsTestExpected = mrgReturn ()
                },
              ProgConstraintsTest
                { progConstraintsTestName = "bad",
                  progConstraintsTestProg =
                    Prog
                      "test"
                      [ProgArg "x" IntType, ProgArg "y" IntType]
                      [ Stmt Add [0 :: SymInteger, 1] 2 [3] 1 (con False) [],
                        Stmt Add [0, 1] 2 [2] 1 (con False) []
                      ]
                      [ProgRes 2 IntType, ProgRes 3 IntType],
                  progConstraintsTestConstraintObj =
                    ComponentSymmetryReduction (),
                  progConstraintsTestExpected = mrgThrowError "Bad order"
                },
              ProgConstraintsTest
                { progConstraintsTestName = "unreorderable",
                  progConstraintsTestProg =
                    Prog
                      "test"
                      [ProgArg "x" OtherType]
                      [ Stmt OpDef [0 :: SymInteger] 1 [4, 5] 2 (con False) [],
                        Stmt OpDef [0] 1 [2, 3] 2 (con False) []
                      ]
                      [ProgRes 3 ConstrainedType, ProgRes 4 OtherType],
                  progConstraintsTestConstraintObj =
                    ComponentSymmetryReduction (Liveliness LivelinessOp),
                  progConstraintsTestExpected = mrgReturn ()
                },
              ProgConstraintsTest
                { progConstraintsTestName = "symbolic",
                  progConstraintsTestProg =
                    Prog
                      "test"
                      [ ProgArg "x" ConstrainedType,
                        ProgArg "y" ConstrainedType2
                      ]
                      [ Stmt OpUseDefAny [1] 1 [3] 1 (con False) [],
                        Stmt OpUseDef [0] 1 [2] 1 (con False) []
                      ]
                      [ProgRes (2 :: SymInteger) ConstrainedType],
                  progConstraintsTestConstraintObj =
                    ComponentSymmetryReduction (Liveliness LivelinessOp),
                  progConstraintsTestExpected =
                    mrgIf
                      -- Both false, means that OpUseDefAny is 2 -> 2,
                      -- then we should reorder
                      ( symNot $
                          isym (livelinessIdentifier "x" ["test:stmt0"]) 0
                            .|| isym (livelinessIdentifier "x" ["test:stmt0"]) 1
                      )
                      (mrgThrowError "Bad order")
                      ( mrgIf
                          -- ? -> 1
                          (isym (livelinessIdentifier "x" ["test:stmt0"]) 0)
                          ( mrgThrowError $
                              "Cannot use invalidated resource for "
                                <> "ConstrainedType"
                          )
                          -- 1 -> 2
                          ( mrgThrowError $
                              "Inconsistent use/def resources for "
                                <> "ConstrainedType"
                          )
                      )
                },
              ProgConstraintsTest
                { progConstraintsTestName = "symbolic/nested",
                  progConstraintsTestProg =
                    let inner =
                          Prog
                            "inner"
                            [ ProgArg "x" ConstrainedType,
                              ProgArg "y" ConstrainedType2
                            ]
                            [ Stmt OpUseDefAny [1] 1 [3] 1 (con False) [],
                              Stmt OpUseDef [0] 1 [2] 1 (con False) []
                            ]
                            [ProgRes (2 :: SymWordN 8) ConstrainedType]
                     in Prog
                          "test"
                          [ ProgArg "x" ConstrainedType,
                            ProgArg "y" ConstrainedType2
                          ]
                          [ Stmt
                              (OpComponentSubProg inner)
                              [0 :: SymWordN 8, 1]
                              2
                              [2]
                              1
                              (con False)
                              []
                          ]
                          [ProgRes 2 ConstrainedType],
                  progConstraintsTestConstraintObj =
                    ComponentSymmetryReduction (Liveliness LivelinessOp),
                  progConstraintsTestExpected =
                    mrgIf
                      -- Both false, means that OpUseDefAny is 2 -> 2,
                      -- then we should reorder
                      ( symNot $
                          isym
                            ( livelinessIdentifier
                                "x"
                                ["test:stmt0", "inner:stmt0"]
                            )
                            0
                            .|| isym
                              ( livelinessIdentifier
                                  "x"
                                  ["test:stmt0", "inner:stmt0"]
                              )
                              1
                      )
                      (mrgThrowError "Bad order")
                      ( mrgIf
                          -- ? -> 1
                          ( isym
                              ( livelinessIdentifier
                                  "x"
                                  ["test:stmt0", "inner:stmt0"]
                              )
                              0
                          )
                          ( mrgThrowError $
                              "Cannot use invalidated resource for "
                                <> "ConstrainedType"
                          )
                          -- 1 -> 2
                          ( mrgThrowError $
                              "Inconsistent use/def resources for "
                                <> "ConstrainedType"
                          )
                      )
                }
              ]
          [ testCase name $ do
              let actual =
                    flip runFreshT "x" $
                      constrainProg
                        constrObj
                        prog ::
                      SymbolicContext ()
              actual .@?= expected
            ]
    ]