{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Lib.Synth.Program.BuiltinProgConstraints.LinearDefUseTest
  ( linearDefUseTest,
  )
where

import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvaluateSym,
    ITEOp (symIte),
    Mergeable,
    SEq,
    Solvable (con),
    SymWordN,
    ToCon (toCon),
    UnionM,
    WordN,
    mrgIf,
    mrgReturn,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Synth.Context
  ( ConcreteContext,
    MonadContext,
    SymbolicContext,
  )
import Grisette.Lib.Synth.Program.BuiltinProgConstraints.LinearDefUse
  ( Def (Def),
    DefUnion,
    LinearDefUse (LinearDefUse),
    LinearDefUseExtract (linearDefUseExtractDefs, linearDefUseExtractUses),
    LinearDefUseName (linearDefUseName),
    LinearDefUseTypePredicate (linearDefUseTypePredicate),
    Use (Use),
    UseUnion,
    componentProgDefs,
    componentProgUses,
    componentStmtDefs,
    componentStmtUses,
    concreteStmtDef,
    concreteStmtUse,
  )
import qualified Grisette.Lib.Synth.Program.ComponentSketch as Component
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.ProgConstraints
  ( ProgConstraints (constrainProg),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Test.SymbolicAssertion ((.@?=))

data Op = OpUse | OpDef | OpNone | OpUseDef | OpDefNoInvalidate
  deriving (Generic)
  deriving (Mergeable, ToCon Op) via (Default Op)

data Type = ConstrainedType | OtherType
  deriving (Generic)
  deriving (Mergeable, ToCon Type) via (Default Type)

data LinearDefUseOp = LinearDefUseOp

instance LinearDefUseName LinearDefUseOp where
  linearDefUseName LinearDefUseOp = "ConstrainedType"

instance LinearDefUseExtract LinearDefUseOp Op where
  linearDefUseExtractDefs LinearDefUseOp OpUse _ _ = Nothing
  linearDefUseExtractDefs LinearDefUseOp OpDef ids disabled =
    Just $ Def (ids !! 1) disabled
  linearDefUseExtractDefs LinearDefUseOp OpNone _ _ = Nothing
  linearDefUseExtractDefs LinearDefUseOp OpUseDef ids disabled =
    Just $ Def (head ids) disabled
  linearDefUseExtractDefs LinearDefUseOp OpDefNoInvalidate _ _ =
    Nothing
  linearDefUseExtractUses LinearDefUseOp OpUse argIds resIds disabled =
    Just $ Use (head resIds) (argIds !! 1) disabled
  linearDefUseExtractUses LinearDefUseOp OpDef _ _ _ = Nothing
  linearDefUseExtractUses LinearDefUseOp OpNone _ _ _ = Nothing
  linearDefUseExtractUses LinearDefUseOp OpUseDef argIds resIds disabled =
    Just $ Use (head resIds) (head argIds) disabled
  linearDefUseExtractUses LinearDefUseOp OpDefNoInvalidate _ _ _ =
    Nothing

instance LinearDefUseTypePredicate LinearDefUseOp Type where
  linearDefUseTypePredicate LinearDefUseOp ConstrainedType = True
  linearDefUseTypePredicate LinearDefUseOp OtherType = False

data LinearDefUseTest where
  LinearDefUseTest ::
    ( ProgConstraints (LinearDefUse LinearDefUseOp) prog ctx,
      SEq (ctx ()),
      EvaluateSym (ctx ()),
      Show (ctx ())
    ) =>
    { linearDefUseTestName :: String,
      linearDefUseTestProg :: prog,
      linearDefUseTestExpectedAssertion :: ctx ()
    } ->
    LinearDefUseTest

opUse :: UnionM Op
opUse = mrgReturn OpUse

opDef :: UnionM Op
opDef = mrgReturn OpDef

opNone :: UnionM Op
opNone = mrgReturn OpNone

opUseDef :: UnionM Op
opUseDef = mrgReturn OpUseDef

opDefNoInvalidate :: UnionM Op
opDefNoInvalidate = mrgReturn OpDefNoInvalidate

concreteDefStmt :: Concrete.Stmt Op (WordN 8)
concreteDefStmt = Concrete.Stmt OpDef [0] [2, 3]

componentDefStmt :: Component.Stmt Op (SymWordN 8) Type
componentDefStmt =
  Component.Stmt
    (mrgIf "da" opDef opUseDef)
    ["da0", "da1"]
    "dna"
    ["dr0", "dr1"]
    "dnr"
    "ddisabled"

concreteUseStmt :: Concrete.Stmt Op (WordN 8)
concreteUseStmt = Concrete.Stmt OpUse [0, 3] [4]

componentUseStmt :: Component.Stmt Op (SymWordN 8) Type
componentUseStmt =
  Component.Stmt
    (mrgIf "ua" opUse opUseDef)
    ["ua0", "ua1"]
    "una"
    ["ur0", "ur1"]
    "unr"
    "udisabled"

componentProg :: Component.Prog Op (SymWordN 8) Type
componentProg =
  Component.Prog
    "test"
    [ Component.ProgArg "arg0" ConstrainedType,
      Component.ProgArg "arg1" OtherType
    ]
    [componentDefStmt, componentUseStmt]
    [Component.ProgRes "pr0" OtherType, Component.ProgRes "pr1" ConstrainedType]

linearDefUseTest :: Test
linearDefUseTest =
  testGroup
    "LinearDefUse"
    [ testCase "concreteStmtDef" $ do
        let actual = concreteStmtDef LinearDefUseOp concreteDefStmt
        actual @?= Just 3,
      testCase "concreteStmtUses" $ do
        let actual = concreteStmtUse LinearDefUseOp concreteUseStmt
        actual @?= Just 3,
      testCase "componentStmtDefs" $ do
        let actual =
              componentStmtDefs LinearDefUseOp componentDefStmt ::
                SymbolicContext [DefUnion (SymWordN 8)]
        let expected =
              mrgReturn
                [mrgReturn $ Just $ Def (symIte "da" "dr1" "dr0") "ddisabled"]
        actual @?= expected,
      testCase "componentStmtUses" $ do
        let actual =
              componentStmtUses LinearDefUseOp componentUseStmt ::
                SymbolicContext [UseUnion (SymWordN 8)]
        let expected =
              mrgReturn
                [ mrgReturn $
                    Just $
                      Use "ur0" (symIte "ua" "ua1" "ua0") "udisabled"
                ]
        actual @?= expected,
      testCase "componentProgDefs" $ do
        let actual =
              componentProgDefs LinearDefUseOp componentProg ::
                SymbolicContext [DefUnion (SymWordN 8)]
        let expected =
              mrgReturn
                [ mrgReturn $ Just $ Def 0 (con False),
                  mrgReturn $ Just $ Def (symIte "da" "dr1" "dr0") "ddisabled",
                  mrgIf
                    "ua"
                    (mrgReturn Nothing)
                    (mrgReturn $ Just $ Def "ur0" "udisabled")
                ]
        actual @?= expected,
      testCase "componentProgUses" $ do
        let actual =
              componentProgUses LinearDefUseOp componentProg ::
                SymbolicContext [UseUnion (SymWordN 8)]
        let expected =
              mrgReturn
                [ mrgReturn $ Just $ Use 4 "pr1" (con False),
                  mrgIf
                    "da"
                    (mrgReturn Nothing)
                    (mrgReturn $ Just $ Use "dr0" "da0" "ddisabled"),
                  mrgReturn $
                    Just $
                      Use "ur0" (symIte "ua" "ua1" "ua0") "udisabled"
                ]
        actual @?= expected,
      testGroup "ProgConstraint" $ do
        LinearDefUseTest name prog (assertion :: ctx ()) <-
          concat
            [ [ LinearDefUseTest
                  { linearDefUseTestName = "Component/correct",
                    linearDefUseTestProg =
                      Component.Prog
                        "test"
                        [ Component.ProgArg "arg0" OtherType,
                          Component.ProgArg "arg1" ConstrainedType
                        ]
                        [ Component.Stmt opUse [0, 1] 2 [2] 1 (con False),
                          Component.Stmt opDef [0] 1 [3, 4] 2 (con False),
                          Component.Stmt opUse [3, 4] 2 [5] 1 (con False),
                          Component.Stmt opUseDef [4] 1 [6] 1 (con False),
                          Component.Stmt opUse [3, 6] 2 [7] 1 (con False),
                          Component.Stmt opNone [7] 2 [8] 1 (con False)
                        ]
                        [Component.ProgRes (6 :: SymWordN 8) ConstrainedType],
                    linearDefUseTestExpectedAssertion =
                      mrgReturn () :: SymbolicContext ()
                  },
                LinearDefUseTest
                  { linearDefUseTestName = "Component/disabled ok",
                    linearDefUseTestProg =
                      Component.Prog
                        "test"
                        [ Component.ProgArg "arg0" OtherType,
                          Component.ProgArg "arg1" ConstrainedType
                        ]
                        [ Component.Stmt opDef [0] 1 [2, 3] 2 (con False),
                          Component.Stmt opUseDef [1] 1 [4] 1 (con True)
                        ]
                        [Component.ProgRes (3 :: SymWordN 8) ConstrainedType],
                    linearDefUseTestExpectedAssertion =
                      mrgReturn () :: SymbolicContext ()
                  },
                LinearDefUseTest
                  { linearDefUseTestName = "Concrete/correct",
                    linearDefUseTestProg =
                      Concrete.Prog
                        "test"
                        [ Concrete.ProgArg "arg0" 0 OtherType,
                          Concrete.ProgArg "arg1" (-1) ConstrainedType
                        ]
                        [ Concrete.Stmt OpUse [0, -1] [2],
                          Concrete.Stmt OpDef [0] [3, 4],
                          Concrete.Stmt OpUse [3, 4] [5],
                          Concrete.Stmt OpUseDef [4] [9],
                          Concrete.Stmt OpUse [3, 9] [7],
                          Concrete.Stmt OpNone [7] [8]
                        ]
                        [Concrete.ProgRes (9 :: WordN 8) ConstrainedType],
                    linearDefUseTestExpectedAssertion =
                      mrgReturn () :: ConcreteContext ()
                  }
              ],
              let correct2 =
                    Component.Prog
                      "test"
                      [ Component.ProgArg "arg0" OtherType,
                        Component.ProgArg "arg1" ConstrainedType
                      ]
                      [ Component.Stmt opDef [0] 1 [2, 3] 2 (con False),
                        Component.Stmt opDef [0] 1 [4, 5] 2 (con False),
                        Component.Stmt opUse [2, 5] 2 [6] 1 (con False)
                      ]
                      [Component.ProgRes (5 :: SymWordN 8) ConstrainedType]
               in [ LinearDefUseTest
                      { linearDefUseTestName = "Component/correct2",
                        linearDefUseTestProg = correct2,
                        linearDefUseTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      },
                    LinearDefUseTest
                      { linearDefUseTestName = "Concrete/correct2",
                        linearDefUseTestProg =
                          fromJust $ toCon correct2 ::
                            Concrete.Prog Op (WordN 8) Type,
                        linearDefUseTestExpectedAssertion =
                          mrgReturn () :: ConcreteContext ()
                      }
                  ],
              let notUsingNewestDefButUsingFromArg =
                    Component.Prog
                      "test"
                      [ Component.ProgArg "arg0" OtherType,
                        Component.ProgArg "arg1" ConstrainedType
                      ]
                      [ Component.Stmt opDef [0] 1 [2, 3] 2 (con False),
                        Component.Stmt opDef [0] 1 [4, 5] 2 (con False),
                        Component.Stmt opUse [2, 1] 2 [6] 1 (con False)
                      ]
                      [Component.ProgRes (5 :: SymWordN 8) ConstrainedType]
                  err :: (MonadContext m) => m ()
                  err =
                    mrgThrowError
                      "Must use the newest definition of ConstrainedType"
               in [ LinearDefUseTest
                      { linearDefUseTestName =
                          "Component/not using newest def but using from arg",
                        linearDefUseTestProg = notUsingNewestDefButUsingFromArg,
                        linearDefUseTestExpectedAssertion =
                          err :: SymbolicContext ()
                      },
                    LinearDefUseTest
                      { linearDefUseTestName =
                          "Concrete/not using newest def but using from arg",
                        linearDefUseTestProg =
                          fromJust $ toCon notUsingNewestDefButUsingFromArg ::
                            Concrete.Prog Op (WordN 8) Type,
                        linearDefUseTestExpectedAssertion =
                          err :: ConcreteContext ()
                      }
                  ],
              let notUsingNewestDef =
                    Component.Prog
                      "test"
                      [ Component.ProgArg "arg0" OtherType,
                        Component.ProgArg "arg1" ConstrainedType
                      ]
                      [ Component.Stmt opDef [0] 1 [2, 3] 2 (con False),
                        Component.Stmt opDef [0] 1 [4, 5] 2 (con False),
                        Component.Stmt opUse [2, 3] 2 [6] 1 (con False)
                      ]
                      [Component.ProgRes (5 :: SymWordN 8) ConstrainedType]
                  err :: (MonadContext m) => m ()
                  err =
                    mrgThrowError
                      "Must use the newest definition of ConstrainedType"
               in [ LinearDefUseTest
                      { linearDefUseTestName = "Component/not using newest def",
                        linearDefUseTestProg = notUsingNewestDef,
                        linearDefUseTestExpectedAssertion =
                          err :: SymbolicContext ()
                      },
                    LinearDefUseTest
                      { linearDefUseTestName = "Concrete/not using newest def",
                        linearDefUseTestProg =
                          fromJust $ toCon notUsingNewestDef ::
                            Concrete.Prog Op (WordN 8) Type,
                        linearDefUseTestExpectedAssertion =
                          err :: ConcreteContext ()
                      }
                  ],
              let prog =
                    Component.Prog
                      "test"
                      [ Component.ProgArg "arg0" OtherType,
                        Component.ProgArg "arg1" ConstrainedType
                      ]
                      [ Component.Stmt opDef [0] 1 [2, 3] 2 (con False),
                        Component.Stmt opDef [0] 1 [4, 5] 2 (con False),
                        Component.Stmt opUse [2, 1] 2 [6] 1 (con False)
                      ]
                      [Component.ProgRes (1 :: SymWordN 8) ConstrainedType]
                  err :: (MonadContext m) => m ()
                  err =
                    mrgThrowError
                      "Must use the newest definition of ConstrainedType"
               in [ LinearDefUseTest
                      { linearDefUseTestName =
                          "Component/res not using newest def, use arg",
                        linearDefUseTestProg = prog,
                        linearDefUseTestExpectedAssertion =
                          err :: SymbolicContext ()
                      },
                    LinearDefUseTest
                      { linearDefUseTestName =
                          "Concrete/res not using newest def, use arg",
                        linearDefUseTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        linearDefUseTestExpectedAssertion =
                          err :: ConcreteContext ()
                      }
                  ],
              let prog =
                    Component.Prog
                      "test"
                      [ Component.ProgArg "arg0" OtherType,
                        Component.ProgArg "arg1" ConstrainedType
                      ]
                      [ Component.Stmt opDef [0] 1 [2, 3] 2 (con False),
                        Component.Stmt opDef [0] 1 [4, 5] 2 (con False),
                        Component.Stmt opUse [2, 1] 2 [6] 1 (con False)
                      ]
                      [Component.ProgRes (3 :: SymWordN 8) ConstrainedType]
                  err :: (MonadContext m) => m ()
                  err =
                    mrgThrowError
                      "Must use the newest definition of ConstrainedType"
               in [ LinearDefUseTest
                      { linearDefUseTestName =
                          "Component/res not using newest def, use old stmt",
                        linearDefUseTestProg = prog,
                        linearDefUseTestExpectedAssertion =
                          err :: SymbolicContext ()
                      },
                    LinearDefUseTest
                      { linearDefUseTestName =
                          "Concrete/res not using newest def, use old stmt",
                        linearDefUseTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        linearDefUseTestExpectedAssertion =
                          err :: ConcreteContext ()
                      }
                  ],
              let prog =
                    Component.Prog @Op
                      "test"
                      [ Component.ProgArg "arg0" OtherType,
                        Component.ProgArg "arg1" ConstrainedType,
                        Component.ProgArg "arg2" ConstrainedType
                      ]
                      []
                      [Component.ProgRes (2 :: SymWordN 8) ConstrainedType]
                  err :: (MonadContext m) => m ()
                  err =
                    mrgThrowError $
                      "At most one argument with the type ConstrainedType to a "
                        <> "program"
               in [ LinearDefUseTest
                      { linearDefUseTestName = "Component/multiple arguments",
                        linearDefUseTestProg = prog,
                        linearDefUseTestExpectedAssertion =
                          err :: SymbolicContext ()
                      },
                    LinearDefUseTest
                      { linearDefUseTestName = "Concrete/multiple arguments",
                        linearDefUseTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        linearDefUseTestExpectedAssertion =
                          err :: ConcreteContext ()
                      }
                  ],
              let prog =
                    Component.Prog @Op
                      "test"
                      [ Component.ProgArg "arg0" OtherType,
                        Component.ProgArg "arg1" ConstrainedType
                      ]
                      []
                      [ Component.ProgRes (1 :: SymWordN 8) ConstrainedType,
                        Component.ProgRes (1 :: SymWordN 8) ConstrainedType
                      ]
                  err :: (MonadContext m) => m ()
                  err =
                    mrgThrowError $
                      "At most one result with the type ConstrainedType to a "
                        <> "program"
               in [ LinearDefUseTest
                      { linearDefUseTestName = "Component/multiple results",
                        linearDefUseTestProg = prog,
                        linearDefUseTestExpectedAssertion =
                          err :: SymbolicContext ()
                      },
                    LinearDefUseTest
                      { linearDefUseTestName = "Concrete/multiple results",
                        linearDefUseTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        linearDefUseTestExpectedAssertion =
                          err :: ConcreteContext ()
                      }
                  ],
              let prog =
                    Component.Prog
                      "test"
                      [ Component.ProgArg "arg0" OtherType,
                        Component.ProgArg "arg1" ConstrainedType
                      ]
                      [ Component.Stmt opDef [0] 1 [2, 3] 2 (con False),
                        Component.Stmt
                          opDefNoInvalidate
                          [1]
                          1
                          [4]
                          1
                          (con False),
                        Component.Stmt opUse [2, 3] 2 [5] 1 (con False)
                      ]
                      [Component.ProgRes (3 :: SymWordN 8) ConstrainedType]
               in [ LinearDefUseTest
                      { linearDefUseTestName =
                          "Component/no invalidate should not invalidate",
                        linearDefUseTestProg = prog,
                        linearDefUseTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      },
                    LinearDefUseTest
                      { linearDefUseTestName =
                          "Concrete/no invalidate should not invalidate",
                        linearDefUseTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        linearDefUseTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      }
                  ],
              let prog =
                    Component.Prog
                      "test"
                      [ Component.ProgArg "arg0" OtherType,
                        Component.ProgArg "arg1" ConstrainedType
                      ]
                      [ Component.Stmt opDef [0] 1 [2, 3] 2 (con False),
                        Component.Stmt
                          opDefNoInvalidate
                          [1]
                          1
                          [4]
                          1
                          (con False)
                      ]
                      [Component.ProgRes (4 :: SymWordN 8) ConstrainedType]
               in [ LinearDefUseTest
                      { linearDefUseTestName =
                          "Component/use no invalidate ok",
                        linearDefUseTestProg = prog,
                        linearDefUseTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      },
                    LinearDefUseTest
                      { linearDefUseTestName =
                          "Concrete/use no invalidate ok",
                        linearDefUseTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        linearDefUseTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      }
                  ],
              let prog =
                    Component.Prog
                      "test"
                      [ Component.ProgArg "arg0" OtherType,
                        Component.ProgArg "arg1" ConstrainedType
                      ]
                      [ Component.Stmt
                          opDefNoInvalidate
                          [1]
                          1
                          [2]
                          1
                          (con False),
                        Component.Stmt opDef [0] 1 [3, 4] 2 (con False)
                      ]
                      [Component.ProgRes (2 :: SymWordN 8) ConstrainedType]
               in [ LinearDefUseTest
                      { linearDefUseTestName =
                          "Component/use old no invalidate ok",
                        linearDefUseTestProg = prog,
                        linearDefUseTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      },
                    LinearDefUseTest
                      { linearDefUseTestName =
                          "Concrete/use old no invalidate ok",
                        linearDefUseTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        linearDefUseTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      }
                  ]
            ]
        return $
          testCase name $
            constrainProg (LinearDefUse LinearDefUseOp) prog .@?= assertion
    ]
