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

import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvaluateSym,
    ITEOp (symIte),
    Mergeable,
    SEq,
    Solvable (con),
    SymWordN,
    UnionM,
    mrgIf,
    mrgReturn,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Synth.Context (SymbolicContext)
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
  )
import qualified Grisette.Lib.Synth.Program.ComponentSketch as Component
import Grisette.Lib.Synth.Program.ProgConstraints
  ( ProgConstraints (constrainProg),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Test.SymbolicAssertion ((.@?=))

data Op = OpUse | OpDef | OpNone | OpUseDef
  deriving (Generic)
  deriving (Mergeable) via (Default Op)

data Type = ConstrainedType | OtherType

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
  linearDefUseExtractUses LinearDefUseOp OpUse argIds resIds disabled =
    Just $ Use (head resIds) (argIds !! 1) disabled
  linearDefUseExtractUses LinearDefUseOp OpDef _ _ _ = Nothing
  linearDefUseExtractUses LinearDefUseOp OpNone _ _ _ = Nothing
  linearDefUseExtractUses LinearDefUseOp OpUseDef argIds resIds disabled =
    Just $ Use (head resIds) (head argIds) disabled

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

defStmt :: Component.Stmt Op (SymWordN 8) Type
defStmt =
  Component.Stmt
    (mrgIf "da" opDef opUseDef)
    ["da0", "da1"]
    "dna"
    ["dr0", "dr1"]
    "dnr"
    "ddisabled"

useStmt :: Component.Stmt Op (SymWordN 8) Type
useStmt =
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
    [defStmt, useStmt]
    [Component.ProgRes "pr0" OtherType, Component.ProgRes "pr1" ConstrainedType]

linearDefUseTest :: Test
linearDefUseTest =
  testGroup
    "LinearDefUse"
    [ testCase "componentStmtDefs" $ do
        let actual =
              componentStmtDefs LinearDefUseOp defStmt ::
                SymbolicContext [DefUnion (SymWordN 8)]
        let expected =
              mrgReturn
                [mrgReturn $ Just $ Def (symIte "da" "dr1" "dr0") "ddisabled"]
        actual @?= expected,
      testCase "componentStmtUses" $ do
        let actual =
              componentStmtUses LinearDefUseOp useStmt ::
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
      testGroup "ProgConstraint/Component" $ do
        LinearDefUseTest name prog (assertion :: ctx ()) <-
          [ LinearDefUseTest
              { linearDefUseTestName = "correct",
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
              { linearDefUseTestName = "correct2",
                linearDefUseTestProg =
                  Component.Prog
                    "test"
                    [ Component.ProgArg "arg0" OtherType,
                      Component.ProgArg "arg1" ConstrainedType
                    ]
                    [ Component.Stmt opDef [0] 1 [2, 3] 2 (con False),
                      Component.Stmt opDef [0] 1 [4, 5] 2 (con False),
                      Component.Stmt opUse [2, 5] 2 [6] 1 (con False)
                    ]
                    [Component.ProgRes (5 :: SymWordN 8) ConstrainedType],
                linearDefUseTestExpectedAssertion =
                  mrgReturn () :: SymbolicContext ()
              },
            LinearDefUseTest
              { linearDefUseTestName =
                  "not using newest def but using from arg",
                linearDefUseTestProg =
                  Component.Prog
                    "test"
                    [ Component.ProgArg "arg0" OtherType,
                      Component.ProgArg "arg1" ConstrainedType
                    ]
                    [ Component.Stmt opDef [0] 1 [2, 3] 2 (con False),
                      Component.Stmt opDef [0] 1 [4, 5] 2 (con False),
                      Component.Stmt opUse [2, 1] 2 [6] 1 (con False)
                    ]
                    [Component.ProgRes (5 :: SymWordN 8) ConstrainedType],
                linearDefUseTestExpectedAssertion =
                  mrgThrowError
                    "Must use the newest definition of ConstrainedType" ::
                    SymbolicContext ()
              },
            LinearDefUseTest
              { linearDefUseTestName = "not using newest def",
                linearDefUseTestProg =
                  Component.Prog
                    "test"
                    [ Component.ProgArg "arg0" OtherType,
                      Component.ProgArg "arg1" ConstrainedType
                    ]
                    [ Component.Stmt opDef [0] 1 [2, 3] 2 (con False),
                      Component.Stmt opDef [0] 1 [4, 5] 2 (con False),
                      Component.Stmt opUse [2, 3] 2 [6] 1 (con False)
                    ]
                    [Component.ProgRes (5 :: SymWordN 8) ConstrainedType],
                linearDefUseTestExpectedAssertion =
                  mrgThrowError
                    "Must use the newest definition of ConstrainedType" ::
                    SymbolicContext ()
              },
            LinearDefUseTest
              { linearDefUseTestName =
                  "res not using newest def but using from arg",
                linearDefUseTestProg =
                  Component.Prog
                    "test"
                    [ Component.ProgArg "arg0" OtherType,
                      Component.ProgArg "arg1" ConstrainedType
                    ]
                    [ Component.Stmt opDef [0] 1 [2, 3] 2 (con False),
                      Component.Stmt opDef [0] 1 [4, 5] 2 (con False),
                      Component.Stmt opUse [2, 1] 2 [6] 1 (con False)
                    ]
                    [Component.ProgRes (1 :: SymWordN 8) ConstrainedType],
                linearDefUseTestExpectedAssertion =
                  mrgThrowError
                    "Must use the newest definition of ConstrainedType" ::
                    SymbolicContext ()
              },
            LinearDefUseTest
              { linearDefUseTestName =
                  "res not using newest def but using from arg",
                linearDefUseTestProg =
                  Component.Prog
                    "test"
                    [ Component.ProgArg "arg0" OtherType,
                      Component.ProgArg "arg1" ConstrainedType
                    ]
                    [ Component.Stmt opDef [0] 1 [2, 3] 2 (con False),
                      Component.Stmt opDef [0] 1 [4, 5] 2 (con False),
                      Component.Stmt opUse [2, 1] 2 [6] 1 (con False)
                    ]
                    [Component.ProgRes (3 :: SymWordN 8) ConstrainedType],
                linearDefUseTestExpectedAssertion =
                  mrgThrowError
                    "Must use the newest definition of ConstrainedType" ::
                    SymbolicContext ()
              },
            LinearDefUseTest
              { linearDefUseTestName = "multiple arguments",
                linearDefUseTestProg =
                  Component.Prog @Op
                    "test"
                    [ Component.ProgArg "arg0" OtherType,
                      Component.ProgArg "arg1" ConstrainedType,
                      Component.ProgArg "arg2" ConstrainedType
                    ]
                    []
                    [Component.ProgRes ("r" :: SymWordN 8) ConstrainedType],
                linearDefUseTestExpectedAssertion =
                  mrgThrowError $
                    "At most one argument with the type ConstrainedType to a "
                      <> "program" ::
                    SymbolicContext ()
              },
            LinearDefUseTest
              { linearDefUseTestName = "multiple results",
                linearDefUseTestProg =
                  Component.Prog @Op
                    "test"
                    [ Component.ProgArg "arg0" OtherType,
                      Component.ProgArg "arg1" ConstrainedType
                    ]
                    []
                    [ Component.ProgRes ("r0" :: SymWordN 8) ConstrainedType,
                      Component.ProgRes ("r1" :: SymWordN 8) ConstrainedType
                    ],
                linearDefUseTestExpectedAssertion =
                  mrgThrowError $
                    "At most one result with the type ConstrainedType to a "
                      <> "program" ::
                    SymbolicContext ()
              }
            ]
        return $
          testCase name $
            constrainProg (LinearDefUse LinearDefUseOp) prog .@?= assertion
    ]
