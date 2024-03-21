{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Program.BuiltinProgConstraints.LivelinessTest
  ( livelinessTest,
  )
where

import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvaluateSym,
    ITEOp (symIte),
    Mergeable,
    Solvable (con),
    SymWordN,
    ToCon (toCon),
    UnionM,
    WordN,
    mrgIf,
    mrgReturn,
  )
import qualified Grisette
import Grisette.Generics.BoolLike (BoolLike)
import Grisette.Generics.Class.SEq (SEq ((.==)))
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Synth.Context
  ( ConcreteContext,
    MonadContext,
    SymbolicContext,
  )
import Grisette.Lib.Synth.Operator.OpTyping
  ( OpTyping,
    OpTypingSimple (typeOpSimple),
  )
import Grisette.Lib.Synth.Program.BuiltinProgConstraints.Liveliness
  ( ComponentUse (ComponentUse),
    Def (Def),
    Liveliness (Liveliness),
    LivelinessConcrete (LivelinessConcrete),
    LivelinessName (livelinessName),
    LivelinessOpResource
      ( livelinessOpDefs,
        livelinessOpInvalidatingDefs
      ),
    LivelinessTypeResource
      ( livelinessTypeResource
      ),
    Resource (conflict),
    UnionComponentUse,
    UnionDef,
    Use (Use),
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
import Grisette.Lib.Synth.TypeSignature (TypeSignature (TypeSignature))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Test.SymbolicAssertion ((.@?=))

data Resources = Resource1 | Resource2
  deriving (Eq, Show, Generic)
  deriving (Mergeable, Grisette.SEq) via (Default Resources)

instance (BoolLike bool, SEq bool Resources) => Resource bool Resources where
  conflict l r = l .== r

data Op
  = OpUse
  | OpDef
  | OpNone
  | OpUseDef
  | OpDefNoInvalidate
  | OpDefShareScope
  | OpUse2
  | OpDef2
  | OpDefShareScope2
  deriving (Generic)
  deriving (Mergeable, ToCon Op) via (Default Op)

data Type = ConstrainedType | ConstrainedType2 | OtherType
  deriving (Generic)
  deriving (Mergeable, ToCon Type) via (Default Type)

data LivelinessOp = LivelinessOp

instance LivelinessName LivelinessOp where
  livelinessName LivelinessOp = "ConstrainedType"

instance OpTypingSimple Op Type where
  typeOpSimple OpUse = TypeSignature [OtherType, ConstrainedType] [OtherType]
  typeOpSimple OpDef = TypeSignature [OtherType] [OtherType, ConstrainedType]
  typeOpSimple OpNone = TypeSignature [OtherType] [OtherType]
  typeOpSimple OpUseDef = TypeSignature [ConstrainedType] [ConstrainedType]
  typeOpSimple OpDefNoInvalidate =
    TypeSignature [OtherType] [ConstrainedType]
  typeOpSimple OpDefShareScope =
    TypeSignature [ConstrainedType] [ConstrainedType]
  typeOpSimple OpUse2 = TypeSignature [OtherType, ConstrainedType2] [OtherType]
  typeOpSimple OpDef2 = TypeSignature [OtherType] [OtherType, ConstrainedType2]
  typeOpSimple OpDefShareScope2 =
    TypeSignature [ConstrainedType2] [ConstrainedType2]

instance (MonadContext ctx) => OpTyping Op Type ctx

instance
  (MonadContext ctx, Resource bool Resources) =>
  LivelinessOpResource LivelinessOp bool Op Resources ctx
  where
  livelinessOpInvalidatingDefs LivelinessOp OpDefNoInvalidate _ _ = mrgReturn []
  livelinessOpInvalidatingDefs LivelinessOp OpDefShareScope _ _ = mrgReturn []
  livelinessOpInvalidatingDefs LivelinessOp op resIds disabled =
    livelinessOpDefs LivelinessOp op resIds disabled

instance LivelinessTypeResource LivelinessOp Resources Type where
  livelinessTypeResource LivelinessOp ConstrainedType = Just Resource1
  livelinessTypeResource LivelinessOp ConstrainedType2 = Just Resource2
  livelinessTypeResource LivelinessOp OtherType = Nothing

data LivelinessTest where
  LivelinessTest ::
    ( ProgConstraints obj prog ctx,
      Show (ctx ()),
      Grisette.SEq (ctx ()),
      EvaluateSym (ctx ())
    ) =>
    { livelinessTestName :: String,
      livelinessTestObj :: obj,
      livelinessTestProg :: prog,
      livelinessTestExpectedAssertion :: ctx ()
    } ->
    LivelinessTest

opUse :: UnionM Op
opUse = mrgReturn OpUse

opUse2 :: UnionM Op
opUse2 = mrgReturn OpUse2

opDef :: UnionM Op
opDef = mrgReturn OpDef

opDef2 :: UnionM Op
opDef2 = mrgReturn OpDef2

opNone :: UnionM Op
opNone = mrgReturn OpNone

opUseDef :: UnionM Op
opUseDef = mrgReturn OpUseDef

opDefNoInvalidate :: UnionM Op
opDefNoInvalidate = mrgReturn OpDefNoInvalidate

opDefShareScope :: UnionM Op
opDefShareScope = mrgReturn OpDefShareScope

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

livelinessTest :: Test
livelinessTest =
  testGroup
    "Liveliness"
    [ testCase "concreteStmtDef" $ do
        let actual = concreteStmtDef LivelinessOp concreteDefStmt
        let expected =
              mrgReturn [Def 3 Resource1 False] ::
                SymbolicContext [Def Bool (WordN 8) Resources]
        actual @?= expected,
      testCase "concreteStmtUses" $ do
        let actual = concreteStmtUse LivelinessOp concreteUseStmt
        let expected =
              mrgReturn [Use 3 False] ::
                SymbolicContext [Use Bool (WordN 8)]
        actual @?= expected,
      testCase "componentStmtDefs" $ do
        let actual =
              componentStmtDefs LivelinessOp componentDefStmt ::
                SymbolicContext (UnionDef (SymWordN 8) Resources)
        let expected =
              mrgReturn . mrgReturn $
                [Def (symIte "da" "dr1" "dr0") Resource1 "ddisabled"]
        actual @?= expected,
      testCase "componentStmtUses" $ do
        let actual =
              componentStmtUses LivelinessOp componentUseStmt ::
                SymbolicContext (UnionComponentUse (SymWordN 8))
        let expected =
              mrgReturn . mrgReturn $
                [ComponentUse (symIte "ua" "ua1" "ua0") "ur0" "udisabled"]
        actual @?= expected,
      testCase "componentProgDefs" $ do
        let actual =
              componentProgDefs LivelinessOp componentProg ::
                SymbolicContext
                  [UnionDef (SymWordN 8) Resources]
        let expected =
              mrgReturn
                [ mrgReturn [Def 0 Resource1 (con False)],
                  mrgReturn
                    [ Def
                        (symIte "da" "dr1" "dr0")
                        Resource1
                        "ddisabled"
                    ],
                  mrgIf
                    "ua"
                    (mrgReturn [])
                    ( mrgReturn
                        [Def "ur0" Resource1 "udisabled"]
                    )
                ]
        actual @?= expected,
      testCase "componentProgUses" $ do
        let actual =
              componentProgUses LivelinessOp componentProg ::
                SymbolicContext [UnionComponentUse (SymWordN 8)]
        let expected =
              mrgReturn
                [ mrgReturn [ComponentUse "pr1" 6 (con False)],
                  mrgIf
                    "da"
                    (mrgReturn [])
                    (mrgReturn [ComponentUse "da0" "dr0" "ddisabled"]),
                  mrgReturn
                    [ ComponentUse
                        (symIte "ua" "ua1" "ua0")
                        "ur0"
                        "udisabled"
                    ]
                ]
        actual @?= expected,
      testGroup "ProgConstraint" $ do
        LivelinessTest name obj prog (assertion :: ctx ()) <-
          concat
            [ [ LivelinessTest
                  { livelinessTestName = "Component/correct",
                    livelinessTestObj = Liveliness LivelinessOp,
                    livelinessTestProg =
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
                    livelinessTestExpectedAssertion =
                      mrgReturn () :: SymbolicContext ()
                  },
                LivelinessTest
                  { livelinessTestName = "Component/disabled ok",
                    livelinessTestObj = Liveliness LivelinessOp,
                    livelinessTestProg =
                      Component.Prog
                        "test"
                        [ Component.ProgArg "arg0" OtherType,
                          Component.ProgArg "arg1" ConstrainedType
                        ]
                        [ Component.Stmt opDef [0] 1 [2, 3] 2 (con False),
                          Component.Stmt opUseDef [1] 1 [4] 1 (con True)
                        ]
                        [Component.ProgRes (3 :: SymWordN 8) ConstrainedType],
                    livelinessTestExpectedAssertion =
                      mrgReturn () :: SymbolicContext ()
                  },
                LivelinessTest
                  { livelinessTestName = "Concrete/correct",
                    livelinessTestObj = Liveliness LivelinessOp,
                    livelinessTestProg =
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
                    livelinessTestExpectedAssertion =
                      mrgReturn () :: SymbolicContext ()
                  },
                LivelinessTest
                  { livelinessTestName = "Concrete/Concrete/correct",
                    livelinessTestObj = LivelinessConcrete LivelinessOp,
                    livelinessTestProg =
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
                    livelinessTestExpectedAssertion =
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
               in [ LivelinessTest
                      { livelinessTestName = "Component/correct2",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg = correct2,
                        livelinessTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName = "Concrete/correct2",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon correct2 ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName = "Concrete/Concrete/correct2",
                        livelinessTestObj = LivelinessConcrete LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon correct2 ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
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
                  err :: (MonadContext ctx) => ctx ()
                  err =
                    mrgThrowError
                      "Cannot use invalidated resource for ConstrainedType"
               in [ LivelinessTest
                      { livelinessTestName =
                          "Component/not using newest def but using from arg",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg = notUsingNewestDefButUsingFromArg,
                        livelinessTestExpectedAssertion =
                          err :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName =
                          "Concrete/not using newest def but using from arg",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon notUsingNewestDefButUsingFromArg ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
                          err :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName =
                          "Concrete/Concrete/not using newest def but using "
                            <> "from arg",
                        livelinessTestObj = LivelinessConcrete LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon notUsingNewestDefButUsingFromArg ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
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
                  err :: (MonadContext ctx) => ctx ()
                  err =
                    mrgThrowError
                      "Cannot use invalidated resource for ConstrainedType"
               in [ LivelinessTest
                      { livelinessTestName = "Component/not using newest def",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg = notUsingNewestDef,
                        livelinessTestExpectedAssertion =
                          err :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName = "Concrete/not using newest def",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon notUsingNewestDef ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
                          err :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName =
                          "Concrete/Concrete/not using newest def",
                        livelinessTestObj = LivelinessConcrete LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon notUsingNewestDef ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
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
                  err :: (MonadContext ctx) => ctx ()
                  err =
                    mrgThrowError
                      "Cannot use invalidated resource for ConstrainedType"
               in [ LivelinessTest
                      { livelinessTestName =
                          "Component/res not using newest def, use arg",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg = prog,
                        livelinessTestExpectedAssertion =
                          err :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName =
                          "Concrete/res not using newest def, use arg",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
                          err :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName =
                          "Concrete/Concrete/res not using newest def, use arg",
                        livelinessTestObj = LivelinessConcrete LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
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
                  err :: (MonadContext ctx) => ctx ()
                  err =
                    mrgThrowError
                      "Cannot use invalidated resource for ConstrainedType"
               in [ LivelinessTest
                      { livelinessTestName =
                          "Component/res not using newest def, use old stmt",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg = prog,
                        livelinessTestExpectedAssertion =
                          err :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName =
                          "Concrete/res not using newest def, use old stmt",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
                          err :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName =
                          "Concrete/Concrete/res not using newest def, use old "
                            <> "stmt",
                        livelinessTestObj = LivelinessConcrete LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
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
                      [ Component.ProgRes (2 :: SymWordN 8) ConstrainedType,
                        Component.ProgRes (1 :: SymWordN 8) ConstrainedType
                      ]
               in [ LivelinessTest
                      { livelinessTestName = "Component/multiple arguments",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg = prog,
                        livelinessTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName = "Concrete/multiple arguments",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName =
                          "Concrete/Concrete/multiple arguments",
                        livelinessTestObj = LivelinessConcrete LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
                          mrgReturn () :: ConcreteContext ()
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
               in [ LivelinessTest
                      { livelinessTestName =
                          "Component/no invalidate should not invalidate",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg = prog,
                        livelinessTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName =
                          "Concrete/no invalidate should not invalidate",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName =
                          "Concrete/Concrete/no invalidate should not "
                            <> "invalidate",
                        livelinessTestObj = LivelinessConcrete LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
                          mrgReturn () :: ConcreteContext ()
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
               in [ LivelinessTest
                      { livelinessTestName =
                          "Component/use no invalidate ok",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg = prog,
                        livelinessTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName =
                          "Concrete/use no invalidate ok",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName =
                          "Concrete/Concrete/use no invalidate ok",
                        livelinessTestObj = LivelinessConcrete LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
                          mrgReturn () :: ConcreteContext ()
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
                      [Component.ProgRes (3 :: SymWordN 8) ConstrainedType]
               in [ LivelinessTest
                      { livelinessTestName =
                          "Component/use old no invalidate ok",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg = prog,
                        livelinessTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName =
                          "Concrete/use old no invalidate ok",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName =
                          "Concrete/Concrete/use old no invalidate ok",
                        livelinessTestObj = LivelinessConcrete LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
                          mrgReturn () :: ConcreteContext ()
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
                          opDefShareScope
                          [3]
                          1
                          [4]
                          1
                          (con False),
                        Component.Stmt opUse [2, 3] 2 [5] 1 (con False),
                        Component.Stmt opUse [2, 4] 2 [6] 1 (con False)
                      ]
                      [ Component.ProgRes (3 :: SymWordN 8) ConstrainedType,
                        Component.ProgRes (4 :: SymWordN 8) ConstrainedType
                      ]
               in [ LivelinessTest
                      { livelinessTestName =
                          "Component/no invalidate should not invalidate",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg = prog,
                        livelinessTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName =
                          "Concrete/no invalidate should not invalidate",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName =
                          "Concrete/Concrete/no invalidate should not "
                            <> "invalidate",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      }
                  ],
              let prog =
                    Component.Prog
                      "test"
                      [ Component.ProgArg "arg0" ConstrainedType,
                        Component.ProgArg "arg1" ConstrainedType2,
                        Component.ProgArg "arg2" OtherType
                      ]
                      [ Component.Stmt opUse [2, 0] 2 [3] 1 (con False),
                        Component.Stmt opUse2 [2, 1] 2 [4] 1 (con False),
                        Component.Stmt opDef [0] 1 [5, 6] 2 (con False),
                        Component.Stmt opDef2 [1] 1 [7, 8] 2 (con False),
                        Component.Stmt opUse [2, 6] 2 [9] 1 (con False),
                        Component.Stmt opUse2 [2, 8] 2 [10] 1 (con False)
                      ]
                      [ Component.ProgRes (6 :: SymWordN 8) ConstrainedType,
                        Component.ProgRes (8 :: SymWordN 8) ConstrainedType2
                      ]
               in [ LivelinessTest
                      { livelinessTestName = "Component/multiple resources",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg = prog,
                        livelinessTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName = "Concrete/multiple resources",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
                          mrgReturn () :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName =
                          "Concrete/Concrete/multiple resources",
                        livelinessTestObj = LivelinessConcrete LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
                          mrgReturn () :: ConcreteContext ()
                      }
                  ],
              let prog =
                    Component.Prog
                      "test"
                      [ Component.ProgArg "arg0" ConstrainedType,
                        Component.ProgArg "arg1" ConstrainedType2,
                        Component.ProgArg "arg2" OtherType
                      ]
                      [ Component.Stmt opUse [2, 0] 2 [3] 1 (con False),
                        Component.Stmt opUse2 [2, 1] 2 [4] 1 (con False),
                        Component.Stmt opDef [0] 1 [5, 6] 2 (con False),
                        Component.Stmt opDef2 [1] 1 [7, 8] 2 (con False),
                        Component.Stmt opUse [2, 0] 2 [9] 1 (con False),
                        Component.Stmt opUse2 [2, 8] 2 [10] 1 (con False)
                      ]
                      [ Component.ProgRes (6 :: SymWordN 8) ConstrainedType,
                        Component.ProgRes (8 :: SymWordN 8) ConstrainedType2
                      ]
                  err :: (MonadContext ctx) => ctx ()
                  err =
                    mrgThrowError
                      "Cannot use invalidated resource for ConstrainedType"
               in [ LivelinessTest
                      { livelinessTestName =
                          "Component/multiple resources failure",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg = prog,
                        livelinessTestExpectedAssertion =
                          err :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName =
                          "Concrete/multiple resources failure",
                        livelinessTestObj = Liveliness LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
                          err :: SymbolicContext ()
                      },
                    LivelinessTest
                      { livelinessTestName =
                          "Concrete/Concrete/multiple resources failure",
                        livelinessTestObj = LivelinessConcrete LivelinessOp,
                        livelinessTestProg =
                          fromJust $ toCon prog ::
                            Concrete.Prog Op (WordN 8) Type,
                        livelinessTestExpectedAssertion =
                          err :: ConcreteContext ()
                      }
                  ]
            ]
        return $
          testCase name $ do
            let actual = constrainProg obj prog
            actual .@?= assertion
    ]
