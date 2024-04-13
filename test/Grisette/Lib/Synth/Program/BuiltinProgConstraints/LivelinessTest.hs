{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Program.BuiltinProgConstraints.LivelinessTest
  ( Op (..),
    Type (..),
    LivelinessOp (..),
    livelinessTest,
  )
where

import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvaluateSym,
    ITEOp (symIte),
    LogicalOp ((.||)),
    Mergeable,
    SEq ((.==)),
    SimpleMergeable (mrgIte),
    Solvable (con),
    SymBool,
    SymWordN,
    ToCon,
    ToSym (toSym),
    UnionM,
    WordN,
    mrgIf,
    mrgReturn,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Synth.Context
  ( MonadContext,
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
    LivelinessName (livelinessName),
    LivelinessOpResource
      ( livelinessOpDefs,
        livelinessOpInvalidatingDefs
      ),
    LivelinessTypeResource
      ( livelinessTypeDefResource
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
    livelinessComponentProgInvalidatingDefs,
    livelinessConcreteProgInvalidatingDefs,
  )
import qualified Grisette.Lib.Synth.Program.ComponentSketch as Component
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.ProgConstraints
  ( OpSubProgConstraints (constrainOpSubProg),
    ProgConstraints (constrainProg),
  )
import Grisette.Lib.Synth.TypeSignature (TypeSignature (TypeSignature))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Test.SymbolicAssertion ((.@?=))

data Resources = Resources {resource1 :: SymBool, resource2 :: SymBool}
  deriving (Eq, Show, Generic)
  deriving
    (Mergeable, SEq, SimpleMergeable)
    via (Default Resources)

mkResource1 :: Resources
mkResource1 = Resources (toSym True) (toSym False)

mkResource2 :: Resources
mkResource2 = Resources (toSym False) (toSym True)

instance Resource Resources where
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
  | OpSubProg (Concrete.Prog Op (WordN 8) Type)
  deriving (Show, Generic)
  deriving (Mergeable, ToCon Op, ToSym Op) via (Default Op)

data Type = ConstrainedType | ConstrainedType2 | OtherType
  deriving (Show, Generic)
  deriving (Mergeable, ToCon Type, ToSym Type) via (Default Type)

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
  typeOpSimple (OpSubProg prog) =
    TypeSignature
      (Concrete.progArgType <$> Concrete.progArgList prog)
      (Concrete.progResType <$> Concrete.progResList prog)

instance (MonadContext ctx) => OpTyping Op Type ctx

instance
  ( MonadContext ctx,
    Resource Resources,
    ProgConstraints
      (Liveliness LivelinessOp)
      (Concrete.Prog Op (WordN 8) Type)
      ctx
  ) =>
  LivelinessOpResource LivelinessOp Op Resources ctx
  where
  livelinessOpInvalidatingDefs LivelinessOp OpDefNoInvalidate _ _ = mrgReturn []
  livelinessOpInvalidatingDefs LivelinessOp OpDefShareScope _ _ = mrgReturn []
  livelinessOpInvalidatingDefs LivelinessOp (OpSubProg prog) resIds disabled =
    livelinessConcreteProgInvalidatingDefs
      LivelinessOp
      prog
      (head resIds)
      disabled
  livelinessOpInvalidatingDefs LivelinessOp op resIds disabled =
    livelinessOpDefs LivelinessOp op resIds disabled

instance
  ( ProgConstraints
      constrObj
      (Concrete.Prog Op (WordN 8) Type)
      ctx,
    MonadContext ctx
  ) =>
  OpSubProgConstraints constrObj Op ctx
  where
  constrainOpSubProg obj (OpSubProg prog) = constrainProg obj prog
  constrainOpSubProg _ _ = mrgReturn ()

instance LivelinessTypeResource LivelinessOp Resources Type where
  livelinessTypeDefResource LivelinessOp ConstrainedType = Just mkResource1
  livelinessTypeDefResource LivelinessOp ConstrainedType2 = Just mkResource2
  livelinessTypeDefResource LivelinessOp OtherType = Nothing

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

opDef :: UnionM Op
opDef = mrgReturn OpDef

opDef2 :: UnionM Op
opDef2 = mrgReturn OpDef2

opNone :: UnionM Op
opNone = mrgReturn OpNone

opUseDef :: UnionM Op
opUseDef = mrgReturn OpUseDef

concreteDefStmt :: Concrete.Stmt Op (WordN 8)
concreteDefStmt = Concrete.Stmt OpDef [0] [2, 3]

componentDefStmt :: Component.Stmt (UnionM Op) (SymWordN 8)
componentDefStmt =
  Component.Stmt
    (mrgIf "da" opDef opUseDef)
    ["da0", "da1"]
    "dna"
    ["dr0", "dr1"]
    "dnr"
    "ddisabled"
    []

concreteUseStmt :: Concrete.Stmt Op (WordN 8)
concreteUseStmt = Concrete.Stmt OpUse [0, 3] [4]

componentUseStmt :: Component.Stmt (UnionM Op) (SymWordN 8)
componentUseStmt =
  Component.Stmt
    (mrgIf "ua" opUse opUseDef)
    ["ua0", "ua1"]
    "una"
    ["ur0", "ur1"]
    "unr"
    "udisabled"
    []

componentProg :: Component.Prog (UnionM Op) (SymWordN 8) Type
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
              mrgReturn [Def 3 mkResource1 $ con False] ::
                SymbolicContext [Def (WordN 8) Resources]
        actual @?= expected,
      testCase "concreteStmtUses" $ do
        let actual = concreteStmtUse LivelinessOp concreteUseStmt
        let expected =
              mrgReturn [Use 3 $ con False] ::
                SymbolicContext [Use (WordN 8)]
        actual @?= expected,
      testCase "componentStmtDefs" $ do
        let actual =
              componentStmtDefs LivelinessOp componentDefStmt ::
                SymbolicContext (UnionDef (SymWordN 8) Resources)
        let expected =
              mrgReturn . mrgReturn $
                [Def (symIte "da" "dr1" "dr0") mkResource1 "ddisabled"]
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
                [ mrgReturn [Def 0 mkResource1 (con False)],
                  mrgReturn
                    [ Def
                        (symIte "da" "dr1" "dr0")
                        mkResource1
                        "ddisabled"
                    ],
                  mrgIf
                    "ua"
                    (mrgReturn [])
                    ( mrgReturn
                        [Def "ur0" mkResource1 "udisabled"]
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
          [ LivelinessTest
              { livelinessTestName = "Component/correct",
                livelinessTestObj = Liveliness LivelinessOp,
                livelinessTestProg =
                  Component.Prog
                    "test"
                    [ Component.ProgArg "arg0" OtherType,
                      Component.ProgArg "arg1" ConstrainedType
                    ]
                    [ Component.Stmt opUse [0, 1] 2 [2] 1 (con False) [],
                      Component.Stmt opDef [0] 1 [3, 4] 2 (con False) [],
                      Component.Stmt opUse [3, 4] 2 [5] 1 (con False) [],
                      Component.Stmt opUseDef [4] 1 [6] 1 (con False) [],
                      Component.Stmt opUse [3, 6] 2 [7] 1 (con False) [],
                      Component.Stmt opNone [7] 2 [8] 1 (con False) []
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
                    [ Component.Stmt opDef [0] 1 [2, 3] 2 (con False) [],
                      Component.Stmt opUseDef [1] 1 [4] 1 (con True) []
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
              }
            ]
            ++ ( do
                   let success = mrgReturn () :: SymbolicContext ()
                   let err =
                         mrgThrowError $
                           "Cannot use invalidated resource for "
                             <> "ConstrainedType" ::
                           SymbolicContext ()
                   let subProg =
                         Concrete.Prog
                           "subProg"
                           [ Concrete.ProgArg "arg0" 0 OtherType,
                             Concrete.ProgArg "arg1" (-1) ConstrainedType,
                             Concrete.ProgArg "arg2" (-2) ConstrainedType2
                           ]
                           [ Concrete.Stmt OpUse [0, -1] [2],
                             Concrete.Stmt OpDef [0] [3, 4],
                             Concrete.Stmt OpUse [3, 4] [5],
                             Concrete.Stmt OpUseDef [4] [9],
                             Concrete.Stmt OpUse [3, 9] [7],
                             Concrete.Stmt OpNone [7] [8]
                           ]
                           [ Concrete.ProgRes 9 ConstrainedType,
                             Concrete.ProgRes (-2) ConstrainedType2
                           ]
                   let invalidSubProg =
                         Concrete.Prog
                           "subProg"
                           [ Concrete.ProgArg "arg0" 0 OtherType,
                             Concrete.ProgArg "arg1" (-1) ConstrainedType
                           ]
                           [ Concrete.Stmt OpDef [0] [3, 4],
                             Concrete.Stmt OpUse [3, -1] [5]
                           ]
                           [ Concrete.ProgRes 4 ConstrainedType
                           ]
                   (name, prog, res) <-
                     [ ( "correct2",
                         Concrete.Prog
                           "test"
                           [ Concrete.ProgArg "arg0" 0 OtherType,
                             Concrete.ProgArg "arg1" 1 ConstrainedType
                           ]
                           [ Concrete.Stmt OpDef [0] [2, 3],
                             Concrete.Stmt OpDef [0] [4, 5],
                             Concrete.Stmt OpUse [2, 5] [6]
                           ]
                           [Concrete.ProgRes (5 :: WordN 8) ConstrainedType],
                         success
                       ),
                       ( "not using newest def but using from arg",
                         Concrete.Prog
                           "test"
                           [ Concrete.ProgArg "arg0" 0 OtherType,
                             Concrete.ProgArg "arg1" 1 ConstrainedType
                           ]
                           [ Concrete.Stmt OpDef [0] [2, 3],
                             Concrete.Stmt OpDef [0] [4, 5],
                             Concrete.Stmt OpUse [2, 1] [6]
                           ]
                           [Concrete.ProgRes 5 ConstrainedType],
                         err
                       ),
                       ( "not using newest def",
                         Concrete.Prog
                           "test"
                           [ Concrete.ProgArg "arg0" 0 OtherType,
                             Concrete.ProgArg "arg1" 1 ConstrainedType
                           ]
                           [ Concrete.Stmt OpDef [0] [2, 3],
                             Concrete.Stmt OpDef [0] [4, 5],
                             Concrete.Stmt OpUse [2, 3] [6]
                           ]
                           [Concrete.ProgRes 5 ConstrainedType],
                         err
                       ),
                       ( "res not using newest def, usg arg",
                         Concrete.Prog
                           "test"
                           [ Concrete.ProgArg "arg0" 0 OtherType,
                             Concrete.ProgArg "arg1" 1 ConstrainedType
                           ]
                           [ Concrete.Stmt OpDef [0] [2, 3],
                             Concrete.Stmt OpDef [0] [4, 5],
                             Concrete.Stmt OpUse [2, 1] [6]
                           ]
                           [Concrete.ProgRes 1 ConstrainedType],
                         err
                       ),
                       ( "res not using newest def, use old stmt",
                         Concrete.Prog
                           "test"
                           [ Concrete.ProgArg "arg0" 0 OtherType,
                             Concrete.ProgArg "arg1" 1 ConstrainedType
                           ]
                           [ Concrete.Stmt OpDef [0] [2, 3],
                             Concrete.Stmt OpDef [0] [4, 5],
                             Concrete.Stmt OpUse [2, 1] [6]
                           ]
                           [Concrete.ProgRes 3 ConstrainedType],
                         err
                       ),
                       ( "multiple arguments",
                         Concrete.Prog
                           "test"
                           [ Concrete.ProgArg "arg0" 0 OtherType,
                             Concrete.ProgArg "arg1" 1 ConstrainedType,
                             Concrete.ProgArg "arg2" 2 ConstrainedType
                           ]
                           []
                           [ Concrete.ProgRes 2 ConstrainedType,
                             Concrete.ProgRes 1 ConstrainedType
                           ],
                         success
                       ),
                       ( "no invalidate should not invalidate",
                         Concrete.Prog
                           "test"
                           [ Concrete.ProgArg "arg0" 0 OtherType,
                             Concrete.ProgArg "arg1" 1 ConstrainedType
                           ]
                           [ Concrete.Stmt OpDef [0] [2, 3],
                             Concrete.Stmt OpDefNoInvalidate [1] [4],
                             Concrete.Stmt OpUse [2, 3] [5]
                           ]
                           [Concrete.ProgRes 3 ConstrainedType],
                         success
                       ),
                       ( "no use invalidate ok",
                         Concrete.Prog
                           "test"
                           [ Concrete.ProgArg "arg0" 0 OtherType,
                             Concrete.ProgArg "arg1" 1 ConstrainedType
                           ]
                           [ Concrete.Stmt OpDef [0] [2, 3],
                             Concrete.Stmt OpDefNoInvalidate [1] [4]
                           ]
                           [Concrete.ProgRes 4 ConstrainedType],
                         success
                       ),
                       ( "use old no invalidate ok",
                         Concrete.Prog
                           "test"
                           [ Concrete.ProgArg "arg0" 0 OtherType,
                             Concrete.ProgArg "arg1" 1 ConstrainedType
                           ]
                           [ Concrete.Stmt OpDef [0] [2, 3],
                             Concrete.Stmt OpDefNoInvalidate [1] [4]
                           ]
                           [Concrete.ProgRes 3 ConstrainedType],
                         success
                       ),
                       ( "no invalidate should not invalidate",
                         Concrete.Prog
                           "test"
                           [ Concrete.ProgArg "arg0" 0 OtherType,
                             Concrete.ProgArg "arg1" 1 ConstrainedType
                           ]
                           [ Concrete.Stmt OpDef [0] [2, 3],
                             Concrete.Stmt OpDefShareScope [3] [4],
                             Concrete.Stmt OpUse [2, 3] [5],
                             Concrete.Stmt OpUse [2, 4] [6]
                           ]
                           [ Concrete.ProgRes 3 ConstrainedType,
                             Concrete.ProgRes 4 ConstrainedType
                           ],
                         success
                       ),
                       ( "multiple resources",
                         Concrete.Prog
                           "test"
                           [ Concrete.ProgArg "arg0" 0 ConstrainedType,
                             Concrete.ProgArg "arg1" 1 ConstrainedType2,
                             Concrete.ProgArg "arg2" 2 OtherType
                           ]
                           [ Concrete.Stmt OpUse [2, 0] [3],
                             Concrete.Stmt OpUse2 [2, 1] [4],
                             Concrete.Stmt OpDef [0] [5, 6],
                             Concrete.Stmt OpDef2 [1] [7, 8],
                             Concrete.Stmt OpUse [2, 6] [9],
                             Concrete.Stmt OpUse2 [2, 8] [10]
                           ]
                           [ Concrete.ProgRes 6 ConstrainedType,
                             Concrete.ProgRes 8 ConstrainedType2
                           ],
                         success
                       ),
                       ( "multiple resources failure",
                         Concrete.Prog
                           "test"
                           [ Concrete.ProgArg "arg0" 0 ConstrainedType,
                             Concrete.ProgArg "arg1" 1 ConstrainedType2,
                             Concrete.ProgArg "arg2" 2 OtherType
                           ]
                           [ Concrete.Stmt OpUse [2, 0] [3],
                             Concrete.Stmt OpUse2 [2, 1] [4],
                             Concrete.Stmt OpDef [0] [5, 6],
                             Concrete.Stmt OpDef2 [1] [7, 8],
                             Concrete.Stmt OpUse [2, 0] [9],
                             Concrete.Stmt OpUse2 [2, 8] [10]
                           ]
                           [ Concrete.ProgRes 6 ConstrainedType,
                             Concrete.ProgRes 8 ConstrainedType2
                           ],
                         err
                       )
                       ]
                       ++ [ ( "correctWithSubProg",
                              Concrete.Prog
                                "test"
                                [ Concrete.ProgArg "arg0" 0 OtherType,
                                  Concrete.ProgArg "arg1" 1 ConstrainedType,
                                  Concrete.ProgArg "arg2" 2 ConstrainedType2
                                ]
                                [ Concrete.Stmt
                                    (OpSubProg subProg)
                                    [0, 1, 2]
                                    [3, 4],
                                  Concrete.Stmt OpUse [0, 3] [5],
                                  Concrete.Stmt OpUse2 [0, 2] [6],
                                  Concrete.Stmt OpUse2 [0, 4] [7]
                                ]
                                [Concrete.ProgRes 3 ConstrainedType],
                              success
                            ),
                            ( "useSubProgInvalidated",
                              Concrete.Prog
                                "test"
                                [ Concrete.ProgArg "arg0" 0 OtherType,
                                  Concrete.ProgArg "arg1" 1 ConstrainedType,
                                  Concrete.ProgArg "arg2" 2 ConstrainedType2
                                ]
                                [ Concrete.Stmt
                                    (OpSubProg subProg)
                                    [0, 1, 2]
                                    [3, 4],
                                  Concrete.Stmt OpUse [0, 1] [5]
                                ]
                                [Concrete.ProgRes 3 ConstrainedType],
                              err
                            ),
                            ( "useInvalidSubProg",
                              Concrete.Prog
                                "test"
                                [ Concrete.ProgArg "arg0" 0 OtherType,
                                  Concrete.ProgArg "arg1" 1 ConstrainedType
                                ]
                                [ Concrete.Stmt
                                    (OpSubProg invalidSubProg)
                                    [0, 1]
                                    [2]
                                ]
                                [Concrete.ProgRes 2 ConstrainedType],
                              err
                            )
                          ]
                   [ LivelinessTest
                       { livelinessTestName = "Component/" <> name,
                         livelinessTestObj = Liveliness LivelinessOp,
                         livelinessTestProg =
                           toSym prog :: Component.Prog Op (SymWordN 8) Type,
                         livelinessTestExpectedAssertion = res
                       },
                     LivelinessTest
                       { livelinessTestName = "Concrete/" <> name,
                         livelinessTestObj = Liveliness LivelinessOp,
                         livelinessTestProg = prog,
                         livelinessTestExpectedAssertion = res
                       }
                     ]
               )
        return $
          testCase name $ do
            let actual = constrainProg obj prog
            actual .@?= assertion,
      testCase "livelinessConcreteProgInvalidatingDefs" $ do
        let prog =
              Concrete.Prog
                "test"
                [ Concrete.ProgArg "arg0" 0 OtherType,
                  Concrete.ProgArg "arg1" (-1) ConstrainedType
                ]
                [ Concrete.Stmt OpDef [0] [2, 3],
                  Concrete.Stmt OpDef2 [0] [4, 5],
                  Concrete.Stmt OpUseDef [3] [6]
                ]
                [Concrete.ProgRes (3 :: WordN 8) ConstrainedType]
        let actual =
              livelinessConcreteProgInvalidatingDefs
                LivelinessOp
                prog
                (10 :: WordN 8)
                (con False)
        let expected =
              mrgReturn
                [ Def 10 mkResource1 (con False),
                  Def 10 mkResource2 (con False),
                  Def 10 mkResource1 (con False)
                ] ::
                SymbolicContext [Def (WordN 8) Resources]
        actual @?= expected,
      testCase "livelinessComponentProgInvalidatingDefs" $ do
        let prog =
              Component.Prog
                "test"
                [ Component.ProgArg "arg0" OtherType,
                  Component.ProgArg "arg1" ConstrainedType
                ]
                [ Component.Stmt
                    (mrgIf "a" opDef opUse)
                    [0, 1]
                    1
                    [2, 3]
                    2
                    "disabled1"
                    [],
                  Component.Stmt
                    (mrgIf "b" opDef opDef2)
                    [0]
                    1
                    [4, 5]
                    2
                    "disabled2"
                    [],
                  Component.Stmt opUseDef [3] 2 [6] 1 "disabled3" []
                ]
                [Component.ProgRes (3 :: SymWordN 8) ConstrainedType]
        let actual =
              livelinessComponentProgInvalidatingDefs
                LivelinessOp
                prog
                (10 :: WordN 8)
                "disabled"
        let expected =
              mrgReturn
                [ Def
                    10
                    mkResource1
                    (symIte "a" "disabled1" (con True) .|| "disabled"),
                  Def
                    10
                    (mrgIte "b" mkResource1 mkResource2)
                    ("disabled2" .|| "disabled"),
                  Def 10 mkResource1 ("disabled3" .|| "disabled")
                ] ::
                SymbolicContext [Def (WordN 8) Resources]
        actual @?= expected
    ]
