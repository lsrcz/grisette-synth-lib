{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Program.BuiltinProgConstraints.LivelinessTest
  ( -- Op (..),
  -- Type (..),
  -- LivelinessOp (..),
  -- livelinessIdentifier,
  -- livelinessTest,
  -- anyResource,
  )
where

{-
-- ConstraintHierarchy (ConstraintHierarchy),

import Control.Monad.Error.Class (liftEither)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    Identifier,
    LogicalOp ((.&&), (.||)),
    Mergeable,
    MonadFresh,
    MonadUnion,
    SExpr (List),
    SimpleMergeable (mrgIte),
    Solvable (con, isym),
    SymBool,
    SymEq ((.==)),
    SymWordN,
    ToCon,
    ToSym (toSym),
    Union,
    WordN,
    chooseFresh,
    liftUnion,
    mrgIf,
    mrgReturn,
    runFreshT,
    withMetadata,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Synth.Context
  ( AngelicContext,
    MonadContext,
    SymbolicContext,
  )
import Grisette.Lib.Synth.Operator.OpTyping
  ( OpTyping (OpTypeType, typeOp),
    simpleTyping,
  )
import Grisette.Lib.Synth.Program.BuiltinProgConstraints.Liveliness
  ( ComponentProgDefUse (ComponentProgDefUse),
    ComponentStmtDefUse (ComponentStmtDefUse),
    ComponentUnionUse,
    ComponentUse (ComponentUse),
    Def (Def),
    Liveliness (Liveliness),
    LivelinessName (livelinessName),
    LivelinessOpResource
      ( livelinessOpDefUses
      ),
    LivelinessTypeResource (livelinessTypeDefResource),
    ProgDefUse (ProgDefUse),
    Resource (conflict),
    StmtDefUse (StmtDefUse),
    UnionDef,
    UnionUse,
    Use (Use),
    livelinessComponentProgDefUses,
    livelinessComponentProgStmtDefUses,
    livelinessOpDefUsesByType,
    livelinessProgDefUses,
    livelinessProgStmtDefUses,
    livelinessSubProgDefUses,
    livelinessTypeDefs,
    livelinessTypeUses,
  )
import qualified Grisette.Lib.Synth.Program.ComponentSketch as Component
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.ProgConstraints
  ( OpSubProgConstraints (constrainOpSubProg),
    ProgConstraints (constrainProg),
    pattern ConstraintHierarchy,
  )
import Grisette.Lib.Synth.Program.ProgTyping (lookupType)
import Grisette.Lib.Synth.Program.SumProg (SumProg (SumProgL, SumProgR))
import Grisette.Lib.Synth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Test.SymbolicAssertion ((.@?=))

data Resources = Resources {resource1 :: SymBool, resource2 :: SymBool}
  deriving (Eq, Show, Generic)
  deriving
    (Mergeable, SymEq, SimpleMergeable)
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
  | OpDefAny
  | OpUseAny
  | OpUseDefAny
  | OpSubProg T.Text
  deriving
    ( -- | OpComponentSubProg (Component.Prog Op (SymWordN 8) Type)
      Show,
      Generic
    )
  deriving (Mergeable, ToCon Op, ToSym Op) via (Default Op)

-- instance
--   (MonadContext ctx) =>
--   HasSubProgs
--     Op
--     ( SumProg
--         (Concrete.Prog Op (WordN 8) Type)
--         (Component.Prog Op (SymWordN 8) Type)
--     )
--     ctx
--   where
--   getSubProgs (OpSubProg prog) = mrgReturn [SumProgL prog]
--   getSubProgs (OpComponentSubProg prog) = mrgReturn [SumProgR prog]
--   getSubProgs _ = mrgReturn []

data Type = ConstrainedType | ConstrainedType2 | OtherType | AnyType
  deriving (Show, Generic)
  deriving (Mergeable, ToCon Type, ToSym Type) via (Default Type)

data LivelinessOp = LivelinessOp

instance LivelinessName LivelinessOp where
  livelinessName LivelinessOp = "ConstrainedType"

instance (MonadContext ctx) => OpTyping Op ctx where
  type OpTypeType Op = Type
  typeOp table (OpSubProg sym) = liftEither $ lookupType table sym
  -- typeOp table (OpComponentSubProg sym) = undefined
  typeOp _ op = flip simpleTyping op $ \case
    OpUse -> TypeSignature [OtherType, ConstrainedType] [OtherType]
    OpDef -> TypeSignature [OtherType] [OtherType, ConstrainedType]
    OpNone -> TypeSignature [OtherType] [OtherType]
    OpUseDef -> TypeSignature [ConstrainedType] [ConstrainedType]
    OpDefNoInvalidate ->
      TypeSignature [OtherType] [ConstrainedType]
    OpDefShareScope ->
      TypeSignature [ConstrainedType] [ConstrainedType]
    OpUse2 -> TypeSignature [OtherType, ConstrainedType2] [OtherType]
    OpDef2 -> TypeSignature [OtherType] [OtherType, ConstrainedType2]
    OpUseAny -> TypeSignature [OtherType, AnyType] [OtherType]
    OpDefAny -> TypeSignature [OtherType] [OtherType, AnyType]
    OpUseDefAny -> TypeSignature [AnyType] [AnyType]
    OpDefShareScope2 ->
      TypeSignature [ConstrainedType2] [ConstrainedType2]
    _ -> error "unreachable"

instance
  ( MonadContext ctx,
    MonadFresh ctx,
    MonadUnion ctx,
    Resource Resources,
    ProgConstraints
      (Liveliness LivelinessOp)
      (Concrete.Prog Op (WordN 8) Type)
      ctx
  ) =>
  LivelinessOpResource LivelinessOp Op Resources ctx
  where
  livelinessOpDefUses LivelinessOp table OpDefNoInvalidate argIds resIds disabled = do
    ty <- typeOp table OpDefNoInvalidate
    defs <- livelinessTypeDefs LivelinessOp (resTypes ty) resIds disabled
    uses <- livelinessTypeUses LivelinessOp (argTypes ty) argIds disabled
    mrgReturn $ StmtDefUse defs (mrgReturn []) uses
  livelinessOpDefUses LivelinessOp table OpDefShareScope argIds resIds disabled = do
    ty <- typeOp table OpDefNoInvalidate
    defs <- livelinessTypeDefs LivelinessOp (resTypes ty) resIds disabled
    uses <- livelinessTypeUses LivelinessOp (argTypes ty) argIds disabled
    mrgReturn $ StmtDefUse defs (mrgReturn []) uses
  livelinessOpDefUses LivelinessOp table (OpSubProg prog) argIds resIds disabled =
    livelinessSubProgDefUses LivelinessOp table prog argIds resIds disabled
  -- livelinessOpDefUses
  --   LivelinessOp
  --   (OpComponentSubProg prog)
  --   argIds
  --   resIds
  --   disabled =
  --     livelinessSubProgDefUses LivelinessOp prog argIds resIds disabled
  livelinessOpDefUses LivelinessOp table op argIds resIds disabled =
    livelinessOpDefUsesByType LivelinessOp table op argIds resIds disabled

instance
  (MonadContext ctx, MonadFresh ctx, MonadUnion ctx) =>
  LivelinessTypeResource LivelinessOp Resources Type ctx
  where
  livelinessTypeDefResource LivelinessOp ConstrainedType =
    mrgReturn $ Just mkResource1
  livelinessTypeDefResource LivelinessOp ConstrainedType2 =
    mrgReturn $ Just mkResource2
  livelinessTypeDefResource LivelinessOp OtherType = mrgReturn Nothing
  livelinessTypeDefResource LivelinessOp AnyType = do
    chosen <- chooseFresh [Just mkResource1, Just mkResource2]
    liftUnion chosen

instance
  ( ProgConstraints
      constrObj
      (Concrete.Prog Op (WordN 8) Type)
      ctx,
    ProgConstraints
      constrObj
      (Component.Prog Op (SymWordN 8) Type)
      ctx,
    MonadContext ctx
  ) =>
  OpSubProgConstraints constrObj Op ctx
  where
  constrainOpSubProg obj (OpSubProg prog) = constrainProg obj prog
  constrainOpSubProg obj (OpComponentSubProg prog) = constrainProg obj prog
  constrainOpSubProg _ _ = mrgReturn ()

data LivelinessTest where
  LivelinessTest ::
    ( ProgConstraints obj prog AngelicContext
    ) =>
    { livelinessTestName :: String,
      livelinessTestObj :: obj,
      livelinessTestProg :: prog,
      livelinessTestExpectedAssertion :: SymbolicContext ()
    } ->
    LivelinessTest

opUse :: Union Op
opUse = mrgReturn OpUse

opDef :: Union Op
opDef = mrgReturn OpDef

opNone :: Union Op
opNone = mrgReturn OpNone

opUseDef :: Union Op
opUseDef = mrgReturn OpUseDef

livelinessIdentifier :: T.Text -> [T.Text] -> Identifier
livelinessIdentifier base reversePath =
  withMetadata base $
    ConstraintHierarchy (reverse reversePath) "Liveliness" (List [])

anyResource :: T.Text -> [T.Text] -> Int -> Resources
anyResource base reversePath idx =
  mrgIte
    (isym (livelinessIdentifier base reversePath) idx)
    mkResource1
    mkResource2

innerConcreteAny :: Concrete.Prog Op (WordN 8) Type
innerConcreteAny =
  Concrete.Prog
    "inner"
    [ Concrete.ProgArg "a" 0 OtherType,
      Concrete.ProgArg "b" 1 AnyType
    ]
    [ Concrete.Stmt OpUseAny [0, 1] [2],
      Concrete.Stmt OpDefAny [2] [3, 4],
      Concrete.Stmt OpUseDefAny [4] [5]
    ]
    [Concrete.ProgRes 3 OtherType, Concrete.ProgRes 5 AnyType]

innerComponentAny :: Component.Prog Op (SymWordN 8) Type
innerComponentAny =
  Component.Prog
    "innerc"
    [ Component.ProgArg "a" OtherType,
      Component.ProgArg "b" AnyType
    ]
    [ Component.Stmt OpUseAny [0, 1] 2 [2] 1 "id0" [],
      Component.Stmt OpDefAny [2] 1 [3, 4] 2 "id1" [],
      Component.Stmt OpUseDefAny [4] 1 [5] 1 "id2" []
    ]
    [Component.ProgRes 3 OtherType, Component.ProgRes 5 AnyType]

progAny :: Concrete.Prog Op (WordN 8) Type
progAny =
  Concrete.Prog
    "test"
    [ Concrete.ProgArg "a" 0 OtherType,
      Concrete.ProgArg "b" 1 AnyType
    ]
    [ Concrete.Stmt (OpSubProg innerConcreteAny) [0, 1] [2, 3],
      Concrete.Stmt OpUseAny [2, 3] [4],
      Concrete.Stmt OpDefAny [4] [5, 6],
      Concrete.Stmt OpUseDefAny [6] [7],
      Concrete.Stmt (OpComponentSubProg innerComponentAny) [5, 7] [8, 9]
    ]
    [Concrete.ProgRes 8 OtherType, Concrete.ProgRes 9 AnyType]

argDef1 :: Def (WordN 8) Resources
argDef1 = Def 1 (anyResource "fresh" ["test:arg"] 0) "x"

argDefs :: UnionDef (WordN 8) Resources
argDefs = mrgReturn [argDef1]

stmt0Def1 :: Def (WordN 8) Resources
stmt0Def1 =
  Def 3 (anyResource "fresh" ["test:stmt0", "inner:res"] 0) "x"

stmt0Invalidate1 :: Def (WordN 8) Resources
stmt0Invalidate1 =
  Def 2 (anyResource "fresh" ["test:stmt0", "inner:stmt1"] 0) "x"

stmt0Invalidate2 :: Def (WordN 8) Resources
stmt0Invalidate2 =
  Def 2 (anyResource "fresh" ["test:stmt0", "inner:stmt2"] 0) "x"

stmt0Use1 :: Use (WordN 8) Resources
stmt0Use1 =
  Use 1 (anyResource "fresh" ["test:stmt0", "inner:arg"] 0) "x"

stmt0DefUse :: StmtDefUse (WordN 8) Resources
stmt0DefUse =
  StmtDefUse
    (mrgReturn [stmt0Def1])
    (mrgReturn [stmt0Invalidate1, stmt0Invalidate2])
    (mrgReturn [stmt0Use1])

stmt1Use1 :: Use (WordN 8) Resources
stmt1Use1 = Use 3 (anyResource "fresh" ["test:stmt1"] 0) "x"

stmt1DefUse :: StmtDefUse (WordN 8) Resources
stmt1DefUse =
  StmtDefUse
    (mrgReturn [])
    (mrgReturn [])
    (mrgReturn [stmt1Use1])

stmt2Def1 :: Def (WordN 8) Resources
stmt2Def1 = Def 6 (anyResource "fresh" ["test:stmt2"] 0) "x"

stmt2DefUse :: StmtDefUse (WordN 8) Resources
stmt2DefUse =
  StmtDefUse
    (mrgReturn [stmt2Def1])
    (mrgReturn [stmt2Def1])
    (mrgReturn [])

stmt3Def0 :: Def (WordN 8) Resources
stmt3Def0 = Def 7 (anyResource "fresh" ["test:stmt3"] 0) "x"

stmt3Use0 :: Use (WordN 8) Resources
stmt3Use0 = Use 6 (anyResource "fresh" ["test:stmt3"] 1) "x"

stmt3DefUse :: StmtDefUse (WordN 8) Resources
stmt3DefUse =
  StmtDefUse
    (mrgReturn [stmt3Def0])
    (mrgReturn [stmt3Def0])
    (mrgReturn [stmt3Use0])

stmt4Def1 :: Def (WordN 8) Resources
stmt4Def1 =
  Def 9 (anyResource "fresh" ["test:stmt4", "innerc:res"] 0) "x"

stmt4Invalidate1 :: Def (WordN 8) Resources
stmt4Invalidate1 =
  Def 8 (anyResource "fresh" ["test:stmt4", "innerc:stmt1"] 0) $ "x" .|| "id1"

stmt4Invalidate2 :: Def (WordN 8) Resources
stmt4Invalidate2 =
  Def 8 (anyResource "fresh" ["test:stmt4", "innerc:stmt2"] 0) $ "x" .|| "id2"

stmt4Use1 :: Use (WordN 8) Resources
stmt4Use1 =
  Use 7 (anyResource "fresh" ["test:stmt4", "innerc:arg"] 0) "x"

stmt4DefUse :: StmtDefUse (WordN 8) Resources
stmt4DefUse =
  StmtDefUse
    (mrgReturn [stmt4Def1])
    (mrgReturn [stmt4Invalidate1, stmt4Invalidate2])
    (mrgReturn [stmt4Use1])

resUse1 :: Use (WordN 8) Resources
resUse1 = Use 9 (anyResource "fresh" ["test:res"] 0) "x"

resUses :: UnionUse (WordN 8) Resources
resUses = mrgReturn [resUse1]

progComponentAny :: Component.Prog Op (SymWordN 8) Type
progComponentAny =
  Component.Prog
    "testc"
    [ Component.ProgArg "a" OtherType,
      Component.ProgArg "b" AnyType
    ]
    [ Component.Stmt (OpSubProg innerConcreteAny) [0, 1] 2 [2, 3] 2 "d0" [],
      Component.Stmt OpUseAny [2, 3] 2 [4] 1 "d1" [],
      Component.Stmt OpDefAny [4] 1 [5, 6] 2 "d2" [],
      Component.Stmt OpUseDefAny [6] 1 [7] 1 "d3" [],
      Component.Stmt
        (OpComponentSubProg innerComponentAny)
        [5, 7]
        2
        [8, 9]
        2
        "d4"
        []
    ]
    [Component.ProgRes 8 OtherType, Component.ProgRes 9 AnyType]

componentArgDef1 :: Def (SymWordN 8) Resources
componentArgDef1 = Def 1 (anyResource "fresh" ["testc:arg"] 0) "x"

componentArgDefs :: UnionDef (SymWordN 8) Resources
componentArgDefs = mrgReturn [componentArgDef1]

componentStmt0Def1 :: Def (SymWordN 8) Resources
componentStmt0Def1 =
  Def 3 (anyResource "fresh" ["testc:stmt0", "inner:res"] 0) $ "x" .|| "d0"

componentStmt0Invalidate1 :: Def (SymWordN 8) Resources
componentStmt0Invalidate1 =
  Def 2 (anyResource "fresh" ["testc:stmt0", "inner:stmt1"] 0) $ "x" .|| "d0"

componentStmt0Invalidate2 :: Def (SymWordN 8) Resources
componentStmt0Invalidate2 =
  Def 2 (anyResource "fresh" ["testc:stmt0", "inner:stmt2"] 0) $ "x" .|| "d0"

componentStmt0Use1 :: Use (SymWordN 8) Resources
componentStmt0Use1 =
  Use 1 (anyResource "fresh" ["testc:stmt0", "inner:arg"] 0) $ "x" .|| "d0"

componentStmt0DefUse :: StmtDefUse (SymWordN 8) Resources
componentStmt0DefUse =
  StmtDefUse
    (mrgReturn [componentStmt0Def1])
    (mrgReturn [componentStmt0Invalidate1, componentStmt0Invalidate2])
    (mrgReturn [componentStmt0Use1])

componentStmt0ComponentUse1 :: ComponentUse (SymWordN 8) Resources
componentStmt0ComponentUse1 =
  ComponentUse
    1
    2
    (anyResource "fresh" ["testc:stmt0", "inner:arg"] 0)
    $ "x" .|| "d0"

componentStmt0ComponentDefUse :: ComponentStmtDefUse (SymWordN 8) Resources
componentStmt0ComponentDefUse =
  ComponentStmtDefUse
    (mrgReturn [componentStmt0Def1])
    (mrgReturn [componentStmt0Invalidate1, componentStmt0Invalidate2])
    (mrgReturn [componentStmt0ComponentUse1])

componentStmt1Use1 :: Use (SymWordN 8) Resources
componentStmt1Use1 =
  Use 3 (anyResource "fresh" ["testc:stmt1"] 0) $ "x" .|| "d1"

componentStmt1DefUse :: StmtDefUse (SymWordN 8) Resources
componentStmt1DefUse =
  StmtDefUse
    (mrgReturn [])
    (mrgReturn [])
    (mrgReturn [componentStmt1Use1])

componentStmt1ComponentUse1 :: ComponentUse (SymWordN 8) Resources
componentStmt1ComponentUse1 =
  ComponentUse 3 4 (anyResource "fresh" ["testc:stmt1"] 0) $ "x" .|| "d1"

componentStmt1ComponentDefUse :: ComponentStmtDefUse (SymWordN 8) Resources
componentStmt1ComponentDefUse =
  ComponentStmtDefUse
    (mrgReturn [])
    (mrgReturn [])
    (mrgReturn [componentStmt1ComponentUse1])

componentStmt2Def1 :: Def (SymWordN 8) Resources
componentStmt2Def1 =
  Def 6 (anyResource "fresh" ["testc:stmt2"] 0) $
    "x" .|| "d2"

componentStmt2DefUse :: StmtDefUse (SymWordN 8) Resources
componentStmt2DefUse =
  StmtDefUse
    (mrgReturn [componentStmt2Def1])
    (mrgReturn [componentStmt2Def1])
    (mrgReturn [])

componentStmt2ComponentDefUse :: ComponentStmtDefUse (SymWordN 8) Resources
componentStmt2ComponentDefUse =
  ComponentStmtDefUse
    (mrgReturn [componentStmt2Def1])
    (mrgReturn [componentStmt2Def1])
    (mrgReturn [])

componentStmt3Def0 :: Def (SymWordN 8) Resources
componentStmt3Def0 =
  Def 7 (anyResource "fresh" ["testc:stmt3"] 0) $ "x" .|| "d3"

componentStmt3Use0 :: Use (SymWordN 8) Resources
componentStmt3Use0 =
  Use 6 (anyResource "fresh" ["testc:stmt3"] 1) $ "x" .|| "d3"

componentStmt3DefUse :: StmtDefUse (SymWordN 8) Resources
componentStmt3DefUse =
  StmtDefUse
    (mrgReturn [componentStmt3Def0])
    (mrgReturn [componentStmt3Def0])
    (mrgReturn [componentStmt3Use0])

componentStmt3ComponentUse0 :: ComponentUse (SymWordN 8) Resources
componentStmt3ComponentUse0 =
  ComponentUse 6 7 (anyResource "fresh" ["testc:stmt3"] 1) $ "x" .|| "d3"

componentStmt3ComponentDefUse :: ComponentStmtDefUse (SymWordN 8) Resources
componentStmt3ComponentDefUse =
  ComponentStmtDefUse
    (mrgReturn [componentStmt3Def0])
    (mrgReturn [componentStmt3Def0])
    (mrgReturn [componentStmt3ComponentUse0])

componentStmt4Def1 :: Def (SymWordN 8) Resources
componentStmt4Def1 =
  Def 9 (anyResource "fresh" ["testc:stmt4", "innerc:res"] 0) $ "x" .|| "d4"

componentStmt4Invalidate1 :: Def (SymWordN 8) Resources
componentStmt4Invalidate1 =
  Def 8 (anyResource "fresh" ["testc:stmt4", "innerc:stmt1"] 0) $
    ("x" .|| "d4") .|| "id1"

componentStmt4Invalidate2 :: Def (SymWordN 8) Resources
componentStmt4Invalidate2 =
  Def 8 (anyResource "fresh" ["testc:stmt4", "innerc:stmt2"] 0) $
    ("x" .|| "d4") .|| "id2"

componentStmt4Use1 :: Use (SymWordN 8) Resources
componentStmt4Use1 =
  Use 7 (anyResource "fresh" ["testc:stmt4", "innerc:arg"] 0) $ "x" .|| "d4"

componentStmt4DefUse :: StmtDefUse (SymWordN 8) Resources
componentStmt4DefUse =
  StmtDefUse
    (mrgReturn [componentStmt4Def1])
    (mrgReturn [componentStmt4Invalidate1, componentStmt4Invalidate2])
    (mrgReturn [componentStmt4Use1])

componentStmt4ComponentUse1 :: ComponentUse (SymWordN 8) Resources
componentStmt4ComponentUse1 =
  ComponentUse
    7
    8
    (anyResource "fresh" ["testc:stmt4", "innerc:arg"] 0)
    $ "x" .|| "d4"

componentStmt4ComponentDefUse :: ComponentStmtDefUse (SymWordN 8) Resources
componentStmt4ComponentDefUse =
  ComponentStmtDefUse
    (mrgReturn [componentStmt4Def1])
    (mrgReturn [componentStmt4Invalidate1, componentStmt4Invalidate2])
    (mrgReturn [componentStmt4ComponentUse1])

componentResUse1 :: Use (SymWordN 8) Resources
componentResUse1 = Use 9 (anyResource "fresh" ["testc:res"] 0) "x"

componentResUses :: UnionUse (SymWordN 8) Resources
componentResUses = mrgReturn [componentResUse1]

componentResComponentUse1 :: ComponentUse (SymWordN 8) Resources
componentResComponentUse1 =
  ComponentUse 9 10 (anyResource "fresh" ["testc:res"] 0) "x"

componentResComponentUses :: ComponentUnionUse (SymWordN 8) Resources
componentResComponentUses = mrgReturn [componentResComponentUse1]

livelinessTest :: Test
livelinessTest =
  testGroup
    "Liveliness"
    [ testGroup "livelinessStmtDefUses/Component" $ do
        let expectedDefUses =
              [ componentStmt0DefUse,
                componentStmt1DefUse,
                componentStmt2DefUse,
                componentStmt3DefUse,
                componentStmt4DefUse
              ]
        (idx, expectedDefUse) <- zip [0 ..] expectedDefUses
        return $ testCase ("Stmt" <> show idx) $ do
          let actual =
                flip runFreshT "fresh" $
                  livelinessProgStmtDefUses
                    LivelinessOp
                    progComponentAny
                    idx
                    "x"
          let expected =
                return expectedDefUse ::
                  SymbolicContext (StmtDefUse (SymWordN 8) Resources)
          actual @?= expected,
      testGroup "livelinessStmtDefUses/Concrete" $ do
        let expectedDefUses =
              [ stmt0DefUse,
                stmt1DefUse,
                stmt2DefUse,
                stmt3DefUse,
                stmt4DefUse
              ]
        (idx, expectedDefUse) <- zip [0 ..] expectedDefUses
        return $ testCase ("Stmt" <> show idx) $ do
          let actual =
                flip runFreshT "fresh" $
                  livelinessProgStmtDefUses LivelinessOp progAny idx "x"
          let expected =
                return expectedDefUse ::
                  SymbolicContext (StmtDefUse (WordN 8) Resources)
          actual @?= expected,
      testGroup "livelinessComponentStmtDefUses" $ do
        let expectedDefUses =
              [ componentStmt0ComponentDefUse,
                componentStmt1ComponentDefUse,
                componentStmt2ComponentDefUse,
                componentStmt3ComponentDefUse,
                componentStmt4ComponentDefUse
              ]
        (idx, expectedDefUse) <- zip [0 ..] expectedDefUses
        return $ testCase ("Stmt" <> show idx) $ do
          let actual =
                flip runFreshT "fresh" $
                  livelinessComponentProgStmtDefUses
                    LivelinessOp
                    progComponentAny
                    idx
                    "x"
          let expected =
                return expectedDefUse ::
                  SymbolicContext (ComponentStmtDefUse (SymWordN 8) Resources)
          actual @?= expected,
      testCase "livelinessProgDefUses/Component" $ do
        let actual =
              flip runFreshT "fresh" $
                livelinessProgDefUses LivelinessOp progComponentAny "x"
        let expected =
              return
                ( ProgDefUse
                    componentArgDefs
                    [ componentStmt0DefUse,
                      componentStmt1DefUse,
                      componentStmt2DefUse,
                      componentStmt3DefUse,
                      componentStmt4DefUse
                    ]
                    componentResUses
                ) ::
                SymbolicContext (ProgDefUse (SymWordN 8) Resources)
        actual @?= expected,
      testCase "livelinessProgDefUses/Concrete" $ do
        let actual =
              flip runFreshT "fresh" $
                livelinessProgDefUses LivelinessOp progAny "x"
        let expected =
              return
                ( ProgDefUse
                    argDefs
                    [ stmt0DefUse,
                      stmt1DefUse,
                      stmt2DefUse,
                      stmt3DefUse,
                      stmt4DefUse
                    ]
                    resUses
                ) ::
                SymbolicContext (ProgDefUse (WordN 8) Resources)
        actual @?= expected,
      testCase "livelinessComponentProgDefUses" $ do
        let actual =
              flip runFreshT "fresh" $
                livelinessComponentProgDefUses LivelinessOp progComponentAny "x"
        let expected =
              return
                ( ComponentProgDefUse
                    componentArgDefs
                    [ componentStmt0ComponentDefUse,
                      componentStmt1ComponentDefUse,
                      componentStmt2ComponentDefUse,
                      componentStmt3ComponentDefUse,
                      componentStmt4ComponentDefUse
                    ]
                    componentResComponentUses
                ) ::
                SymbolicContext (ComponentProgDefUse (SymWordN 8) Resources)
        actual @?= expected,
      testGroup "ProgConstraint" $ do
        LivelinessTest name obj prog (assertion :: ctx ()) <-
          [ LivelinessTest
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
              { livelinessTestName = "Concrete/sub prog",
                livelinessTestObj = Liveliness LivelinessOp,
                livelinessTestProg =
                  Concrete.Prog
                    "test"
                    [ Concrete.ProgArg "arg0" 0 OtherType,
                      Concrete.ProgArg "arg1" 1 ConstrainedType
                    ]
                    [ Concrete.Stmt
                        ( OpSubProg $
                            Concrete.Prog
                              "sub"
                              [ Concrete.ProgArg "arg0" 0 OtherType,
                                Concrete.ProgArg "arg1" 1 AnyType
                              ]
                              []
                              [Concrete.ProgRes 1 AnyType]
                        )
                        [0, 1]
                        [2 :: WordN 8]
                    ]
                    [Concrete.ProgRes 2 ConstrainedType],
                livelinessTestExpectedAssertion =
                  mrgIf
                    ( ( isym
                          (livelinessIdentifier "x" ["test:stmt0", "sub:arg"])
                          0 ::
                          SymBool
                      )
                        .&& ( isym
                                ( livelinessIdentifier
                                    "x"
                                    ["test:stmt0", "sub:res"]
                                )
                                0 ::
                                SymBool
                            )
                    )
                    (return ())
                    ( mrgThrowError
                        "Inconsistent use/def resources for ConstrainedType"
                    ) ::
                    SymbolicContext ()
              },
            LivelinessTest
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
              { livelinessTestName = "Component/sub prog",
                livelinessTestObj = Liveliness LivelinessOp,
                livelinessTestProg =
                  Component.Prog
                    "test"
                    [ Component.ProgArg "arg0" OtherType,
                      Component.ProgArg "arg1" ConstrainedType
                    ]
                    [ Component.Stmt
                        ( OpSubProg $
                            Concrete.Prog
                              "sub"
                              [ Concrete.ProgArg "arg0" 0 OtherType,
                                Concrete.ProgArg "arg1" 1 AnyType
                              ]
                              []
                              [Concrete.ProgRes 1 AnyType]
                        )
                        [0, 1]
                        2
                        [2 :: SymWordN 8]
                        1
                        "disabled"
                        []
                    ]
                    [Component.ProgRes 2 ConstrainedType],
                livelinessTestExpectedAssertion =
                  mrgIf
                    ( "disabled"
                        .|| ( ( isym
                                  ( livelinessIdentifier
                                      "x"
                                      ["test:stmt0", "sub:arg"]
                                  )
                                  0 ::
                                  SymBool
                              )
                                .&& ( isym
                                        ( livelinessIdentifier
                                            "x"
                                            ["test:stmt0", "sub:res"]
                                        )
                                        0 ::
                                        SymBool
                                    )
                            )
                    )
                    (return ())
                    ( mrgThrowError
                        "Inconsistent use/def resources for ConstrainedType"
                    ) ::
                    SymbolicContext ()
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
                       ),
                       ( "bad use type",
                         Concrete.Prog
                           "test"
                           [ Concrete.ProgArg "arg0" 0 OtherType,
                             Concrete.ProgArg "arg1" 1 ConstrainedType
                           ]
                           [ Concrete.Stmt OpDef [0] [2, 3],
                             Concrete.Stmt OpUse2 [2, 3] [4]
                           ]
                           [Concrete.ProgRes (4 :: WordN 8) OtherType],
                         mrgThrowError
                           "Inconsistent use/def resources for ConstrainedType"
                       ),
                       ( "correctWithSubProg",
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
            let actual = flip runFreshT "x" $ constrainProg obj prog
            actual .@?= assertion
    ]
-}