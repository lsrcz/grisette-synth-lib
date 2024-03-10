{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Program.BuiltinProgConstraints.LinearDefUseTest
  ( linearDefUseTest,
  )
where

import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvaluateSym,
    Mergeable,
    SEq,
    Solvable (con),
    SymWordN,
    mrgReturn,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Synth.Context (SymbolicContext)
import Grisette.Lib.Synth.Program.BuiltinProgConstraints.LinearDefUse
  ( Def (Def),
    LinearDefUse (LinearDefUse),
    LinearDefUseExtract (linearDefUseExtractDefs, linearDefUseExtractUses),
    LinearDefUseName (linearDefUseName),
    LinearDefUseTypePredicate (linearDefUseTypePredicate),
    Use (Use),
  )
import qualified Grisette.Lib.Synth.Program.ComponentSketch as Component
import Grisette.Lib.Synth.Program.ProgConstraints
  ( ProgConstraints (constrainProg),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
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

linearDefUseTest :: Test
linearDefUseTest = testGroup "LinearDefUse" $ do
  LinearDefUseTest name prog (assertion :: ctx ()) <-
    [ LinearDefUseTest
        { linearDefUseTestName = "correct",
          linearDefUseTestProg =
            Component.Prog
              "test"
              [ Component.ProgArg "arg0" OtherType,
                Component.ProgArg "arg1" ConstrainedType
              ]
              [ Component.Stmt (mrgReturn OpUse) [0, 1] 2 [2] 1 (con False),
                Component.Stmt (mrgReturn OpDef) [0] 1 [3, 4] 2 (con False),
                Component.Stmt (mrgReturn OpUse) [3, 4] 2 [5] 1 (con False),
                Component.Stmt (mrgReturn OpUseDef) [4] 1 [6] 1 (con False),
                Component.Stmt (mrgReturn OpUse) [3, 6] 2 [7] 1 (con False)
              ]
              [Component.ProgRes (6 :: SymWordN 8) ConstrainedType],
          linearDefUseTestExpectedAssertion = mrgReturn () :: SymbolicContext ()
        },
      LinearDefUseTest
        { linearDefUseTestName = "correct2",
          linearDefUseTestProg =
            Component.Prog
              "test"
              [ Component.ProgArg "arg0" OtherType,
                Component.ProgArg "arg1" ConstrainedType
              ]
              [ Component.Stmt (mrgReturn OpDef) [0] 1 [2, 3] 2 (con False),
                Component.Stmt (mrgReturn OpDef) [0] 1 [4, 5] 2 (con False),
                Component.Stmt (mrgReturn OpUse) [2, 5] 2 [6] 1 (con False)
              ]
              [Component.ProgRes (5 :: SymWordN 8) ConstrainedType],
          linearDefUseTestExpectedAssertion = mrgReturn () :: SymbolicContext ()
        },
      LinearDefUseTest
        { linearDefUseTestName = "not using newest def but using from arg",
          linearDefUseTestProg =
            Component.Prog
              "test"
              [ Component.ProgArg "arg0" OtherType,
                Component.ProgArg "arg1" ConstrainedType
              ]
              [ Component.Stmt (mrgReturn OpDef) [0] 1 [2, 3] 2 (con False),
                Component.Stmt (mrgReturn OpDef) [0] 1 [4, 5] 2 (con False),
                Component.Stmt (mrgReturn OpUse) [2, 1] 2 [6] 1 (con False)
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
              [ Component.Stmt (mrgReturn OpDef) [0] 1 [2, 3] 2 (con False),
                Component.Stmt (mrgReturn OpDef) [0] 1 [4, 5] 2 (con False),
                Component.Stmt (mrgReturn OpUse) [2, 3] 2 [6] 1 (con False)
              ]
              [Component.ProgRes (5 :: SymWordN 8) ConstrainedType],
          linearDefUseTestExpectedAssertion =
            mrgThrowError
              "Must use the newest definition of ConstrainedType" ::
              SymbolicContext ()
        },
      LinearDefUseTest
        { linearDefUseTestName = "res not using newest def but using from arg",
          linearDefUseTestProg =
            Component.Prog
              "test"
              [ Component.ProgArg "arg0" OtherType,
                Component.ProgArg "arg1" ConstrainedType
              ]
              [ Component.Stmt (mrgReturn OpDef) [0] 1 [2, 3] 2 (con False),
                Component.Stmt (mrgReturn OpDef) [0] 1 [4, 5] 2 (con False),
                Component.Stmt (mrgReturn OpUse) [2, 1] 2 [6] 1 (con False)
              ]
              [Component.ProgRes (1 :: SymWordN 8) ConstrainedType],
          linearDefUseTestExpectedAssertion =
            mrgThrowError
              "Must use the newest definition of ConstrainedType" ::
              SymbolicContext ()
        },
      LinearDefUseTest
        { linearDefUseTestName = "res not using newest def but using from arg",
          linearDefUseTestProg =
            Component.Prog
              "test"
              [ Component.ProgArg "arg0" OtherType,
                Component.ProgArg "arg1" ConstrainedType
              ]
              [ Component.Stmt (mrgReturn OpDef) [0] 1 [2, 3] 2 (con False),
                Component.Stmt (mrgReturn OpDef) [0] 1 [4, 5] 2 (con False),
                Component.Stmt (mrgReturn OpUse) [2, 1] 2 [6] 1 (con False)
              ]
              [Component.ProgRes (3 :: SymWordN 8) ConstrainedType],
          linearDefUseTestExpectedAssertion =
            mrgThrowError
              "Must use the newest definition of ConstrainedType" ::
              SymbolicContext ()
        }
      ]
  return $
    testCase name $
      constrainProg (LinearDefUse LinearDefUseOp) prog .@?= assertion
