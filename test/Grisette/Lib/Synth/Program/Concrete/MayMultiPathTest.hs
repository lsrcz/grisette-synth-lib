{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Program.Concrete.MayMultiPathTest
  ( mayMultiPathTest,
  )
where

import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    Mergeable,
    MonadUnion,
    Solvable (ssym),
    SymBool,
    Union,
    identifier,
    liftToMonadUnion,
    mrgIf,
  )
import Grisette.Lib.Synth.Context (MonadContext, SymbolicContext)
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Operator.OpTyping (OpTyping (OpTypeType, typeOp))
import Grisette.Lib.Synth.Program.Concrete
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgMayMultiPath (ProgMayMultiPath),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.TypeSignature (TypeSignature (TypeSignature))
import Grisette.Lib.Synth.Util.Show (showText)
import Test.Framework (Test, TestOptions' (topt_timeout), plusTestOptions)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

newtype MayAddOneOp = MayAddOneOp SymBool
  deriving (Show, Generic, Eq)
  deriving (Mergeable) via (Default MayAddOneOp)

data Sem = Sem

data IntType = IntType
  deriving (Generic)
  deriving (Mergeable) via (Default IntType)

mayAddOne :: SymBool -> Int -> Union Int
mayAddOne s x = mrgIf s (return x) (return $ x + 1)

instance
  (MonadContext ctx, MonadUnion ctx) =>
  OpTyping MayAddOneOp ctx
  where
  type OpTypeType MayAddOneOp = IntType
  typeOp _ = return $ TypeSignature [IntType] [IntType]

instance
  (MonadContext ctx, MonadUnion ctx) =>
  OpSemantics Sem MayAddOneOp Int ctx
  where
  applyOp _ _ (MayAddOneOp s) [x] = do
    r <- liftToMonadUnion $ mayAddOne s x
    return [r]
  applyOp _ _ _ _ =
    error "Incorrect number of arguments for MayAddOne, expected 1 argument."

prog :: Prog MayAddOneOp Int IntType
prog =
  Prog
    "prog"
    [ProgArg "x" 0 IntType]
    ( fmap
        ( \i ->
            Stmt
              (MayAddOneOp $ ssym $ identifier $ "s" <> showText i)
              [i]
              [i + 1]
        )
        [0 .. 99]
    )
    [ProgRes 100 IntType]

mayMultiPathTest :: Test
mayMultiPathTest =
  plusTestOptions (mempty {topt_timeout = Just $ Just 5000000}) $
    testCase "ProgMayMultiPath should not have path explosion" $ do
      let actual =
            runProg Sem mempty (ProgMayMultiPath prog) [0] :: SymbolicContext [Int]
      actual @?= actual
