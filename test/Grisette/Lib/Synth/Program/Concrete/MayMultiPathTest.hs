{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

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
    UnionM,
    liftToMonadUnion,
    mrgIf,
  )
import Grisette.Lib.Synth.Context (MonadContext, SymbolicContext)
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Program.Concrete
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgMayMultiPath (ProgMayMultiPath),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Util.Show (showText)
import Test.Framework (Test, TestOptions' (topt_timeout), plusTestOptions)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

newtype MayAddOneOp = MayAddOneOp SymBool
  deriving (Show, Generic, Eq)
  deriving (Mergeable) via (Default MayAddOneOp)

data Sem = Sem

data IntType = IntType

mayAddOne :: SymBool -> Int -> UnionM Int
mayAddOne s x = mrgIf s (return x) (return $ x + 1)

instance
  (MonadContext ctx, MonadUnion ctx) =>
  OpSemantics Sem MayAddOneOp Int ctx
  where
  applyOp _ (MayAddOneOp s) [x] = do
    r <- liftToMonadUnion $ mayAddOne s x
    return [r]
  applyOp _ _ _ =
    error "Incorrect number of arguments for MayAddOne, expected 1 argument."

prog :: Prog MayAddOneOp Int IntType
prog =
  Prog
    "prog"
    [ProgArg "x" 0 IntType]
    ( fmap
        (\i -> Stmt (MayAddOneOp $ ssym $ "s" <> showText i) [i] [i + 1])
        [0 .. 99]
    )
    [ProgRes 100 IntType]

mayMultiPathTest :: Test
mayMultiPathTest =
  plusTestOptions (mempty {topt_timeout = Just $ Just 5000000}) $
    testCase "ProgMayMultiPath should not have path explosion" $ do
      let actual =
            runProg Sem (ProgMayMultiPath prog) [0] :: SymbolicContext [Int]
      actual @?= actual
