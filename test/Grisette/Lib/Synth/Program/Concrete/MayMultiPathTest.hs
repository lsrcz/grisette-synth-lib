{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Program.Concrete.MayMultiPathTest
  ( mayMultiPathTest,
  )
where

import Data.List ((\\))
import GHC.Generics (Generic)
import Grisette
  ( MonadUnion,
    Solvable (ssym),
    SymBool,
    Union,
    allClasses0,
    derive,
    identifier,
    liftToMonadUnion,
    mrgIf,
    ordClasses,
    unifiedSymOrdClasses,
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
import Grisette.Lib.Synth.Util.Show (showAsText)
import Test.Framework (Test, TestOptions' (topt_timeout), plusTestOptions)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

newtype MayAddOneOp = MayAddOneOp SymBool deriving (Generic)

derive [''MayAddOneOp] (allClasses0 \\ (ordClasses ++ unifiedSymOrdClasses))

data Sem = Sem

data IntType = IntType deriving (Generic)

derive [''IntType] allClasses0

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
    [ProgArg "x" 0 IntType]
    ( fmap
        ( \i ->
            Stmt
              (MayAddOneOp $ ssym $ identifier $ "s" <> showAsText i)
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
