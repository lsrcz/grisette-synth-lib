{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Operator.OpSemanticsTest (opSemanticsTest) where

import Control.Monad.Error.Class (MonadError (throwError))
import Grisette (LogicalOp ((.||)), SymInteger, Union, mrgIf, mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgModifyError)
import Grisette.Lib.Synth.Context (SymbolicContext)
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsObj (TestSemanticsObj),
    TestSemanticsOp (Add, DivMod, Double, Inc),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

opSemanticsTest :: Test
opSemanticsTest =
  testGroup
    "Grisette.Lib.Synth.Operator.OpSemantics"
    [ testCase "Union OpSemantics" $ do
        let op =
              mrgIf "a" (return Add) $
                mrgIf "b" (return DivMod) $
                  mrgIf "c" (mrgReturn Inc) (mrgReturn Double) ::
                Union TestSemanticsOp
        let actual1 =
              mrgModifyError (const "Err") $ applyOp TestSemanticsObj op [2] ::
                SymbolicContext [SymInteger]
        let expected1 =
              mrgIf ("a" .|| "b") (throwError "Err") $
                mrgIf "c" (mrgReturn [3]) (mrgReturn [4])
        actual1 @?= expected1
        let actual2 =
              mrgModifyError (const "Err") $
                applyOp TestSemanticsObj op [5, 3] ::
                SymbolicContext [SymInteger]
        let expected2 =
              mrgIf "a" (return [8]) $
                mrgIf "b" (return [1, 2]) $
                  throwError "Err"
        actual2 @?= expected2
    ]
