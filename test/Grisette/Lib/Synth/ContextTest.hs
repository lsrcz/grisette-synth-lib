{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.ContextTest (contextTest) where

import Control.Monad.State.Class (modify)
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import qualified Data.Text as T
import Grisette
  ( GenSymSimple (simpleFresh),
    ITEOp (symIte),
    Solvable (isym),
    SymInteger,
    UnionLike (unionIf),
    mrgReturn,
    runFreshT,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Synth.Context
  ( ConcreteContext,
    MonadContext (mergeIfNeeded, raiseError, result),
    SymbolicContext,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

contextTest :: Test
contextTest =
  testGroup
    "Grisette.Lib.Synth.Context"
    [ testGroup
        "ConcreteContext"
        [ testCase "result" $
            result 1 @?= (Right 1 :: ConcreteContext T.Text Int),
          testCase "raiseError" $
            raiseError "err" @?= (Left "err" :: ConcreteContext T.Text Int),
          testCase "mergeIfNeeded" $
            mergeIfNeeded (return 1) @?= (Right 1 :: ConcreteContext T.Text Int)
        ],
      testGroup
        "SymbolicContext"
        [ testCase "result" $
            result 1 @?= (mrgReturn 1 :: SymbolicContext T.Text Int),
          testCase "raiseError" $ do
            let actual = raiseError "err" :: SymbolicContext T.Text Int
            let expected = mrgThrowError "err"
            actual @?= expected,
          testCase "mergeIfNeeded" $ do
            let actual = mergeIfNeeded (return 1)
            let expected = mrgReturn 1 :: SymbolicContext T.Text Int
            actual @?= expected
        ],
      testCase "default result" $ do
        let actual =
              flip Lazy.runStateT "st" $
                unionIf "a" (result "b") (result "c")
        let expected =
              mrgReturn (symIte "a" "b" "c", "st") ::
                SymbolicContext T.Text (SymInteger, SymInteger)
        actual @?= expected,
      testCase "default raiseError" $ do
        let actual =
              flip Lazy.runStateT "st" $
                unionIf "a" (raiseError "err") (raiseError "err")
        let expected =
              raiseError "err" ::
                SymbolicContext T.Text (SymInteger, SymInteger)
        actual @?= expected,
      testCase "mergeIfNeeded for FreshT" $ do
        let actual =
              flip runFreshT "x" $
                unionIf "a" (simpleFresh ()) (result "b") ::
                SymbolicContext T.Text SymInteger
        let expected = return (symIte "a" (isym "x" 0) "b")
        actual @?= expected,
      testCase "mergeIfNeeded for strict StateT" $ do
        let actual =
              flip Strict.runStateT "x" $
                unionIf "a" (modify (+ 1) >> result "b") (result "c") ::
                SymbolicContext T.Text (SymInteger, SymInteger)
        let expected = return (symIte "a" "b" "c", symIte "a" ("x" + 1) "x")
        actual @?= expected
    ]
