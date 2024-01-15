{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.ContextTest (contextTest) where

import Control.Monad.State.Class (modify)
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import Grisette
  ( GenSymSimple (simpleFresh),
    ITEOp (symIte),
    Mergeable,
    MergingStrategy (NoStrategy, SimpleStrategy),
    Solvable (isym),
    SymInteger,
    UnionLike (unionIf),
    mrgReturn,
    runFreshT,
  )
import Grisette.Core.Data.Class.Mergeable (Mergeable (rootStrategy))
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Synth.Context
  ( ConcreteContext,
    MonadContext (mergeIfNeeded, raiseError, result),
    SymbolicContext,
    sequenceC,
    sequenceC_,
    traverseC,
    traverseC_,
  )
import Test.Framework
  ( Test,
    TestOptions' (topt_timeout),
    plusTestOptions,
    testGroup,
  )
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

contextTest :: Test
contextTest =
  testGroup
    "Grisette.Lib.Synth.Context"
    [ testGroup
        "ConcreteContext"
        [ testCase "result" $
            result 1 @?= (Right 1 :: ConcreteContext Int),
          testCase "raiseError" $
            raiseError "err" @?= (Left "err" :: ConcreteContext Int),
          testCase "mergeIfNeeded" $
            mergeIfNeeded (return 1) @?= (Right 1 :: ConcreteContext Int)
        ],
      testGroup
        "SymbolicContext"
        [ testCase "result" $
            result 1 @?= (mrgReturn 1 :: SymbolicContext Int),
          testCase "raiseError" $ do
            let actual = raiseError "err" :: SymbolicContext Int
            let expected = mrgThrowError "err"
            actual @?= expected,
          testCase "mergeIfNeeded" $ do
            let actual = mergeIfNeeded (return 1)
            let expected = mrgReturn 1 :: SymbolicContext Int
            actual @?= expected
        ],
      testCase "default result" $ do
        let actual =
              flip Lazy.runStateT "st" $
                unionIf "a" (result "b") (result "c")
        let expected =
              mrgReturn (symIte "a" "b" "c", "st") ::
                SymbolicContext (SymInteger, SymInteger)
        actual @?= expected,
      testCase "default raiseError" $ do
        let actual =
              flip Lazy.runStateT "st" $
                unionIf "a" (raiseError "err") (raiseError "err")
        let expected =
              raiseError "err" ::
                SymbolicContext (SymInteger, SymInteger)
        actual @?= expected,
      testCase "mergeIfNeeded for FreshT" $ do
        let actual =
              flip runFreshT "x" $
                unionIf "a" (simpleFresh ()) (result "b") ::
                SymbolicContext SymInteger
        let expected = return (symIte "a" (isym "x" 0) "b")
        actual @?= expected,
      testCase "mergeIfNeeded for strict StateT" $ do
        let actual =
              flip Strict.runStateT "x" $
                unionIf "a" (modify (+ 1) >> result "b") (result "c") ::
                SymbolicContext (SymInteger, SymInteger)
        let expected = return (symIte "a" "b" "c", symIte "a" ("x" + 1) "x")
        actual @?= expected,
      plusTestOptions (mempty {topt_timeout = Just $ Just 1000000}) $
        testGroup
          "No path explosion if possible with C-variants"
          [ testCase "traverseC" $ do
              let val = unionIf "a" (return T1) (return T2)
              let actual = traverseC (const val) [1 .. 1000]
              let expected = mrgReturn $ replicate 1000 T1
              (actual :: SymbolicContext [T]) @?= expected,
            testCase "traverseC_" $ do
              let val = unionIf "a" (return S1) (return S2)
              let actual = traverseC_ (const val) [1 .. 1000]
              let expected = mrgReturn () :: SymbolicContext ()
              actual @?= expected,
            testCase "sequenceC" $ do
              let val = unionIf "a" (return T1) (return T2)
              let actual = sequenceC (replicate 1000 val)
              let expected = mrgReturn $ replicate 1000 T1
              (actual :: SymbolicContext [T]) @?= expected,
            testCase "sequenceC_" $ do
              let val = unionIf "a" (return S1) (return S2)
              let actual = sequenceC_ (replicate 1000 val)
              let expected = mrgReturn () :: SymbolicContext ()
              actual @?= expected
          ]
    ]

data T = T1 | T2 deriving (Eq, Show)

instance Mergeable T where
  rootStrategy = SimpleStrategy $ \_ _ _ -> T1

data S = S1 | S2 deriving (Eq, Show)

instance Mergeable S where
  rootStrategy = NoStrategy
