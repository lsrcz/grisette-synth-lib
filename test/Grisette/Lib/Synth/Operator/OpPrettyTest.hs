{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Operator.OpPrettyTest (opPrettyTest) where

import Control.Arrow (Arrow (second))
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette (Default (Default), Mergeable)
import Grisette.Lib.Synth.Operator.OpPretty
  ( OpPretty (describeArguments, prefixResults),
    OpPrettyError
      ( IncorrectNumberOfArguments,
        IncorrectNumberOfResults,
        RedefinedResult,
        UndefinedArgument
      ),
    VarIdMap,
    prettyArguments,
    prettyResults,
  )
import Grisette.Lib.Synth.Util.Pretty (renderDoc)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

data TestSem = Sem

data TestOp = Op1 | Op2
  deriving (Show, Generic, Eq)
  deriving (Mergeable) via (Default TestOp)

instance OpPretty TestSem TestOp where
  describeArguments _ Op1 1 = Right [Just "op1"]
  describeArguments _ Op1 n = Left $ IncorrectNumberOfArguments Op1 n
  describeArguments _ Op2 0 = Right []
  describeArguments _ Op2 1 = Right [Nothing]
  describeArguments _ Op2 2 = Right [Just "op2'2'0'arg", Nothing]
  describeArguments _ Op2 n = Left $ IncorrectNumberOfArguments Op2 n
  prefixResults _ Op1 1 1 = Right ["op1res"]
  prefixResults _ Op1 numArguments numResults =
    Left $ IncorrectNumberOfResults Op1 numArguments numResults
  prefixResults _ Op2 0 0 = Right []
  prefixResults _ Op2 1 1 = Right ["op2'1'res"]
  prefixResults _ Op2 2 2 = Right ["op2'2'0'res", "op2'2'1'res"]
  prefixResults _ Op2 numArguments numResults =
    Left $ IncorrectNumberOfResults Op2 numArguments numResults

env :: VarIdMap Int
env = HM.fromList [(0, "x"), (1, "y")]

looselyRenderArguments ::
  TestOp -> [Int] -> Either (OpPrettyError TestOp Int) T.Text
looselyRenderArguments op args =
  renderDoc 80 <$> prettyArguments Sem op args env

compactlyRenderArguments ::
  TestOp -> [Int] -> Either (OpPrettyError TestOp Int) T.Text
compactlyRenderArguments op args =
  renderDoc 1 <$> prettyArguments Sem op args env

looselyRenderResults ::
  TestOp ->
  Int ->
  [Int] ->
  Either (OpPrettyError TestOp Int) (VarIdMap Int, T.Text)
looselyRenderResults op numArgs res =
  second (renderDoc 80) <$> prettyResults Sem op numArgs res env

compactlyRenderResults ::
  TestOp ->
  Int ->
  [Int] ->
  Either (OpPrettyError TestOp Int) (VarIdMap Int, T.Text)
compactlyRenderResults op numArgs res =
  second (renderDoc 1) <$> prettyResults Sem op numArgs res env

opPrettyTest :: Test
opPrettyTest =
  testGroup
    "Grisette.Lib.Synth.Operator.OpPretty"
    [ testGroup
        "prettyArguments"
        [ testGroup
            "Errors"
            [ testCase "IncorrectNumofArguments" $ do
                let actual = looselyRenderArguments Op1 [0, 1]
                let expected = Left (IncorrectNumberOfArguments Op1 2)
                actual @?= expected,
              testCase "UndefinedArgument" $ do
                let actual = looselyRenderArguments Op2 [1, 2]
                let expected = Left (UndefinedArgument 1 2)
                actual @?= expected
            ],
          testGroup
            "Success"
            [ testGroup
                "0 arguments"
                [ testCase "loose" $
                    looselyRenderArguments Op2 [] @?= Right "()",
                  testCase "compact" $
                    compactlyRenderArguments Op2 [] @?= Right "()"
                ],
              testGroup
                "1 arguments"
                [ testCase "loose" $
                    looselyRenderArguments Op2 [0] @?= Right "(x)",
                  testCase "compact" $
                    compactlyRenderArguments Op2 [0] @?= Right "(\n  x\n)"
                ],
              testGroup
                "2 arguments"
                [ testCase "loose" $ do
                    let actual = looselyRenderArguments Op2 [0, 1]
                    let expected = Right "(op2'2'0'arg=x, y)"
                    actual @?= expected,
                  testCase "compact" $ do
                    let actual = compactlyRenderArguments Op2 [0, 1]
                    let expected = Right "(\n  op2'2'0'arg=x,\n  y\n)"
                    actual @?= expected
                ]
            ]
        ],
      testGroup
        "prettyResults"
        [ testGroup
            "Errors"
            [ testCase "IncorrectNumofResults" $ do
                let actual = looselyRenderResults Op1 1 [2, 3]
                let expected = Left (IncorrectNumberOfResults Op1 1 2)
                actual @?= expected,
              testCase "RedefinedResult" $ do
                let actual = looselyRenderResults Op2 2 [0, 2]
                let expected = Left (RedefinedResult 0 0)
                actual @?= expected
            ],
          testGroup
            "Success"
            [ testGroup
                "0 results"
                [ testCase "loose" $
                    looselyRenderResults Op2 0 [] @?= Right (env, "()"),
                  testCase "compact" $
                    compactlyRenderResults Op2 0 [] @?= Right (env, "()")
                ],
              testGroup
                "1 results"
                [ testCase "loose" $ do
                    let actual = looselyRenderResults Op2 1 [3]
                    let name = "op2'1'res3"
                    let expected = Right (HM.insert 3 name env, name)
                    actual @?= expected,
                  testCase "compact" $ do
                    let actual = compactlyRenderResults Op2 1 [3]
                    let name = "op2'1'res3"
                    let expected = Right (HM.insert 3 name env, name)
                    actual @?= expected
                ],
              testGroup "2 results" $ do
                let name2 = "op2'2'0'res2"
                let name3 = "op2'2'1'res3"
                let newEnv = HM.insert 2 name2 $ HM.insert 3 name3 env
                [ testCase "loose" $ do
                    let actual = looselyRenderResults Op2 2 [2, 3]
                    let expected =
                          Right (newEnv, "(" <> name2 <> ", " <> name3 <> ")")
                    actual @?= expected,
                  testCase "compact" $ do
                    let actual = compactlyRenderResults Op2 2 [2, 3]
                    let expected =
                          Right
                            ( newEnv,
                              "(\n  " <> name2 <> ",\n  " <> name3 <> "\n)"
                            )
                    actual @?= expected
                  ]
            ]
        ]
    ]
