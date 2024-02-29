{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Operator.OpPrettyTest (opPrettyTest) where

import Control.Arrow (Arrow (second))
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Grisette.Lib.Synth.Program.Concrete
  ( OpPrettyError
      ( IncorrectNumberOfArguments,
        IncorrectNumberOfResults,
        RedefinedResult,
        UndefinedArgument
      ),
    VarIdMap,
    prettyArguments,
    prettyResults,
  )
import Grisette.Lib.Synth.TestOperator.TestPrettyOperator
  ( TestPrettyOp (PrettyOp1, PrettyOp2),
  )
import Grisette.Lib.Synth.Util.Pretty (renderDoc)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

env :: VarIdMap Int
env = HM.fromList [(0, "x"), (1, "y")]

looselyRenderArguments ::
  TestPrettyOp -> [Int] -> Either (OpPrettyError TestPrettyOp Int) T.Text
looselyRenderArguments op args =
  renderDoc 80 <$> prettyArguments op args env

compactlyRenderArguments ::
  TestPrettyOp -> [Int] -> Either (OpPrettyError TestPrettyOp Int) T.Text
compactlyRenderArguments op args =
  renderDoc 1 <$> prettyArguments op args env

looselyRenderResults ::
  TestPrettyOp ->
  Int ->
  [Int] ->
  Either (OpPrettyError TestPrettyOp Int) (VarIdMap Int, T.Text)
looselyRenderResults op numArgs res =
  second (renderDoc 80) <$> prettyResults op numArgs res env

compactlyRenderResults ::
  TestPrettyOp ->
  Int ->
  [Int] ->
  Either (OpPrettyError TestPrettyOp Int) (VarIdMap Int, T.Text)
compactlyRenderResults op numArgs res =
  second (renderDoc 1) <$> prettyResults op numArgs res env

opPrettyTest :: Test
opPrettyTest =
  testGroup
    "Grisette.Lib.Synth.Operator.OpPretty"
    [ testGroup
        "prettyArguments"
        [ testGroup
            "Errors"
            [ testCase "IncorrectNumofArguments" $ do
                let actual = looselyRenderArguments PrettyOp1 [0, 1]
                let expected = Left (IncorrectNumberOfArguments PrettyOp1 2)
                actual @?= expected,
              testCase "UndefinedArgument" $ do
                let actual = looselyRenderArguments PrettyOp2 [1, 2]
                let expected = Left (UndefinedArgument 1 2)
                actual @?= expected
            ],
          testGroup
            "Success"
            [ testGroup
                "0 arguments"
                [ testCase "loose" $
                    looselyRenderArguments PrettyOp2 [] @?= Right "()",
                  testCase "compact" $
                    compactlyRenderArguments PrettyOp2 [] @?= Right "()"
                ],
              testGroup
                "1 arguments"
                [ testCase "loose" $
                    looselyRenderArguments PrettyOp2 [0] @?= Right "(x)",
                  testCase "compact" $
                    compactlyRenderArguments PrettyOp2 [0] @?= Right "(\n  x\n)"
                ],
              testGroup
                "2 arguments"
                [ testCase "loose" $ do
                    let actual = looselyRenderArguments PrettyOp2 [0, 1]
                    let expected = Right "(op2'2'0'arg=x, y)"
                    actual @?= expected,
                  testCase "compact" $ do
                    let actual = compactlyRenderArguments PrettyOp2 [0, 1]
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
                let actual = looselyRenderResults PrettyOp1 1 [2, 3]
                let expected = Left (IncorrectNumberOfResults PrettyOp1 1 2)
                actual @?= expected,
              testCase "RedefinedResult" $ do
                let actual = looselyRenderResults PrettyOp2 2 [0, 2]
                let expected = Left (RedefinedResult 0 0)
                actual @?= expected
            ],
          testGroup
            "Success"
            [ testGroup
                "0 results"
                [ testCase "loose" $
                    looselyRenderResults PrettyOp2 0 [] @?= Right (env, "()"),
                  testCase "compact" $
                    compactlyRenderResults PrettyOp2 0 [] @?= Right (env, "()")
                ],
              testGroup
                "1 results"
                [ testCase "loose" $ do
                    let actual = looselyRenderResults PrettyOp2 1 [3]
                    let name = "op2'1'res3"
                    let expected = Right (HM.insert 3 name env, name)
                    actual @?= expected,
                  testCase "compact" $ do
                    let actual = compactlyRenderResults PrettyOp2 1 [3]
                    let name = "op2'1'res3"
                    let expected = Right (HM.insert 3 name env, name)
                    actual @?= expected
                ],
              testGroup "2 results" $ do
                let name2 = "op2'2'0'res2"
                let name3 = "op2'2'1'res3"
                let newEnv = HM.insert 2 name2 $ HM.insert 3 name3 env
                [ testCase "loose" $ do
                    let actual = looselyRenderResults PrettyOp2 2 [2, 3]
                    let expected =
                          Right (newEnv, "(" <> name2 <> ", " <> name3 <> ")")
                    actual @?= expected,
                  testCase "compact" $ do
                    let actual = compactlyRenderResults PrettyOp2 2 [2, 3]
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
