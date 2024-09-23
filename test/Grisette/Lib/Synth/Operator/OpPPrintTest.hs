{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Operator.OpPPrintTest (opPPrintTest) where

import Control.Arrow (Arrow (second))
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Grisette.Lib.Synth.Program.Concrete
  ( OpPPrintError
      ( IncorrectNumberOfArguments,
        IncorrectNumberOfResults,
        RedefinedResult,
        UndefinedArgument
      ),
    VarIdMap,
    prettyArguments,
    prettyResults,
  )
import Grisette.Lib.Synth.Program.ProgTyping (ProgTypeTable)
import Grisette.Lib.Synth.TestOperator.TestPrettyOperator
  ( TestPrettyOp (PrettyOp0, PrettyOp1, PrettyOp2),
    TestPrettyType,
  )
import Grisette.Lib.Synth.Util.Pretty (renderDoc)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

env :: VarIdMap Int
env = HM.fromList [(0, "x"), (1, "y")]

looselyRenderArguments ::
  ProgTypeTable TestPrettyType ->
  TestPrettyOp ->
  [Int] ->
  Either (OpPPrintError Int TestPrettyOp) T.Text
looselyRenderArguments table op args =
  renderDoc 80 <$> prettyArguments table op args env

compactlyRenderArguments ::
  ProgTypeTable TestPrettyType ->
  TestPrettyOp ->
  [Int] ->
  Either (OpPPrintError Int TestPrettyOp) T.Text
compactlyRenderArguments table op args =
  renderDoc 1 <$> prettyArguments table op args env

looselyRenderResults ::
  ProgTypeTable TestPrettyType ->
  TestPrettyOp ->
  [Int] ->
  Either (OpPPrintError Int TestPrettyOp) (VarIdMap Int, T.Text)
looselyRenderResults table op res =
  second (renderDoc 80) <$> prettyResults table op res env

compactlyRenderResults ::
  ProgTypeTable TestPrettyType ->
  TestPrettyOp ->
  [Int] ->
  Either (OpPPrintError Int TestPrettyOp) (VarIdMap Int, T.Text)
compactlyRenderResults table op res =
  second (renderDoc 1) <$> prettyResults table op res env

opPPrintTest :: Test
opPPrintTest =
  testGroup
    "Grisette.Lib.Synth.Operator.OpPPrint"
    [ testGroup
        "prettyArguments"
        [ testGroup
            "Errors"
            [ testCase "IncorrectNumofArguments" $ do
                let actual = looselyRenderArguments mempty PrettyOp1 [0, 1]
                let expected = Left (IncorrectNumberOfArguments PrettyOp1 1 2)
                actual @?= expected,
              testCase "UndefinedArgument" $ do
                let actual = looselyRenderArguments mempty PrettyOp2 [1, 2]
                let expected = Left (UndefinedArgument 1 2)
                actual @?= expected
            ],
          testGroup
            "Success"
            [ testGroup
                "0 arguments"
                [ testCase "loose" $
                    looselyRenderArguments mempty PrettyOp0 [] @?= Right "()",
                  testCase "compact" $
                    compactlyRenderArguments mempty PrettyOp0 [] @?= Right "()"
                ],
              testGroup
                "1 arguments"
                [ testCase "loose" $
                    looselyRenderArguments mempty PrettyOp1 [0]
                      @?= Right "(op1=x)",
                  testCase "compact" $
                    compactlyRenderArguments mempty PrettyOp1 [0]
                      @?= Right "(\n  op1=x\n)"
                ],
              testGroup
                "2 arguments"
                [ testCase "loose" $ do
                    let actual = looselyRenderArguments mempty PrettyOp2 [0, 1]
                    let expected = Right "(op2'2'0'arg=x, y)"
                    actual @?= expected,
                  testCase "compact" $ do
                    let actual = 
                         compactlyRenderArguments mempty PrettyOp2 [0, 1]
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
                let actual = looselyRenderResults mempty PrettyOp1 [2, 3]
                let expected = Left (IncorrectNumberOfResults PrettyOp1 1 2)
                actual @?= expected,
              testCase "RedefinedResult" $ do
                let actual = looselyRenderResults mempty PrettyOp2 [0, 2]
                let expected = Left (RedefinedResult 0 0)
                actual @?= expected
            ],
          testGroup
            "Success"
            [ testGroup
                "1 results"
                [ testCase "loose" $ do
                    let actual = looselyRenderResults mempty PrettyOp1 [3]
                    let name = "t1_3"
                    let expected = Right (HM.insert 3 name env, name)
                    actual @?= expected,
                  testCase "compact" $ do
                    let actual = compactlyRenderResults mempty PrettyOp1 [3]
                    let name = "t1_3"
                    let expected = Right (HM.insert 3 name env, name)
                    actual @?= expected
                ],
              testGroup "2 results" $ do
                let name2 = "op2_2"
                let name3 = "op2'_3"
                let newEnv = HM.insert 2 name2 $ HM.insert 3 name3 env
                [ testCase "loose" $ do
                    let actual = looselyRenderResults mempty PrettyOp2 [2, 3]
                    let expected =
                          Right (newEnv, "(" <> name2 <> ", " <> name3 <> ")")
                    actual @?= expected,
                  testCase "compact" $ do
                    let actual = compactlyRenderResults mempty PrettyOp2 [2, 3]
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
