{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.ComponentSketch.ToSymTest (toSymTest) where

import Grisette (Solvable (con), SymWordN, ToSym (toSym), mrgReturn)
import Grisette.Lib.Synth.Program.ComponentSketch
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add, DivMod),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

toSymTest :: Test
toSymTest =
  testGroup
    "ToSym"
    [ testCase "from concrete program" $ do
        let prog =
              Concrete.Prog
                "test"
                [ Concrete.ProgArg "x" 100 IntType,
                  Concrete.ProgArg "y" (-1) IntType
                ]
                [ Concrete.Stmt Add [100, -1] [10],
                  Concrete.Stmt DivMod [10, 100] [0, 2]
                ]
                [Concrete.ProgRes 0 IntType, Concrete.ProgRes 2 IntType] ::
                Concrete.Prog TestSemanticsOp Int TestSemanticsType
        let expected =
              Prog
                "test"
                [ProgArg "x" IntType, ProgArg "y" IntType]
                [ Stmt (mrgReturn Add) [0, 1] 2 [2] 1 (con False),
                  Stmt (mrgReturn DivMod) [2, 0] 2 [3, 4] 2 (con False)
                ]
                [ProgRes 3 IntType, ProgRes 4 IntType] ::
                Prog TestSemanticsOp (SymWordN 8) TestSemanticsType
        toSym prog @?= expected
    ]
