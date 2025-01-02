{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.Concrete.GenSymSimpleTest
  ( genSymSimpleTest,
  )
where

import Grisette (Solvable (isym), Union, genSymSimple, mrgIf)
import Grisette.Lib.Synth.Program.Choice.ChoiceTree (ChoiceTree (Leaf))
import Grisette.Lib.Synth.Program.Concrete (Prog, buildProg, node)
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add, DivMod, Double, Inc),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

prog ::
  ChoiceTree TestSemanticsOp ->
  ChoiceTree TestSemanticsOp ->
  Prog (ChoiceTree TestSemanticsOp) Integer TestSemanticsType
prog op1 op2 = buildProg [("x", IntType), ("y", IntType)] $ \[x, y] ->
  let [r1] = node op1 1 [x, y]
      [r2] = node op2 1 [x, y]
   in [(r1, IntType), (r2, IntType)]

genSymSimpleTest :: Test
genSymSimpleTest =
  testCase "GenSymSimple" $ do
    let p = prog (Leaf [Add, DivMod]) (Leaf [Inc, Double])
    let p' =
          genSymSimple p "prog" ::
            Prog (Union TestSemanticsOp) Integer TestSemanticsType
    let expected = buildProg [("x", IntType), ("y", IntType)] $ \[x, y] ->
          let [r1] =
                node
                  (mrgIf (isym "prog" 0) (return Add) (return DivMod))
                  1
                  [x, y]
              [r2] =
                node
                  (mrgIf (isym "prog" 1) (return Inc) (return Double))
                  1
                  [x, y]
           in [(r1, IntType), (r2, IntType)]
    p' @?= expected
