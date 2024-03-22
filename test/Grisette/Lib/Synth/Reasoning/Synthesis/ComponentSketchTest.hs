{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Reasoning.Synthesis.ComponentSketchTest
  ( componentSketchTest,
  )
where

import Data.Proxy (Proxy (Proxy))
import Grisette (SymBool, SymInteger, mrgIf, mrgReturn, precise, z3)
import Grisette.Lib.Synth.Context (AngelicContext)
import Grisette.Lib.Synth.Program.ComponentSketch
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Reasoning.Fuzzing
  ( SynthesisWithFuzzerMatcherTask
      ( SynthesisWithFuzzerMatcherTask,
        synthesisWithFuzzerMatcherTaskConSemantics,
        synthesisWithFuzzerMatcherTaskContextType,
        synthesisWithFuzzerMatcherTaskGenerators,
        synthesisWithFuzzerMatcherTaskMaxTests,
        synthesisWithFuzzerMatcherTaskSolverConfig,
        synthesisWithFuzzerMatcherTaskSpec,
        synthesisWithFuzzerMatcherTaskSymProg,
        synthesisWithFuzzerMatcherTaskSymSemantics,
        synthesisWithFuzzerMatcherTaskSymValType
      ),
    fuzzingTestProg,
  )
import Grisette.Lib.Synth.Reasoning.Matcher (Matcher)
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( SynthesisResult (SynthesisSuccess),
    synthesizeProgWithVerifier,
  )
import Grisette.Lib.Synth.Reasoning.Synthesis.Problem
  ( addThenDoubleGen,
    addThenDoubleReverseSpec,
    addThenDoubleSpec,
    divModTwiceGen,
    divModTwiceSpec,
  )
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsObj (TestSemanticsObj),
    TestSemanticsOp (Add, DivMod),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Test.QuickCheck.Counterexamples (Gen)

type ConVal = Integer

type SymVal = SymInteger

type ConProg = Concrete.Prog TestSemanticsOp Integer TestSemanticsType

type SymProg = Prog TestSemanticsOp SymInteger TestSemanticsType

sharedSketch :: SymProg
sharedSketch =
  Prog
    "test"
    [ProgArg "x" IntType, ProgArg "y" IntType]
    [ Stmt
        (mrgReturn Add)
        ["stmt0'arg0", "stmt0'arg1", "stmt0'arg2"]
        "stmt0'arg_num"
        ["stmt0'ret0", "stmt0'ret1"]
        "stmt0'ret_num"
        "stmt0'dis",
      Stmt
        (mrgReturn Add)
        ["stmt1'arg0", "stmt1'arg1", "stmt1'arg2"]
        "stmt1'arg_num"
        ["stmt1'ret0"]
        "stmt1'ret_num"
        "stmt1'dis",
      Stmt
        (mrgReturn DivMod)
        ["stmt2'arg0", "stmt2'arg1"]
        "stmt2'arg_num"
        ["stmt2'ret0", "stmt2'ret1", "stmt2'ret2"]
        "stmt2'ret_num"
        "stmt2'dis",
      Stmt
        (mrgReturn DivMod)
        ["stmt3'arg0", "stmt3'arg1"]
        "stmt3'arg_num"
        ["stmt3'ret0", "stmt3'ret1"]
        "stmt3'ret_num"
        "stmt3'dis"
    ]
    [ProgRes "res0" IntType, ProgRes "res1" IntType]

sharedSketchUnion :: SymProg
sharedSketchUnion =
  Prog
    "test"
    [ProgArg "x" IntType, ProgArg "y" IntType]
    [ Stmt
        (mrgIf "stmt0'sel" (return Add) (return DivMod))
        ["stmt0'arg0", "stmt0'arg1", "stmt0'arg2"]
        "stmt0'arg_num"
        ["stmt0'ret0", "stmt0'ret1", "stmt0'ret2"]
        "stmt0'ret_num"
        "stmt0'dis",
      Stmt
        (mrgIf "stmt1'sel" (return Add) (return DivMod))
        ["stmt1'arg0", "stmt1'arg1", "stmt1'arg2"]
        "stmt1'arg_num"
        ["stmt1'ret0", "stmt1'ret1", "stmt1'ret2"]
        "stmt1'ret_num"
        "stmt1'dis"
    ]
    [ProgRes "res0" IntType, ProgRes "res1" IntType]

data ComponentSynthesisTestCase where
  ComponentSynthesisTestCase ::
    forall matcher.
    (Matcher matcher Bool Integer, Matcher matcher SymBool SymInteger) =>
    { componentSynthesisTestCaseName :: String,
      componentSynthesisTestCaseSpec :: [Integer] -> ([Integer], matcher),
      componentSynthesisTestCaseGen :: Gen [Integer]
    } ->
    ComponentSynthesisTestCase

componentSketchTest :: Test
componentSketchTest =
  testGroup "ComponentSketch" $ do
    (sketch, namePostFix) <- [(sharedSketch, ""), (sharedSketchUnion, "/union")]
    ComponentSynthesisTestCase
      name
      (spec :: [Integer] -> ([Integer], matcher))
      gen <-
      [ ComponentSynthesisTestCase
          { componentSynthesisTestCaseName = "Add then double",
            componentSynthesisTestCaseSpec = addThenDoubleSpec,
            componentSynthesisTestCaseGen = addThenDoubleGen
          },
        ComponentSynthesisTestCase
          { componentSynthesisTestCaseName = "Add then double/reverse",
            componentSynthesisTestCaseSpec = addThenDoubleReverseSpec,
            componentSynthesisTestCaseGen = addThenDoubleGen
          },
        ComponentSynthesisTestCase
          { componentSynthesisTestCaseName = "DivMod twice",
            componentSynthesisTestCaseSpec = divModTwiceSpec,
            componentSynthesisTestCaseGen = divModTwiceGen
          }
        ]
    let task :: SynthesisWithFuzzerMatcherTask ConVal ConProg matcher
        task =
          SynthesisWithFuzzerMatcherTask
            { synthesisWithFuzzerMatcherTaskContextType =
                Proxy :: Proxy AngelicContext,
              synthesisWithFuzzerMatcherTaskSymValType = Proxy :: Proxy SymVal,
              synthesisWithFuzzerMatcherTaskSolverConfig = precise z3,
              synthesisWithFuzzerMatcherTaskSpec = spec,
              synthesisWithFuzzerMatcherTaskMaxTests = 100,
              synthesisWithFuzzerMatcherTaskGenerators = [gen],
              synthesisWithFuzzerMatcherTaskConSemantics = TestSemanticsObj,
              synthesisWithFuzzerMatcherTaskSymSemantics = TestSemanticsObj,
              synthesisWithFuzzerMatcherTaskSymProg = sketch
            }
    return $ testCase (name <> namePostFix) $ do
      (_, SynthesisSuccess prog) <- synthesizeProgWithVerifier task
      fuzzingResult <- fuzzingTestProg gen spec 100 TestSemanticsObj prog
      fst <$> fuzzingResult @?= Nothing
