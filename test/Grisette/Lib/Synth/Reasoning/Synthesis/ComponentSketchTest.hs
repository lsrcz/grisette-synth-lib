{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Reasoning.Synthesis.ComponentSketchTest
  ( componentSketchTest,
  )
where

import Grisette (SymInteger, precise, z3)
import Grisette.Lib.Synth.Context (AngelicContext)
import Grisette.Lib.Synth.Program.ComponentSketch
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Reasoning.Fuzzing
  ( SynthesisWithFuzzerTask
      ( SynthesisWithFuzzerTask,
        synthesisWithFuzzerTaskGenerators,
        synthesisWithFuzzerTaskMaxTests,
        synthesisWithFuzzerTaskSemantics,
        synthesisWithFuzzerTaskSolverConfig,
        synthesisWithFuzzerTaskSpec,
        synthesisWithFuzzerTaskSymProg
      ),
    fuzzingTestProg,
  )
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( SynthesisResult (SynthesisSuccess),
    synthesizeProgWithVerifier,
  )
import Grisette.Lib.Synth.Reasoning.Synthesis.Problem
  ( addThenDoubleGen,
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
    [ProgArg IntType "x", ProgArg IntType "y"]
    [ Stmt
        Add
        ["stmt0'arg0", "stmt0'arg1"]
        ["stmt0'ret0"]
        "stmt0'dis",
      Stmt
        Add
        ["stmt1'arg0", "stmt1'arg1"]
        ["stmt1'ret0"]
        "stmt1'dis",
      Stmt
        DivMod
        ["stmt2'arg0", "stmt2'arg1"]
        ["stmt2'ret0", "stmt2'ret1"]
        "stmt2'dis",
      Stmt
        DivMod
        ["stmt3'arg0", "stmt3'arg1"]
        ["stmt3'ret0", "stmt3'ret1"]
        "stmt3'dis"
    ]
    [ProgRes IntType "res0", ProgRes IntType "res1"]

data ComponentSynthesisTestCase = ComponentSynthesisTestCase
  { componentSynthesisTestCaseName :: String,
    componentSynthesisTestCaseSketch :: SymProg,
    componentSynthesisTestCaseSpec :: [Integer] -> [Integer],
    componentSynthesisTestCaseGen :: Gen [Integer]
  }

componentSketchTest :: Test
componentSketchTest =
  testGroup "ComponentSketch" $ do
    ComponentSynthesisTestCase name sketch spec gen <-
      [ ComponentSynthesisTestCase
          { componentSynthesisTestCaseName = "Add then double",
            componentSynthesisTestCaseSketch = sharedSketch,
            componentSynthesisTestCaseSpec = addThenDoubleSpec,
            componentSynthesisTestCaseGen = addThenDoubleGen
          },
        ComponentSynthesisTestCase
          { componentSynthesisTestCaseName = "DivMod twice",
            componentSynthesisTestCaseSketch = sharedSketch,
            componentSynthesisTestCaseSpec = divModTwiceSpec,
            componentSynthesisTestCaseGen = divModTwiceGen
          }
        ]
    let task ::
          SynthesisWithFuzzerTask
            ConVal
            SymVal
            ConProg
            SymProg
            AngelicContext
        task =
          SynthesisWithFuzzerTask
            { synthesisWithFuzzerTaskSolverConfig = precise z3,
              synthesisWithFuzzerTaskSpec = spec,
              synthesisWithFuzzerTaskMaxTests = 100,
              synthesisWithFuzzerTaskGenerators = [gen],
              synthesisWithFuzzerTaskSemantics = TestSemanticsObj,
              synthesisWithFuzzerTaskSymProg = sketch
            }
    return $ testCase name $ do
      (_, SynthesisSuccess prog) <- synthesizeProgWithVerifier task
      fuzzingResult <- fuzzingTestProg gen spec 100 TestSemanticsObj prog
      fuzzingResult @?= Nothing
