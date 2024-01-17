{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Reasoning.Synthesis.ByteCodeSketchTest
  ( byteCodeSketchTest,
  )
where

import Grisette (SymInteger, mrgIf, precise, z3)
import Grisette.Lib.Synth.Context (SymbolicContext)
import Grisette.Lib.Synth.Program.ByteCodeSketch
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

type SymProg = Prog TestSemanticsOp Integer SymInteger TestSemanticsType

sharedSketch :: SymProg
sharedSketch =
  Prog
    "test"
    [ProgArg IntType "x" 0, ProgArg IntType "y" 1]
    [ Stmt
        (mrgIf "stmt0'op" (return Add) (return DivMod))
        ["stmt0'arg1", "stmt0'arg2"]
        "stmt0'numArg"
        [2, 3]
        "stmt0'numRes",
      Stmt
        (mrgIf "stmt1'op" (return Add) (return DivMod))
        ["stmt1'arg1", "stmt1'arg2"]
        "stmt1'numArg"
        [4, 5]
        "stmt1'numRes"
    ]
    [ProgRes IntType "res0", ProgRes IntType "res1"]

data ByteCodeSynthesisTestCase = ByteCodeSynthesisTestCase
  { byteCodeSynthesisTestCaseName :: String,
    byteCodeSynthesisTestCaseSketch :: SymProg,
    byteCodeSynthesisTestCaseSpec :: [Integer] -> [Integer],
    byteCodeSynthesisTestCaseGen :: Gen [Integer]
  }

byteCodeSketchTest :: Test
byteCodeSketchTest =
  testGroup "ByteCodeSketch" $ do
    ByteCodeSynthesisTestCase name sketch spec gen <-
      [ ByteCodeSynthesisTestCase
          { byteCodeSynthesisTestCaseName = "Add then double",
            byteCodeSynthesisTestCaseSketch = sharedSketch,
            byteCodeSynthesisTestCaseSpec = addThenDoubleSpec,
            byteCodeSynthesisTestCaseGen = addThenDoubleGen
          },
        ByteCodeSynthesisTestCase
          { byteCodeSynthesisTestCaseName = "DivMod twice",
            byteCodeSynthesisTestCaseSketch = sharedSketch,
            byteCodeSynthesisTestCaseSpec = divModTwiceSpec,
            byteCodeSynthesisTestCaseGen = divModTwiceGen
          }
        ]
    let task ::
          SynthesisWithFuzzerTask
            ConVal
            SymVal
            ConProg
            SymProg
            SymbolicContext
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
