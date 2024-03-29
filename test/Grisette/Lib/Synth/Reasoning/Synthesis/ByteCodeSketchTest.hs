{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Reasoning.Synthesis.ByteCodeSketchTest
  ( byteCodeSketchTest,
  )
where

import Data.Proxy (Proxy (Proxy))
import Grisette (SymBool, SymInteger, mrgIf, precise, z3)
import Grisette.Lib.Synth.Context (SymbolicContext)
import Grisette.Lib.Synth.Program.ByteCodeSketch
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.ProgConstraints
  ( WithConstraints (WithConstraints),
  )
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

type SymProg = Prog TestSemanticsOp Integer SymInteger TestSemanticsType

sharedSketch :: SymProg
sharedSketch =
  Prog
    "test"
    [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
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
    [ProgRes "res0" IntType, ProgRes "res1" IntType]

data ByteCodeSynthesisTestCase where
  ByteCodeSynthesisTestCase ::
    forall matcher.
    (Matcher matcher Bool Integer, Matcher matcher SymBool SymInteger) =>
    { byteCodeSynthesisTestCaseName :: String,
      byteCodeSynthesisTestCaseSketch :: SymProg,
      byteCodeSynthesisTestCaseSpec :: [Integer] -> ([Integer], matcher),
      byteCodeSynthesisTestCaseGen :: Gen [Integer]
    } ->
    ByteCodeSynthesisTestCase

byteCodeSketchTest :: Test
byteCodeSketchTest =
  testGroup "ByteCodeSketch" $ do
    ByteCodeSynthesisTestCase
      name
      sketch
      (spec :: [Integer] -> ([Integer], matcher))
      gen <-
      [ ByteCodeSynthesisTestCase
          { byteCodeSynthesisTestCaseName = "Add then double",
            byteCodeSynthesisTestCaseSketch = sharedSketch,
            byteCodeSynthesisTestCaseSpec = addThenDoubleSpec,
            byteCodeSynthesisTestCaseGen = addThenDoubleGen
          },
        ByteCodeSynthesisTestCase
          { byteCodeSynthesisTestCaseName = "Add then double/reverse",
            byteCodeSynthesisTestCaseSketch = sharedSketch,
            byteCodeSynthesisTestCaseSpec = addThenDoubleReverseSpec,
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
          SynthesisWithFuzzerMatcherTask ConVal ConProg matcher
        task =
          SynthesisWithFuzzerMatcherTask
            { synthesisWithFuzzerMatcherTaskContextType =
                Proxy :: Proxy SymbolicContext,
              synthesisWithFuzzerMatcherTaskSymValType = Proxy :: Proxy SymVal,
              synthesisWithFuzzerMatcherTaskSolverConfig = precise z3,
              synthesisWithFuzzerMatcherTaskSpec = spec,
              synthesisWithFuzzerMatcherTaskMaxTests = 100,
              synthesisWithFuzzerMatcherTaskGenerators = [gen],
              synthesisWithFuzzerMatcherTaskConSemantics =
                WithConstraints TestSemanticsObj (),
              synthesisWithFuzzerMatcherTaskSymSemantics =
                WithConstraints TestSemanticsObj (),
              synthesisWithFuzzerMatcherTaskSymProg = sketch
            }
    return $ testCase name $ do
      (_, SynthesisSuccess prog) <- synthesizeProgWithVerifier task
      fuzzingResult <-
        fuzzingTestProg
          gen
          spec
          100
          (WithConstraints TestSemanticsObj ())
          prog
      fst <$> fuzzingResult @?= Nothing
