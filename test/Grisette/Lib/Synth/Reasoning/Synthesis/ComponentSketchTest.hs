{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Reasoning.Synthesis.ComponentSketchTest
  ( ConProg,
    SymProg,
    componentSketchTest,
    ComponentSynthesisTestCase (..),
    task,
    sharedSketch,
    fuzzResult,
    times4Sketch,
  )
where

import Data.Data (Typeable)
import Grisette
  ( SolvingFailure (Unsat),
    SymBool,
    SymInteger,
    UnionM,
    mrgIf,
    mrgReturn,
    precise,
    z3,
  )
import Grisette.Lib.Synth.Context (AngelicContext, ConcreteContext)
import Grisette.Lib.Synth.Program.BuiltinProgConstraints.ComponentSymmetryReduction
  ( ComponentSymmetryReduction (ComponentSymmetryReduction),
  )
import Grisette.Lib.Synth.Program.ComponentSketch
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.Concrete.Flatten (flattenProg)
import Grisette.Lib.Synth.Program.CostModel.PerStmtCostModel
  ( PerStmtCostObj (PerStmtCostObj),
  )
import Grisette.Lib.Synth.Program.ProgConstraints
  ( WithConstraints (WithConstraints),
  )
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics)
import Grisette.Lib.Synth.Reasoning.Fuzzing
  ( QuickCheckFuzzer
      ( QuickCheckFuzzer,
        quickCheckFuzzerConSemantics,
        quickCheckFuzzerGenerators,
        quickCheckFuzzerMaxTests,
        quickCheckFuzzerSpec,
        quickCheckFuzzerSymSemantics
      ),
    fuzzingTestProg,
  )
import Grisette.Lib.Synth.Reasoning.Matcher (Matcher)
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( SomeVerifier (SomeVerifier),
    SynthesisMinimalCostTask
      ( SynthesisMinimalCostTask,
        synthesisMinimalCostTaskConCostObj,
        synthesisMinimalCostTaskInitialMaxCost,
        synthesisMinimalCostTaskSymCostObj,
        synthesisMinimalCostTaskSymProg,
        synthesisMinimalCostTaskVerifiers
      ),
    SynthesisResult (SynthesisSolverFailure, SynthesisSuccess),
    SynthesisTask
      ( SynthesisTask,
        synthesisSketch,
        synthesisVerifiers
      ),
    runSynthesisMinimalCostTask,
    runSynthesisTask,
  )
import Grisette.Lib.Synth.Reasoning.Synthesis.Problem
  ( addThenDivModGen,
    addThenDivModSpec,
    addThenDoubleGen,
    addThenDoubleReverseSpec,
    addThenDoubleSpec,
    divModTwiceGen,
    divModTwiceSpec,
    times4Gen,
    times4Spec,
  )
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsCost (TestSemanticsCost),
    TestSemanticsObj (TestSemanticsObj),
    TestSemanticsOp (Add, DivMod, Double),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Test.QuickCheck.Counterexamples (Gen)

type ConVal = Integer

type SymVal = SymInteger

type ConProg = Concrete.Prog TestSemanticsOp Integer TestSemanticsType

type SymProg = Prog (UnionM TestSemanticsOp) SymInteger TestSemanticsType

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
        "stmt0'dis"
        [],
      Stmt
        (mrgReturn Add)
        ["stmt1'arg0", "stmt1'arg1", "stmt1'arg2"]
        "stmt1'arg_num"
        ["stmt1'ret0"]
        "stmt1'ret_num"
        "stmt1'dis"
        [],
      Stmt
        (mrgReturn DivMod)
        ["stmt2'arg0", "stmt2'arg1"]
        "stmt2'arg_num"
        ["stmt2'ret0", "stmt2'ret1", "stmt2'ret2"]
        "stmt2'ret_num"
        "stmt2'dis"
        [],
      Stmt
        (mrgReturn DivMod)
        ["stmt3'arg0", "stmt3'arg1"]
        "stmt3'arg_num"
        ["stmt3'ret0", "stmt3'ret1"]
        "stmt3'ret_num"
        "stmt3'dis"
        []
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
        "stmt0'dis"
        [],
      Stmt
        (mrgIf "stmt1'sel" (return Add) (return DivMod))
        ["stmt1'arg0", "stmt1'arg1", "stmt1'arg2"]
        "stmt1'arg_num"
        ["stmt1'ret0", "stmt1'ret1", "stmt1'ret2"]
        "stmt1'ret_num"
        "stmt1'dis"
        []
    ]
    [ProgRes "res0" IntType, ProgRes "res1" IntType]

addThenDivModSketch :: SymProg
addThenDivModSketch =
  Prog
    "test"
    [ProgArg "x" IntType, ProgArg "y" IntType, ProgArg "z" IntType]
    [ Stmt
        (mrgReturn Add)
        ["stmt0'arg0", "stmt0'arg1", "stmt0'arg2"]
        "stmt0'arg_num"
        ["stmt0'ret0", "stmt0'ret1", "stmt0'ret2"]
        "stmt0'ret_num"
        "stmt0'dis"
        [],
      Stmt
        (mrgReturn DivMod)
        ["stmt1'arg0", "stmt1'arg1", "stmt1'arg2"]
        "stmt1'arg_num"
        ["stmt1'ret0", "stmt1'ret1", "stmt1'ret2"]
        "stmt1'ret_num"
        "stmt1'dis"
        ["stmt0'ret0", "stmt0'ret1", "stmt0'ret2"]
    ]
    [ProgRes "res0" IntType, ProgRes "res1" IntType]

addThenDivModSketchBadMustBeAfter :: SymProg
addThenDivModSketchBadMustBeAfter =
  Prog
    "test"
    [ProgArg "x" IntType, ProgArg "y" IntType, ProgArg "z" IntType]
    [ Stmt
        (return Add)
        ["stmt0'arg0", "stmt0'arg1", "stmt0'arg2"]
        "stmt0'arg_num"
        ["stmt0'ret0", "stmt0'ret1", "stmt0'ret2"]
        "stmt0'ret_num"
        "stmt0'dis"
        ["stmt1'ret0", "stmt1'ret1", "stmt1'ret2"],
      Stmt
        (return DivMod)
        ["stmt1'arg0", "stmt1'arg1", "stmt1'arg2"]
        "stmt1'arg_num"
        ["stmt1'ret0", "stmt1'ret1", "stmt1'ret2"]
        "stmt1'ret_num"
        "stmt1'dis"
        []
    ]
    [ProgRes "res0" IntType, ProgRes "res1" IntType]

data ComponentSynthesisTestCase where
  ComponentSynthesisTestCase ::
    forall matcher.
    ( Matcher matcher Bool Integer,
      Matcher matcher SymBool SymInteger,
      Typeable matcher
    ) =>
    { componentSynthesisTestCaseName :: String,
      componentSynthesisTestCaseSpec :: [Integer] -> ([Integer], matcher),
      componentSynthesisTestCaseGen :: Gen [Integer]
    } ->
    ComponentSynthesisTestCase

fuzzResult ::
  ( Matcher b Bool conVal,
    ProgSemantics TestSemanticsObj conProg conVal ConcreteContext,
    Eq conVal,
    Show conVal,
    Show conProg
  ) =>
  SynthesisResult conProg ->
  Gen [conVal] ->
  ([conVal] -> ([conVal], b)) ->
  IO ()
fuzzResult (SynthesisSuccess prog) gen spec = do
  fuzzingResult <-
    fuzzingTestProg
      gen
      spec
      100
      TestSemanticsObj
      prog
  fst <$> fuzzingResult @?= Nothing
fuzzResult r _ _ = fail $ "Unexpected result: " <> show r

verifier ::
  (Matcher matcher SymBool SymVal, Matcher matcher Bool ConVal, Typeable matcher) =>
  ([ConVal] -> ([ConVal], matcher)) ->
  Gen [ConVal] ->
  QuickCheckFuzzer SymVal ConVal SymProg ConProg AngelicContext
verifier spec gen =
  QuickCheckFuzzer
    { quickCheckFuzzerSymSemantics =
        WithConstraints TestSemanticsObj (ComponentSymmetryReduction ()),
      quickCheckFuzzerConSemantics = TestSemanticsObj,
      quickCheckFuzzerMaxTests = 100,
      quickCheckFuzzerGenerators = [gen],
      quickCheckFuzzerSpec = spec
    }

task ::
  ( Matcher matcher SymBool SymVal,
    Matcher matcher Bool ConVal,
    Typeable matcher
  ) =>
  ([ConVal] -> ([ConVal], matcher)) ->
  Gen [ConVal] ->
  SymProg ->
  SynthesisTask SymProg ConProg
task spec gen sketch =
  SynthesisTask
    { synthesisVerifiers = [SomeVerifier $ verifier spec gen],
      synthesisSketch = sketch
    }

componentSketchTest :: Test
componentSketchTest =
  testGroup
    "ComponentSketch"
    [ testGroup "general" $
        ( do
            (sketch, namePostFix) <-
              [ (sharedSketch, ""),
                (sharedSketchUnion, "/union")
                ]
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

            return $ testCase (name <> namePostFix) $ do
              result <- runSynthesisTask (precise z3) $ task spec gen sketch
              fuzzResult result gen spec
        )
          ++ [ testCase "Add then DivMod with must be after constraint" $ do
                 SynthesisSuccess prog <-
                   runSynthesisTask (precise z3) $
                     task addThenDivModSpec addThenDivModGen addThenDivModSketch
                 fuzzingResult <-
                   fuzzingTestProg
                     addThenDivModGen
                     addThenDivModSpec
                     100
                     TestSemanticsObj
                     prog
                 fst <$> fuzzingResult @?= Nothing,
               testCase "Add then DivMod with bad must be after constraint" $ do
                 SynthesisSolverFailure Unsat <-
                   runSynthesisTask (precise z3) $
                     task
                       addThenDivModSpec
                       addThenDivModGen
                       addThenDivModSketchBadMustBeAfter
                 return ()
             ],
      testGroup
        "With cost model"
        [ testCase "refinement" $ do
            let task =
                  SynthesisMinimalCostTask
                    { synthesisMinimalCostTaskVerifiers =
                        [SomeVerifier $ verifier times4Spec times4Gen],
                      synthesisMinimalCostTaskSymProg = times4Sketch,
                      synthesisMinimalCostTaskInitialMaxCost =
                        Nothing :: Maybe SymInteger,
                      synthesisMinimalCostTaskConCostObj =
                        PerStmtCostObj TestSemanticsCost,
                      synthesisMinimalCostTaskSymCostObj =
                        PerStmtCostObj TestSemanticsCost
                    }
            let expectedSynthesizedProg =
                  Concrete.Prog
                    "test"
                    [Concrete.ProgArg "x" (0 :: Int) IntType]
                    [ Concrete.Stmt Double [0] [1],
                      Concrete.Stmt Double [1] [2]
                    ]
                    [Concrete.ProgRes 2 IntType]
            result <- runSynthesisMinimalCostTask (precise z3) task
            case result of
              SynthesisSuccess prog ->
                flattenProg prog @?= Right expectedSynthesizedProg
              _ -> fail "Unexpected result",
          testCase "initial" $ do
            let task =
                  SynthesisMinimalCostTask
                    { synthesisMinimalCostTaskVerifiers =
                        [SomeVerifier $ verifier times4Spec times4Gen],
                      synthesisMinimalCostTaskSymProg = times4Sketch,
                      synthesisMinimalCostTaskInitialMaxCost =
                        Just 2 :: Maybe SymInteger,
                      synthesisMinimalCostTaskConCostObj =
                        PerStmtCostObj TestSemanticsCost,
                      synthesisMinimalCostTaskSymCostObj =
                        PerStmtCostObj TestSemanticsCost
                    }
            result <- runSynthesisMinimalCostTask (precise z3) task
            case result of
              SynthesisSolverFailure Unsat -> return ()
              _ -> fail "Unexpected result"
        ]
    ]

times4Sketch :: SymProg
times4Sketch =
  Prog
    "test"
    [ProgArg "x" IntType]
    [ Stmt
        (mrgReturn Add)
        ["stmt0'arg0", "stmt0'arg1", "stmt0'arg2"]
        "stmt0'arg_num"
        ["stmt0'ret0", "stmt0'ret1"]
        "stmt0'ret_num"
        "stmt0'dis"
        [],
      Stmt
        (mrgReturn Add)
        ["stmt1'arg0", "stmt1'arg1", "stmt1'arg2"]
        "stmt1'arg_num"
        ["stmt1'ret0"]
        "stmt1'ret_num"
        "stmt1'dis"
        [],
      Stmt
        (mrgReturn Double)
        ["stmt2'arg0", "stmt2'arg1"]
        "stmt2'arg_num"
        ["stmt2'ret0", "stmt2'ret1", "stmt2'ret2"]
        "stmt2'ret_num"
        "stmt2'dis"
        [],
      Stmt
        (mrgReturn Double)
        ["stmt3'arg0", "stmt3'arg1"]
        "stmt3'arg_num"
        ["stmt3'ret0", "stmt3'ret1"]
        "stmt3'ret_num"
        "stmt3'dis"
        []
    ]
    [ProgRes "res0" IntType]
