{-# LANGUAGE ImportQualifiedPost #-}

module TestComponentGenOpSpec where

import Component.CEGIS
import Component.CInputGen
import Component.Circuit
import Component.ConcreteCircuit
import Component.IntermediateGen
import Component.ProgramSpec
import Component.QuickCheck
import Component.SemMap
import Data.ByteString qualified as B
import Grisette
import Sem
import Test.QuickCheck

concreteCircuit :: CCircuit (OpCode Integer) Integer
concreteCircuit =
  CCircuit
    1
    [CNode (PlusN 203) [1] [0]]
    [1]

qcspec :: PlainCSpec VerificationConditions Integer
qcspec = PlainCSpec qcspec'
  where
    qcspec' :: [Integer] -> Either VerificationConditions [Integer] -> Bool
    qcspec' [a] (Right [b]) = b == a + 203
    qcspec' _ _ = False

gen :: SimpleCGen Integer
gen = SimpleCGen $ \i -> vectorOf 1 (resize i arbitrary)

qcProblem ::
  QuickCheckProblem
    VerificationConditions
    Integer
    (OpCode Integer)
    USem
    Integer
qcProblem = QuickCheckProblem gen [200] qcspec USem concreteCircuit

spec ::
  CircuitSpec
    VerificationConditions
    (OpCode SymInteger)
    USem
spec =
  defaultCircuitSpec
    [(ComponentGenOpSpec (PlusN <$> simpleFresh ()) 1 1, 1)]
    1
    1
    USem

qcsspec :: PlainSSpec VerificationConditions SymInteger
qcsspec = PlainSSpec qcsspec'
  where
    qcsspec' :: [SymInteger] -> Either VerificationConditions [SymInteger] -> SymBool
    qcsspec' [a] (Right [b]) = b ==~ a + 203
    qcsspec' _ _ = con False

igen :: HomogeneousSGen (OpCode SymInteger) SymInteger
igen = HomogeneousSGen $ const $ simpleFresh ()

cegisQCProblem ::
  CegisQCProblem
    VerificationConditions
    SymInteger
    VerificationConditions
    Integer
    (OpCode SymInteger)
    (OpCode Integer)
    SymInteger
    Integer
cegisQCProblem =
  CegisQCProblem
    gen
    [100]
    qcspec
    USem
    qcsspec
    spec
    AssertionViolation
    AssertionViolation
    USem
    igen

testComponentGenOpSpec :: IO ()
testComponentGenOpSpec = do
  r <- quickCheckCCircuit qcProblem
  print r
  v <- cegisQC (precise z3) cegisQCProblem
  print v
