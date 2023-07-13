{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module TestMultipleOutputs (testMultipleOutputs) where

import Component.CEGIS
import Component.CInputGen
import Component.Circuit
import Component.ConcreteCircuit
import Component.IntermediateGen
import Component.Monad
import Component.ProgramSpec
import Component.QuickCheck
import Component.SemMap
import Data.ByteString qualified as B
import Data.HashMap.Strict qualified as M
import Grisette
import Sem
import Test.QuickCheck

spec ::
  CircuitSpec
    VerificationConditions
    B.ByteString
    ( SimpleUniversalSemMap B.ByteString VerificationConditions SymInteger VerificationConditions Integer
    )
spec =
  defaultCircuitSpec
    [ (ComponentSpec "+" 3 1, 1),
      (ComponentSpec "*" 2 1, 1),
      (ComponentSpec "+-" 2 2, 1)
    ]
    2
    2
    arithUSem

v :: M VerificationConditions (Circuit B.ByteString SymInteger)
v = genCircuit AssertionViolation spec

r :: M VerificationConditions [SymInteger]
r = do
  v1 <- v
  interpretCircuit
    AssertionViolation
    [1, 2]
    v1
    arithSem
    1
    ( HomogeneousSGen $ const $ simpleFresh () ::
        HomogeneousSGen B.ByteString SymInteger
    )

concreteCircuit :: CCircuit B.ByteString Integer
concreteCircuit =
  CCircuit
    2
    [CNode "+" [3] [0, 1, 2], CNode "*" [2] [0, 1], CNode "+-" [4, 5] [2, 3]]
    [4, 5]

qcspec :: PlainCSpec VerificationConditions Integer
qcspec = PlainCSpec qcspec'
  where
    qcspec' :: [Integer] -> Either VerificationConditions [Integer] -> Bool
    qcspec' [a, b] (Right [c, d]) = c == (a + b + a * b + a * b) && d == (-(a + b))
    qcspec' _ _ = False

qcsspec :: PlainSSpec VerificationConditions SymInteger
qcsspec = PlainSSpec qcsspec'
  where
    qcsspec' :: [SymInteger] -> Either VerificationConditions [SymInteger] -> SymBool
    qcsspec' [a, b] (Right [c, d]) = c ==~ (a + b + a * b + a * b) &&~ d ==~ (-(a + b))
    qcsspec' _ _ = con False

gen :: SimpleCGen Integer
gen = SimpleCGen $ \i -> vectorOf 2 (resize i arbitrary)

qcProblem ::
  QuickCheckProblem
    VerificationConditions
    Integer
    B.ByteString
    ( SimpleUniversalSemMap
        B.ByteString
        VerificationConditions
        SymInteger
        VerificationConditions
        Integer
    )
    Integer
qcProblem = QuickCheckProblem gen [200] qcspec arithUSem concreteCircuit 10000

igen :: HomogeneousSGen B.ByteString SymInteger
igen = HomogeneousSGen $ const $ simpleFresh ()

cegisQCProblem ::
  CegisQCProblem
    VerificationConditions
    SymInteger
    VerificationConditions
    Integer
    B.ByteString
    B.ByteString
    SymInteger
    Integer
cegisQCProblem =
  CegisQCProblem
    gen
    [100]
    qcspec
    arithUSem
    10000
    qcsspec
    spec
    AssertionViolation
    AssertionViolation
    arithUSem
    igen

testMultipleOutputs :: IO ()
testMultipleOutputs = do
  let v1 = runFreshT v "x"
  print v1
  let r1 = runFreshT r "x"
  print r1
  r <- quickCheckCCircuit qcProblem
  print r

  let v =
        interpretCCircuit
          [4, 5]
          concreteCircuit
          arithUSem
  print v

  v <- cegisQC (precise z3) cegisQCProblem
  print v
