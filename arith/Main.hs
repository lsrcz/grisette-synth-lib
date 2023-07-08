{-# LANGUAGE OverloadedStrings #-}

module Main where

import Component.CInputGen
import Component.Circuit
import Component.ConcreteCircuit
import Component.Monad
import Component.ProgramSpec
import Component.QuickCheck
import Component.SemMap
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as M
import Grisette
import Sem
import Test.QuickCheck

spec :: CircuitSpec B.ByteString (SimpleOpSemMap B.ByteString VerificationConditions SymInteger)
spec =
  defaultCircuitSpec
    [ (ComponentSpec "+" 3, 1),
      (ComponentSpec "*" 2, 1)
    ]
    2
    1
    arithSem

v :: M VerificationConditions (Circuit B.ByteString SymInteger)
v = genCircuit AssertionViolation spec

r :: M VerificationConditions [SymInteger]
r = do
  v1 <- v
  interpretCircuit AssertionViolation [1, 2] v1 arithSem (const $ const ())

concreteCircuit :: CCircuit B.ByteString Integer
concreteCircuit =
  CCircuit
    2
    [CNode "+" 3 [0, 1, 2], CNode "*" 2 [0, 1]]
    [3]

qcspec :: PlainCSpec VerificationConditions Integer
qcspec = PlainCSpec qcspec'
  where
    qcspec' :: [Integer] -> Either VerificationConditions [Integer] -> Bool
    qcspec' [a, b] (Right [c]) = c == a + b + a * b
    qcspec' _ _ = False

gen :: SimpleCGen Integer
gen = SimpleCGen $ \i -> vectorOf 2 (resize i arbitrary)

main :: IO ()
main = do
  let r1 = runFreshT r "x"
  print r1
  r <- quickCheckCCircuit (QuickCheckProblem gen [200] qcspec arithUSem concreteCircuit)
  print r

  let v =
        interpretCCircuit
          [4, 5]
          concreteCircuit
          arithUSem
  print v
