module Grisette.Lib.Synth.Reasoning.Synthesis.Problem
  ( addThenDoubleSpec,
    addThenDoubleGen,
    divModTwiceSpec,
    divModTwiceGen,
  )
where

import Test.QuickCheck.Counterexamples
  ( Arbitrary (arbitrary),
    Gen,
    suchThat,
    vectorOf,
  )

addThenDoubleSpec :: [Integer] -> [Integer]
addThenDoubleSpec [x, y] = [x + y, 2 * (x + y)]
addThenDoubleSpec _ = error "Error"

addThenDoubleGen :: Gen [Integer]
addThenDoubleGen = vectorOf 2 arbitrary

divModTwiceSpec :: [Integer] -> [Integer]
divModTwiceSpec [x, y] =
  let [a, b] = [x `div` y, x `mod` y]
   in [a `div` b, a `mod` b]
divModTwiceSpec _ = error "Error"

divModTwiceGen :: Gen [Integer]
divModTwiceGen = flip suchThat noException $ vectorOf 2 arbitrary
  where
    noException [a, b] = b /= 0 && a `mod` b /= 0
    noException _ = False
