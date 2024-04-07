module Grisette.Lib.Synth.Reasoning.Synthesis.Problem
  ( addThenDoubleSpec,
    addThenDoubleGen,
    divModTwiceSpec,
    divModTwiceGen,
    addThenDoubleReverseSpec,
    addThenDivModSpec,
    addThenDivModGen,
  )
where

import Grisette.Lib.Synth.Reasoning.Matcher (EqMatcher (EqMatcher))
import Grisette.Lib.Synth.Reasoning.ReverseMatcher
  ( ReverseMatcher (ReverseMatcher),
  )
import Test.QuickCheck.Counterexamples
  ( Arbitrary (arbitrary),
    Gen,
    suchThat,
    vectorOf,
  )

addThenDoubleSpec :: [Integer] -> ([Integer], EqMatcher)
addThenDoubleSpec [x, y] = ([x + y, 2 * (x + y)], EqMatcher)
addThenDoubleSpec _ = error "Error"

addThenDoubleReverseSpec :: [Integer] -> ([Integer], ReverseMatcher)
addThenDoubleReverseSpec [x, y] = ([2 * (x + y), x + y], ReverseMatcher)
addThenDoubleReverseSpec _ = error "Error"

addThenDoubleGen :: Gen [Integer]
addThenDoubleGen = vectorOf 2 arbitrary

addThenDivModSpec :: [Integer] -> ([Integer], EqMatcher)
addThenDivModSpec [x, y, z] =
  ([(x + y) `div` z, (x + y) `mod` z], EqMatcher)
addThenDivModSpec _ = error "Error"

addThenDivModGen :: Gen [Integer]
addThenDivModGen = flip suchThat noException $ vectorOf 3 arbitrary
  where
    noException [_, _, c] = c /= 0
    noException _ = False

divModTwiceSpec :: [Integer] -> ([Integer], EqMatcher)
divModTwiceSpec [x, y] =
  let [a, b] = [x `div` y, x `mod` y]
   in ([a `div` b, a `mod` b], EqMatcher)
divModTwiceSpec _ = error "Error"

divModTwiceGen :: Gen [Integer]
divModTwiceGen = flip suchThat noException $ vectorOf 2 arbitrary
  where
    noException [a, b] = b /= 0 && a `mod` b /= 0
    noException _ = False
