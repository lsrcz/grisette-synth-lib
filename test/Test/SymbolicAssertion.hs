module Test.SymbolicAssertion ((.@?=)) where

import GHC.Stack (HasCallStack)
import Grisette (SEq ((./=), (.==)), precise, solve, z3)

(.@?=) :: (HasCallStack, Show a, SEq a) => a -> a -> IO ()
(.@?=) actual expected = do
  canBeNotEqual <- solve (precise z3) $ actual ./= expected
  canBeEqual <- solve (precise z3) $ actual .== expected
  case (canBeNotEqual, canBeEqual) of
    (Left _, Right _) -> return ()
    (Right _, _) -> fail "Can be not equal"
    (_, Left _) -> fail "Cannot be equal"
