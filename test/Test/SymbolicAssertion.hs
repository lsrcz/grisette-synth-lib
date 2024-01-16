{-# LANGUAGE OverloadedStrings #-}

module Test.SymbolicAssertion ((.@?=), symShouldEq) where

import GHC.Stack (HasCallStack)
import Grisette
  ( EvaluateSym (evaluateSym),
    Model,
    SEq ((./=), (.==)),
    precise,
    solve,
    z3,
  )

(.@?=) :: (HasCallStack, Show a, SEq a, EvaluateSym a) => a -> a -> IO ()
(.@?=) actual expected =
  symShouldEq
    actual
    expected
    ( \m ->
        "Can be not equal, model: "
          <> show m
          <> ". Actual value: "
          <> show (evaluateSym False m actual)
          <> ". Expected value: "
          <> show (evaluateSym False m expected)
    )

symShouldEq ::
  (HasCallStack, Show a, SEq a, EvaluateSym a) =>
  a ->
  a ->
  (Model -> String) ->
  IO ()
symShouldEq actual expected notEqualCaseMessage = do
  canBeNotEqual <- solve (precise z3) $ actual ./= expected
  canBeEqual <- solve (precise z3) $ actual .== expected
  case (canBeNotEqual, canBeEqual) of
    (Left _, Right _) -> return ()
    (Right m, _) -> fail $ notEqualCaseMessage m
    (_, Left _) -> fail "Cannot be equal"
