module Grisette.Lib.Synth.Program.BuiltinProgConstraintsTest
  ( builtinProgConstraintsTest,
  )
where

-- import Grisette.Lib.Synth.Program.BuiltinProgConstraints.ComponentSymmetryReductionTest
--   ( componentSymmetryReductionTest,
--   )
import Grisette.Lib.Synth.Program.BuiltinProgConstraints.LivelinessTest
  ( livelinessTest,
  )
import Test.Framework (Test, testGroup)

builtinProgConstraintsTest :: Test
builtinProgConstraintsTest =
  testGroup
    "BuiltinProgConstraints"
    [ -- componentSymmetryReductionTest,
      livelinessTest
    ]
