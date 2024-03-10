module Grisette.Lib.Synth.Program.BuiltinProgConstraintsTest
  ( builtinProgConstraintsTest,
  )
where

import Grisette.Lib.Synth.Program.BuiltinProgConstraints.LinearDefUseTest
  ( linearDefUseTest,
  )
import Test.Framework (Test, testGroup)

builtinProgConstraintsTest :: Test
builtinProgConstraintsTest =
  testGroup
    "BuiltinProgConstraints"
    [ linearDefUseTest
    ]
