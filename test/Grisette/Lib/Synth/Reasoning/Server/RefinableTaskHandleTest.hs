{-# LANGUAGE TypeApplications #-}

module Grisette.Lib.Synth.Reasoning.Server.RefinableTaskHandleTest
  ( refinableTaskHandleTest,
  )
where

import Data.Typeable (Proxy (Proxy))
import Grisette.Lib.Synth.Reasoning.Server.BaseTaskHandleTestCommon
  ( baseTaskHandleTestCommon,
  )
import Grisette.Lib.Synth.Reasoning.Server.RefinableTaskHandle
  ( RefinableTaskHandle,
  )
import Grisette.Lib.Synth.Reasoning.Synthesis.ComponentSketchTest
  ( ConProg,
  )
import Test.Framework (Test, testGroup)

type Handle = RefinableTaskHandle ConProg

refinableTaskHandleTest :: Test
refinableTaskHandleTest =
  testGroup
    "RefinableTaskHandle"
    [ baseTaskHandleTestCommon "BaseTaskHandle" (Proxy @Handle)
    ]
