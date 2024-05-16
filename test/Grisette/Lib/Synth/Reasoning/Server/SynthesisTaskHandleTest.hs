{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Lib.Synth.Reasoning.Server.SynthesisTaskHandleTest
  ( synthesisTaskHandleTest,
  )
where

import Data.Proxy (Proxy (Proxy))
import Grisette.Lib.Synth.Reasoning.Server.BaseTaskHandleTestCommon
  ( baseTaskHandleTestCommon,
  )
import Grisette.Lib.Synth.Reasoning.Server.SynthesisTaskHandle
  ( SynthesisTaskHandle,
  )
import Grisette.Lib.Synth.Reasoning.Synthesis.ComponentSketchTest
  ( ConProg,
  )
import Test.Framework (Test, testGroup)

type Handle = SynthesisTaskHandle ConProg

synthesisTaskHandleTest :: Test
synthesisTaskHandleTest =
  testGroup
    "SynthesisTaskHandle"
    [ baseTaskHandleTestCommon "BaseTaskHandle" (Proxy @Handle)
    ]
