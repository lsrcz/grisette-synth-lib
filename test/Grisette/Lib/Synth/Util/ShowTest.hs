{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Util.ShowTest (showTest) where

import qualified Data.Text as T
import Grisette.Lib.Synth.Util.Show (showText)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

showTest :: Test
showTest =
  testGroup
    "Grisette.Lib.Synth.Util.Show"
    [testCase "showText" $ showText (123 :: Integer) @?= ("123" :: T.Text)]
