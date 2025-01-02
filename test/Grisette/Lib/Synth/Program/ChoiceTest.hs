module Grisette.Lib.Synth.Program.ChoiceTest (choiceTest) where

import Grisette.Lib.Synth.Program.Choice.ChoiceTreeTest (choiceTreeTest)
import Grisette.Lib.Synth.Program.Choice.ComponentBagTest (componentBagTest)
import Test.Framework (Test, testGroup)

choiceTest :: Test
choiceTest = testGroup "Choice" [choiceTreeTest, componentBagTest]
