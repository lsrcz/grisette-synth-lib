module Grisette.Lib.Synth.Program.ProgNaming (ProgNaming (..)) where

import qualified Data.Text as T

class ProgNaming prog where
  nameProg :: prog -> T.Text
