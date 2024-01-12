module Grisette.Lib.Synth.Util.Show (showText) where

import Data.String (IsString (fromString))
import qualified Data.Text as T

showText :: (Show a) => a -> T.Text
showText = fromString . show
