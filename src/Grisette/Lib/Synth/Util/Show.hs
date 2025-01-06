module Grisette.Lib.Synth.Util.Show (showText, showFloat, showDiffTime) where

import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Numeric (showFFloat)

showText :: (Show a) => a -> T.Text
showText = fromString . show

showFloat :: (RealFloat a) => a -> String
showFloat x = showFFloat (Just 2) x []

showDiffTime :: NominalDiffTime -> String
showDiffTime dt = showFloat (realToFrac dt) <> "s"
