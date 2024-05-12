module Grisette.Lib.Synth.Program.ProgGPretty
  ( ProgGPretty (..),
    gprettyProg,
  )
where

import qualified Data.Map.Ordered as OM
import qualified Data.Text as T
import Grisette.Lib.Synth.Util.Pretty (Doc, concatWith, hardline)

class ProgGPretty prog where
  topologicalGPrettyProg ::
    prog -> OM.OMap T.Text (Doc ann) -> OM.OMap T.Text (Doc ann)

gprettyProg :: (ProgGPretty prog) => prog -> Doc ann
gprettyProg prog =
  concatWith (\l r -> l <> hardline <> r) allProgs
  where
    allProgs = topologicalGPrettyProg prog OM.empty
