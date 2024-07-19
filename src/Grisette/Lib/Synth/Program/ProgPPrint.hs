module Grisette.Lib.Synth.Program.ProgPPrint
  ( ProgPPrint (..),
    pformatProg,
  )
where

import qualified Data.Map.Ordered as OM
import qualified Data.Text as T
import Grisette.Lib.Synth.Util.Pretty (Doc, concatWith, hardline)

class ProgPPrint prog where
  topologicalPFormatProg ::
    prog -> OM.OMap T.Text (Doc ann) -> OM.OMap T.Text (Doc ann)

pformatProg :: (ProgPPrint prog) => prog -> Doc ann
pformatProg prog =
  concatWith (\l r -> l <> hardline <> r) allProgs
  where
    allProgs = topologicalPFormatProg prog OM.empty
