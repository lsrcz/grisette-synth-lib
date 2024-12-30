{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Program.ProgPPrint
  ( ProgPPrint (..),
  )
where

import qualified Data.Text as T
import Grisette.Lib.Synth.Util.Pretty (Doc)

class ProgPPrint prog where
  pformatProg :: T.Text -> prog -> Either (Doc ann) (Doc ann)
