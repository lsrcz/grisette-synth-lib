{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Program.ProgPPrint
  ( ProgPPrint (..),
  )
where

import Data.List (intersperse)
import qualified Data.Text as T
import Grisette (PPrint (pformat))
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable (SymbolTable))
import Grisette.Lib.Synth.Util.Pretty (Doc, hardline)

class ProgPPrint prog where
  pformatProg :: T.Text -> prog -> Either (Doc ann) (Doc ann)

instance (ProgPPrint prog) => PPrint (SymbolTable prog) where
  pformat (SymbolTable lst) =
    mconcat $ intersperse hardline $ go <$> lst
    where
      go (key, prog) =
        case pformatProg key prog of
          Left err -> err
          Right doc -> doc
