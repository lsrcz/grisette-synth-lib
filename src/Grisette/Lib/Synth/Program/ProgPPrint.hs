{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Program.ProgPPrint
  ( ProgPPrint (..),
  -- pformatProg,
  )
where

import Data.List (intersperse)
import Grisette (PPrint (pformat))
import Grisette.Lib.Synth.Program.ProgTyping (ProgTyping)
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable (SymbolTable))
import Grisette.Lib.Synth.Util.Pretty (Doc, hardline)

class ProgPPrint prog where
  pformatProg :: prog -> Either (Doc ann) (Doc ann)

instance
  (ProgPPrint prog, ProgTyping prog) =>
  PPrint (SymbolTable prog)
  where
  pformat (SymbolTable lst) =
    mconcat $ intersperse hardline $ go <$> lst
    where
      go (name, prog) =
        case pformatProg prog of
          Left err -> pformat name <> ": " <> err
          Right doc -> pformat name <> ": " <> doc
