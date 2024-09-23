{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Program.ProgPPrint
  ( ProgPPrint (..),
  -- pformatProg,
  )
where

import Data.List (intersperse)
import Grisette (PPrint (pformat))
import Grisette.Lib.Synth.Program.ProgTyping
  ( ProgTypeTable,
    ProgTyping,
    typeSymbolTable,
  )
import Grisette.Lib.Synth.Program.ProgUtil (ProgUtil (ProgTypeType))
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable (SymbolTable))
import Grisette.Lib.Synth.Util.Pretty (Doc, hardline)

class ProgPPrint prog where
  pformatProg ::
    ProgTypeTable (ProgTypeType prog) ->
    prog ->
    Either (Doc ann) (Doc ann)

-- pformatProg :: (ProgPPrint prog) => prog -> Doc ann
-- pformatProg prog =
--   concatWith (\l r -> l <> hardline <> r) allProgs
--   where
--     allProgs = topologicalPFormatProg prog OM.empty

instance
  (ProgPPrint prog, ProgTyping prog) =>
  PPrint (SymbolTable prog)
  where
  pformat table@(SymbolTable lst) =
    mconcat $ intersperse hardline $ go <$> lst
    where
      go (name, prog) =
        case pformatProg (typeSymbolTable table) prog of
          Left _ -> pformat name <> ": err"
          Right doc -> pformat name <> ": " <> doc
