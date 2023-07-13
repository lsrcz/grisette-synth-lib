module Component.PrettyPrinterUtil
  ( commaSepParenLists,
  )
where

import Prettyprinter

commaSepParenLists :: [Doc ann] -> Doc ann
commaSepParenLists =
  encloseSep lparen rparen (comma <> space)
