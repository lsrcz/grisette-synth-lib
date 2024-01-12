{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Util.Pretty
  ( encloseListIfNotSingle,
    encloseList,
    renderDoc,
  )
where

import qualified Data.Text as T
import Prettyprinter
  ( Doc,
    LayoutOptions (LayoutOptions, layoutPageWidth),
    PageWidth (AvailablePerLine),
    cat,
    flatAlt,
    group,
    layoutPretty,
    nest,
    unAnnotate,
    vcat,
  )
import Prettyprinter.Render.Text (renderStrict)

encloseListIfNotSingle :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
encloseListIfNotSingle l r s ds = case ds of
  [d] -> d
  _ -> encloseList l r s ds

encloseList :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
encloseList l r s ds = case ds of
  [] -> l <> r
  [d] -> cat [nest 2 $ vcat [l, d], r]
  _ ->
    group $
      vcat [nest 2 $ vcat [l, vcat $ map (<> sep) (init ds), last ds], r]
  where
    sep = flatAlt s (s <> " ")

renderDoc :: Int -> Doc ann -> T.Text
renderDoc w doc = renderStrict $ layoutPretty layoutOptions (unAnnotate doc)
  where
    layoutOptions = LayoutOptions {layoutPageWidth = AvailablePerLine w 1}
