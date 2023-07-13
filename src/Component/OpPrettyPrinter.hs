{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Component.OpPrettyPrinter
  ( OpRegNamer (..),
    OpPrinterDescription (..),
    prettyPrintOpInst,
    simpleRegName,
    fixedRegName,
  )
where

import Component.PrettyPrinterUtil
import Data.ByteString qualified as B
import Data.ByteString.UTF8 qualified as U
import Data.HashMap.Strict qualified as M
import Data.Hashable
import Prettyprinter

class (Pretty op, Pretty idx) => OpRegNamer op idx where
  nameOutputs ::
    op ->
    [idx] ->
    M.HashMap idx (Doc acc) ->
    M.HashMap idx (Doc acc)

simpleRegName ::
  (Hashable idx, Pretty idx) =>
  Doc acc ->
  [idx] ->
  M.HashMap idx (Doc acc) ->
  M.HashMap idx (Doc acc)
simpleRegName _ [] regNames = regNames
simpleRegName prefix (idx : idxs) regNames =
  simpleRegName prefix idxs (M.insert idx (prefix <> pretty idx) regNames)

fixedRegName ::
  (Hashable idx, Pretty idx) =>
  [Doc acc] ->
  [idx] ->
  M.HashMap idx (Doc acc) ->
  M.HashMap idx (Doc acc)
fixedRegName _ [] regNames = regNames
fixedRegName (prefix : morePrefixes) (idx : idxs) regNames =
  fixedRegName morePrefixes idxs (M.insert idx (prefix <> pretty idx) regNames)

class OpPrinterDescription op where
  describeArgs :: op -> Int -> [Maybe (Doc acc)]
  opIsBinaryInfix :: op -> Bool

prettyPrintOpInst ::
  (Pretty op, Pretty idx, OpPrinterDescription op, Hashable idx) =>
  op ->
  [idx] ->
  [idx] ->
  M.HashMap idx (Doc acc) ->
  Doc acc
prettyPrintOpInst op outputs [a, b] regNames
  | opIsBinaryInfix op =
      simplePrettyPrintOutputs outputs regNames
        <+> "="
        <+> regNames M.! a
        <+> pretty op
        <+> regNames M.! b
prettyPrintOpInst op outputs inputs regNames =
  simplePrettyPrintOutputs outputs regNames
    <+> "="
    <+> pretty op
      <> simplePrettyPrintArgsWithDescription (describeArgs op $ length inputs) inputs regNames

simplePrettyPrintArgsWithDescription ::
  (Hashable idx) =>
  [Maybe (Doc acc)] ->
  [idx] ->
  M.HashMap idx (Doc acc) ->
  Doc acc
simplePrettyPrintArgsWithDescription desc idxs m =
  commaSepParenLists $
    zipWith
      ( \d i ->
          case d of
            Nothing -> m M.! i
            Just dd -> dd <> "=" <> (m M.! i)
      )
      (desc ++ repeat Nothing)
      idxs

simplePrettyPrintOutputs :: (Hashable idx) => [idx] -> M.HashMap idx (Doc acc) -> Doc acc
simplePrettyPrintOutputs [] m = error "Add least one output"
simplePrettyPrintOutputs [x] m = m M.! x
simplePrettyPrintOutputs os m = commaSepParenLists $ (m M.!) <$> os
