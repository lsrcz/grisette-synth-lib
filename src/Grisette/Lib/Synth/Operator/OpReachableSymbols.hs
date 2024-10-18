module Grisette.Lib.Synth.Operator.OpReachableSymbols
  ( OpReachableSymbols (..),
  )
where

import qualified Data.HashSet as HS
import qualified Data.Text as T
import Grisette (Mergeable, PlainUnion (overestimateUnionValues), Union)

class OpReachableSymbols op where
  opReachableSymbols :: op -> HS.HashSet T.Text

instance
  (OpReachableSymbols op, Mergeable op) =>
  OpReachableSymbols (Union op)
  where
  opReachableSymbols =
    mconcat . fmap opReachableSymbols . overestimateUnionValues
