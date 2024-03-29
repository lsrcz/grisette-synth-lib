{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.Concrete.OpToDot
  ( OpToDot (..),
    VarIdToLabel,
    argumentsToFieldEdges,
    resultsToFieldEdges,
  )
where

import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Foldable (traverse_)
import Data.GraphViz (DotEdge (DotEdge), DotSubGraph)
import Data.GraphViz.Attributes.Complete
  ( Attribute (HeadPort, TailPort),
    PortName (PN),
    PortPos (LabelledPort),
    RecordField (LabelledTarget),
    RecordFields,
  )
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map.Ordered as OM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Grisette.Lib.Synth.Program.Concrete.OpPretty
  ( OpPretty (describeArguments, prefixResults),
    OpPrettyError
      ( IncorrectNumberOfArguments,
        IncorrectNumberOfResults,
        RedefinedResult,
        UndefinedArgument
      ),
  )
import Grisette.Lib.Synth.Util.Show (showText)
import Grisette.Lib.Synth.VarId (ConcreteVarId)

class (OpPretty op) => OpToDot op where
  topologicalSubProgToDot ::
    op ->
    OM.OMap T.Text (DotSubGraph T.Text) ->
    OM.OMap T.Text (DotSubGraph T.Text)
  topologicalSubProgToDot _ = id

type VarIdToLabel varId = HM.HashMap varId (T.Text, PortName)

argumentsToFieldEdges ::
  (ConcreteVarId varId, OpToDot op) =>
  T.Text ->
  op ->
  [varId] ->
  VarIdToLabel varId ->
  Either (OpPrettyError varId op) (RecordFields, [DotEdge T.Text])
argumentsToFieldEdges nodeId op argIds map = do
  argDescriptions <- describeArguments op
  when (length argIds /= length argDescriptions) $
    throwError $
      IncorrectNumberOfArguments op (length argDescriptions) (length argIds)
  let argPortAtPos argPos = TL.fromStrict $ "arg" <> showText argPos
  let buildArgLabel argPos argDesc =
        LabelledTarget (PN $ argPortAtPos argPos) $
          maybe (argPortAtPos argPos) TL.fromStrict argDesc
  let argLabel = zipWith buildArgLabel [0 ..] argDescriptions
  let lookupLabel map idx varId =
        maybe
          (throwError $ UndefinedArgument idx varId)
          return
          (HM.lookup varId map)
  argPreLabels <- traverse (uncurry $ lookupLabel map) $ zip [0 ..] argIds
  let preLabelToEdge (from, port) argPos =
        DotEdge
          from
          nodeId
          [ HeadPort $ LabelledPort (PN $ argPortAtPos argPos) Nothing,
            TailPort $ LabelledPort port Nothing
          ]
  return (argLabel, zipWith preLabelToEdge argPreLabels [0 ..])

resultsToFieldEdges ::
  (ConcreteVarId varId, OpToDot op) =>
  T.Text ->
  op ->
  [varId] ->
  VarIdToLabel varId ->
  Either
    (OpPrettyError varId op)
    (VarIdToLabel varId, RecordFields)
resultsToFieldEdges nodeId op resIds map = do
  let ensureNotRedefined (idx, varId) =
        when (HM.member varId map) $ throwError $ RedefinedResult idx varId
  traverse_ ensureNotRedefined $ zip [0 ..] resIds
  prefixes <- prefixResults op
  when (length resIds /= length prefixes) $
    throwError $
      IncorrectNumberOfResults op (length prefixes) (length resIds)
  let resPortAtPos argPos = TL.fromStrict $ "res" <> showText argPos
  let buildResLabel argPos prefix =
        LabelledTarget (PN $ resPortAtPos argPos) $ TL.fromStrict prefix
  let resLabel = zipWith buildResLabel [0 ..] prefixes
  return
    ( foldl
        ( \m (resPos, resId) ->
            HM.insert resId (nodeId, PN $ resPortAtPos resPos) m
        )
        map
        $ zip [0 ..] resIds,
      resLabel
    )
