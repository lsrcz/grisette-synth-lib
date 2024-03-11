{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.Concrete.OpPretty
  ( VarIdMap,
    OpPrettyError (..),
    OpPretty (..),
    prettyArguments,
    prettyResults,
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError))
import Data.Foldable (traverse_)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette (Default (Default), GPretty (gpretty), Mergeable)
import Grisette.Lib.Synth.Util.Pretty
  ( Doc,
    parenCommaList,
    parenCommaListIfNotSingle,
  )
import Grisette.Lib.Synth.Util.Show (showText)
import Grisette.Lib.Synth.VarId (ConcreteVarId)

data OpPrettyError varId op
  = IncorrectNumberOfArguments op Int
  | UndefinedArgument Int varId
  | IncorrectNumberOfResults op Int Int
  | RedefinedResult Int varId
  deriving (Show, Eq, Generic, Functor)
  deriving (Mergeable) via (Default (OpPrettyError varId op))

instance
  (GPretty op, ConcreteVarId varId) =>
  GPretty (OpPrettyError varId op)
  where
  gpretty (IncorrectNumberOfArguments op numOfArguments) =
    "Incorrect number of arguments for "
      <> gpretty op
      <> ": it cannot accept "
      <> gpretty numOfArguments
      <> " arguments."
  gpretty (UndefinedArgument idx varId) =
    "The argument "
      <> gpretty (toInteger varId)
      <> " at index "
      <> gpretty idx
      <> " is undefined."
  gpretty (IncorrectNumberOfResults op numOfArguments numOfResults) =
    "Incorrect number of result for "
      <> gpretty op
      <> ": it cannot return "
      <> gpretty numOfResults
      <> " results when supplied with"
      <> gpretty numOfArguments
      <> " arguments."
  gpretty (RedefinedResult idx varId) =
    "The result "
      <> gpretty (toInteger varId)
      <> " at index "
      <> gpretty idx
      <> " is redefined."

class OpPretty op where
  describeArguments ::
    op -> Int -> Either (OpPrettyError varId op) [Maybe T.Text]
  prefixResults ::
    op -> Int -> Int -> Either (OpPrettyError varId op) [T.Text]

type VarIdMap varId = HM.HashMap varId T.Text

prettyArguments ::
  (ConcreteVarId varId, OpPretty op) =>
  op ->
  [varId] ->
  VarIdMap varId ->
  Either (OpPrettyError varId op) (Doc ann)
prettyArguments op varIds map = do
  let lookupVarId (idx, varId) =
        maybe
          (throwError $ UndefinedArgument idx varId)
          return
          (HM.lookup varId map)
  argNames <- traverse lookupVarId $ zip [0 ..] varIds
  argDescriptions <- describeArguments op (length varIds)

  let describe argName Nothing = gpretty argName
      describe argName (Just argDesc) =
        gpretty argDesc <> "=" <> gpretty argName
  let argPretty = zipWith describe argNames argDescriptions
  return $ parenCommaList argPretty

prettyResults ::
  (ConcreteVarId varId, OpPretty op) =>
  op ->
  Int ->
  [varId] ->
  VarIdMap varId ->
  Either (OpPrettyError varId op) (VarIdMap varId, Doc ann)
prettyResults op numOfArguments varIds map = do
  let ensureNotRedefined (idx, varId) =
        when (HM.member varId map) $ throwError $ RedefinedResult idx varId
  traverse_ ensureNotRedefined $ zip [0 ..] varIds
  prefixes <- prefixResults op numOfArguments (length varIds)
  let names =
        zipWith
          (\prefix varId -> prefix <> showText (toInteger varId))
          prefixes
          varIds
  let newMap = HM.union map $ HM.fromList $ zip varIds names
  return (newMap, parenCommaListIfNotSingle $ gpretty <$> names)
