{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.Concrete.OpPretty
  ( VarIdMap,
    OpPrettyError (..),
    DescribeArguments (..),
    PrefixByType (..),
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
import Grisette.Lib.Synth.Context (ConcreteContext)
import Grisette.Lib.Synth.Operator.OpTyping (OpTyping (typeOp))
import Grisette.Lib.Synth.TypeSignature (TypeSignature (TypeSignature))
import Grisette.Lib.Synth.Util.Pretty
  ( Doc,
    parenCommaList,
    parenCommaListIfNotSingle,
  )
import Grisette.Lib.Synth.Util.Show (showText)
import Grisette.Lib.Synth.VarId (ConcreteVarId)

data OpPrettyError varId op
  = IncorrectNumberOfArguments op Int Int
  | UndefinedArgument Int varId
  | IncorrectNumberOfResults op Int Int
  | RedefinedResult Int varId
  | PrettyTypingError op T.Text
  deriving (Show, Eq, Generic, Functor)
  deriving (Mergeable) via (Default (OpPrettyError varId op))

instance
  (GPretty op, ConcreteVarId varId) =>
  GPretty (OpPrettyError varId op)
  where
  gpretty (IncorrectNumberOfArguments op expectedNumArguments numArguments) =
    "Incorrect number of arguments for "
      <> gpretty op
      <> ": expected "
      <> gpretty expectedNumArguments
      <> " arguments, but got"
      <> gpretty numArguments
  gpretty (UndefinedArgument idx varId) =
    "The argument "
      <> gpretty (toInteger varId)
      <> " at index "
      <> gpretty idx
      <> " is undefined."
  gpretty (IncorrectNumberOfResults op expectedNumResults numResults) =
    "Incorrect number of result for "
      <> gpretty op
      <> ": expected "
      <> gpretty expectedNumResults
      <> " results, but got"
      <> gpretty numResults
      <> " results."
  gpretty (RedefinedResult idx varId) =
    "The result "
      <> gpretty (toInteger varId)
      <> " at index "
      <> gpretty idx
      <> " is redefined."
  gpretty (PrettyTypingError op err) =
    "Error while typing "
      <> gpretty op
      <> ": "
      <> gpretty err

class PrefixByType ty where
  prefixByType :: ty -> T.Text

class DescribeArguments op where
  describeArguments :: op -> Either (OpPrettyError varId op) [Maybe T.Text]

prefixResults ::
  (OpTyping op ty ConcreteContext, PrefixByType ty) =>
  op ->
  Either (OpPrettyError varId op) [T.Text]
prefixResults op = case typeOp op of
  Right (TypeSignature _ resTypes) ->
    return $ prefixByType <$> resTypes
  Left err -> throwError $ PrettyTypingError op err

type VarIdMap varId = HM.HashMap varId T.Text

prettyArguments ::
  ( ConcreteVarId varId,
    DescribeArguments op,
    OpTyping op ty ConcreteContext,
    PrefixByType ty
  ) =>
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
  argDescriptions <- describeArguments op
  when (length argNames /= length argDescriptions) $
    throwError $
      IncorrectNumberOfArguments op (length argDescriptions) (length argNames)

  let describe argName Nothing = gpretty argName
      describe argName (Just argDesc) =
        gpretty argDesc <> "=" <> gpretty argName
  let argPretty = zipWith describe argNames argDescriptions
  return $ parenCommaList argPretty

prettyResults ::
  ( ConcreteVarId varId,
    DescribeArguments op,
    OpTyping op ty ConcreteContext,
    PrefixByType ty
  ) =>
  op ->
  [varId] ->
  VarIdMap varId ->
  Either (OpPrettyError varId op) (VarIdMap varId, Doc ann)
prettyResults op varIds map = do
  let ensureNotRedefined (idx, varId) =
        when (HM.member varId map) $ throwError $ RedefinedResult idx varId
  traverse_ ensureNotRedefined $ zip [0 ..] varIds
  prefixes <- prefixResults op
  when (length varIds /= length prefixes) $
    throwError $
      IncorrectNumberOfResults op (length prefixes) (length varIds)
  let names =
        zipWith
          (\prefix varId -> prefix <> showText (toInteger varId))
          prefixes
          varIds
  let newMap = HM.union map $ HM.fromList $ zip varIds names
  return (newMap, parenCommaListIfNotSingle $ gpretty <$> names)
