{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Lib.Synth.Combinator.Invoke (Invoke (..)) where

import qualified Data.HashSet as HS
import Data.List ((\\))
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( GenSym (fresh),
    GenSymSimple (simpleFresh),
    LogicalOp (true),
    Mergeable,
    PPrint (pformat, pformatPrec),
    PPrint1 (liftPFormatPrec),
    allClasses01,
    derive,
    mrgReturn,
    mrgSequence,
    nest,
    pformatPrec1,
    pprintClasses,
    vcat,
    vsep,
    (<+>),
  )
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Operator.OpParser (OpParser (opParser))
import Grisette.Lib.Synth.Operator.OpReachableSymbols
  ( OpReachableSymbols (opReachableSymbols),
  )
import Grisette.Lib.Synth.Operator.OpSemantics (OpSemantics (applyOp))
import Grisette.Lib.Synth.Operator.OpTyping (OpTyping (OpTypeType, typeOp))
import Grisette.Lib.Synth.Program.Choice.Counting (SplitChoice (splitChoice))
import Grisette.Lib.Synth.Program.ComponentSketch.SymmetryReduction
  ( OpSymmetryReduction (opCommutativeArgPos, opUnreorderable),
  )
import Grisette.Lib.Synth.Program.Concrete
  ( OpPPrint (describeArguments),
    PrefixByType,
  )
import Grisette.Lib.Synth.Program.Concrete.Flatten
  ( OpFlatten (opForwardedSubProg),
  )
import Grisette.Lib.Synth.Program.CostModel.PerStmtCostModel (OpCost (opCost))
import Grisette.Lib.Synth.Program.ProgCost (lookupCost)
import Grisette.Lib.Synth.Program.ProgSemantics (runEvaledSymbol)
import Grisette.Lib.Synth.Type.TypeParser (TypeParser)
import Grisette.Lib.Synth.TypeSignature
  ( TypeSignature (TypeSignature),
    typeSignatureParser,
  )
import Grisette.Lib.Synth.Util.Parser (betweenBrackets, colon, identifier)
import qualified Grisette.Lib.Synth.Util.Parser as P

data Invoke ty = Invoke
  { signature :: TypeSignature ty,
    symbol :: T.Text
  }
  deriving (Generic)

derive [''Invoke] (allClasses01 \\ pprintClasses)

instance (PPrint ty) => PPrint (Invoke ty) where
  pformatPrec = pformatPrec1

instance PPrint1 Invoke where
  liftPFormatPrec ppt ppl _ (Invoke sig sym) =
    vcat
      [ nest 2 $
          vsep
            [ "invoke" <> "[",
              pformat sym <> ":" <+> liftPFormatPrec ppt ppl 0 sig
            ],
        "]"
      ]

instance (MonadContext ctx, Mergeable ty) => OpTyping (Invoke ty) ctx where
  type OpTypeType (Invoke ty) = ty
  typeOp (Invoke sig _) = mrgReturn sig

instance
  (MonadContext ctx, Mergeable ty, Mergeable val) =>
  OpSemantics semObj (Invoke ty) val ctx
  where
  applyOp _ table (Invoke _ sym) = runEvaledSymbol table sym

instance (GenSymSimple ty ty1) => GenSymSimple (Invoke ty) (Invoke ty1) where
  simpleFresh (Invoke (TypeSignature argTypes resTypes) sym) = do
    newArgTypes <- mapM simpleFresh argTypes
    newResTypes <- mapM simpleFresh resTypes
    return $ Invoke (TypeSignature newArgTypes newResTypes) sym

instance (GenSym ty ty1) => GenSym (Invoke ty) (Invoke ty1) where
  fresh (Invoke (TypeSignature argTypes resTypes) sym) = do
    newArgTypes <- mapM fresh argTypes
    newResTypes <- mapM fresh resTypes
    return $ do
      newArgTypes <- mrgSequence newArgTypes
      newResTypes <- mrgSequence newResTypes
      mrgReturn $ Invoke (TypeSignature newArgTypes newResTypes) sym

instance OpReachableSymbols (Invoke ty) where
  opReachableSymbols (Invoke _ sym) = HS.singleton sym

instance OpSymmetryReduction (Invoke ty) where
  opUnreorderable _ _ = true
  opCommutativeArgPos _ = mrgReturn []

instance (PPrint ty, Mergeable ty, PrefixByType ty) => OpPPrint (Invoke ty) where
  describeArguments (Invoke (TypeSignature argTypes _) _) =
    Right $ replicate (length argTypes) Nothing

instance (Mergeable op) => OpFlatten (Invoke ty) op where
  opForwardedSubProg (Invoke _ sym) = mrgReturn $ Left sym

instance
  (MonadContext ctx, Mergeable cost) =>
  OpCost costObj (Invoke ty) cost ctx
  where
  opCost _ table (Invoke _ sym) = lookupCost table sym

instance SplitChoice (Invoke ty) where
  splitChoice i = [i]

instance (TypeParser ty) => OpParser (Invoke ty) where
  opParser = do
    P.symbol "invoke"
    betweenBrackets $ do
      sym <- identifier
      colon
      ty <- typeSignatureParser
      return $ Invoke ty sym
