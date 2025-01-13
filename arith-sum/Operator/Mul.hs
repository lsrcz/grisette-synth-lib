{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Operator.Mul (Mul (..), mul) where

import qualified Data.HashSet as HS
import Data.List ((\\))
import EvalMode (MonadEvalContext)
import Grisette
  ( LogicalOp (false),
    PPrint (pformat),
    allClasses0,
    deriveGADT,
    mrgReturn,
    pprintClasses,
  )
import Grisette.Lib.Synth.Combinator.Sum (type (:<:) (inj))
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Operator.OpReachableSymbols
  ( OpReachableSymbols (opReachableSymbols),
  )
import Grisette.Lib.Synth.Operator.OpSemantics
  ( DefaultSem,
    OpSemantics (applyOp),
    pureBinaryOp,
  )
import Grisette.Lib.Synth.Operator.OpTyping
  ( DefaultType,
    OpTyping (OpTypeType, typeOp),
    binaryDefaultType,
    simpleTyping,
  )
import Grisette.Lib.Synth.Program.ComponentSketch
  ( OpSymmetryReduction (opCommutativeArgPos, opUnreorderable),
  )
import Grisette.Lib.Synth.Program.Concrete (OpPPrint (describeArguments))
import Grisette.Unified (GetInteger)

data Mul = Mul

mul :: (Mul :<: op) => op
mul = inj Mul

deriveGADT [''Mul] (allClasses0 \\ pprintClasses)

instance (MonadContext ctx) => OpTyping Mul ctx where
  type OpTypeType Mul = DefaultType
  typeOp = simpleTyping $ const binaryDefaultType

instance
  (MonadEvalContext mode ctx, i ~ GetInteger mode) =>
  OpSemantics DefaultSem Mul i ctx
  where
  applyOp _ _ Mul = pureBinaryOp "Mul" (*)

instance PPrint Mul where
  pformat Mul = "mul"

instance OpPPrint Mul where
  describeArguments Mul = Right [Just "lhs", Just "rhs"]

instance OpSymmetryReduction Mul where
  opUnreorderable _ _ = false
  opCommutativeArgPos Mul = mrgReturn [[0, 1]]

instance OpReachableSymbols Mul where
  opReachableSymbols _ = HS.empty
