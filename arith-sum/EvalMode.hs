{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module EvalMode
  ( EvalMode,
    MonadEvalMode,
    deriveGADTWithEvalMode,
    MonadEvalContext,
  )
where

import Grisette
  ( DeriveConfig (evalModeConfig),
    EvalModeConfig (EvalModeConstraints),
    deriveGADTWith,
  )
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Unified
  ( TheoryToUnify (UBool, UInteger),
    genEvalMode,
  )
import Language.Haskell.TH (DecsQ, Name)

genEvalMode "EvalMode" [UBool, UInteger]

deriveGADTWithEvalMode :: [Name] -> [Name] -> DecsQ
deriveGADTWithEvalMode =
  deriveGADTWith
    ( mempty
        { evalModeConfig = [(0, EvalModeConstraints [''EvalMode])]
        }
    )

type MonadEvalContext mode ctx = (MonadContext ctx, MonadEvalMode mode ctx)
