{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Grisette.Lib.Synth.Program.NullProg (NullProg) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Grisette (Default (Default), EvalSym, Mergeable, ToCon, ToSym)
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Program.Concrete.Program
  ( ProgPPrint (pformatProg),
    ProgToDot (toDotProg),
  )
import Grisette.Lib.Synth.Program.ProgNaming (ProgNaming (nameProg))
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))
import Grisette.Lib.Synth.Program.ProgTyping (ProgTyping (typeProg))
import Grisette.Lib.Synth.Program.ProgUtil
  ( ProgUtil
      ( ProgOpType,
        ProgStmtType,
        ProgTypeType,
        ProgVarIdType
      ),
    ProgUtilImpl
      ( getProgArgIds,
        getProgNumStmts,
        getProgResIds,
        getProgStmtAtIdx
      ),
    StmtUtil
      ( StmtOpType,
        StmtVarIdType
      ),
    StmtUtilImpl
      ( getStmtArgIds,
        getStmtDisabled,
        getStmtOp,
        getStmtResIds
      ),
  )

data NullProg ty
  deriving (Generic)
  deriving anyclass (NFData, Hashable)
  deriving
    (EvalSym, Mergeable, ToCon (NullProg ty0), ToSym (NullProg ty0))
    via (Default (NullProg ty))

instance Show (NullProg ty) where
  show _ = error "Impossible"

instance Eq (NullProg ty) where
  _ == _ = error "Impossible"

instance (MonadContext ctx) => ProgSemantics semObj (NullProg ty) val ctx where
  runProg _ _ = error "Impossible"

instance ProgNaming (NullProg ty) where
  nameProg _ = error "Impossible"

instance ProgTyping (NullProg ty) where
  typeProg _ = error "Impossible"

instance ProgPPrint (NullProg ty) where
  pformatProg _ = error "Impossible"

instance ProgToDot (NullProg ty) where
  toDotProg _ = error "Impossible"

data NullStmt ty
  deriving (Generic)
  deriving anyclass (NFData, Hashable)
  deriving
    (EvalSym, Mergeable, ToCon (NullStmt ty0), ToSym (NullStmt ty0))
    via (Default (NullStmt ty))

instance Show (NullStmt ty) where
  show _ = error "Impossible"

instance Eq (NullStmt ty) where
  _ == _ = error "Impossible"

instance StmtUtilImpl (NullStmt ty) () () where
  getStmtArgIds = error "Impossible"
  getStmtResIds = error "Impossible"
  getStmtOp = error "Impossible"
  getStmtDisabled _ = error "Impossible"

instance StmtUtil (NullStmt ty) where
  type StmtVarIdType (NullStmt _) = ()
  type StmtOpType (NullStmt _) = ()

instance ProgUtilImpl (NullProg ty) () (NullStmt ty) () where
  getProgArgIds _ = error "Impossible"
  getProgResIds _ = error "Impossible"
  getProgNumStmts _ = error "Impossible"
  getProgStmtAtIdx _ _ = error "Impossible"

instance ProgUtil (NullProg ty) where
  type ProgOpType _ = ()
  type ProgTypeType (NullProg ty) = ty
  type ProgVarIdType _ = ()
  type ProgStmtType (NullProg ty) = NullStmt ty