{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Grisette.Lib.Synth.Program.ProgUtil
  ( StmtUtil (..),
    ProgUtil (..),
  )
where

import Grisette (SymBool)
import Grisette.Lib.Synth.Context (MonadContext)

class StmtUtil stmt varId | stmt -> varId where
  type StmtOpType stmt
  getStmtArgIds :: stmt -> [varId]
  getStmtResIds :: stmt -> [varId]
  getStmtOp :: stmt -> StmtOpType stmt
  getStmtDisabled :: stmt -> SymBool

class
  (StmtUtil stmt varId) =>
  ProgUtil prog stmt varId
    | prog -> stmt,
      prog -> varId
  where
  type ProgTypeType prog
  getProgArgIds :: prog -> [varId]
  getProgResIds :: prog -> [varId]
  getProgNumStmts :: prog -> Int
  getProgStmtAtIdx :: (MonadContext ctx) => prog -> Int -> ctx stmt
