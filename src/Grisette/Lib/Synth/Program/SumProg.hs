{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Program.SumProg (SumProg (..)) where

import Data.Functor.Classes
  ( Show1 (liftShowsPrec),
    Show2 (liftShowsPrec2),
    showsPrec1,
  )
import Data.List ((\\))
import GHC.Generics (Generic)
import Grisette
  ( PPrint (pformatList, pformatPrec),
    PPrint1 (liftPFormatPrec),
    PPrint2 (liftPFormatPrec2),
    ToCon (toCon),
    allClasses012,
    deriveGADT,
    pformatPrec1,
    pprintClasses,
    showClasses,
  )
import Grisette.Lib.Synth.Program.Concrete.Program
  ( ProgPPrint (pformatProg),
    ProgToDot (toDotProg),
  )
import Grisette.Lib.Synth.Program.ProgCost (ProgCost (progCost))
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

data SumVarId l r = SumVarIdL l | SumVarIdR r deriving (Generic)

data SumOp l r = SumOpL l | SumOpR r deriving (Generic)

data SumStmt l r = SumStmtL l | SumStmtR r deriving (Generic)

deriveGADT [''SumVarId, ''SumOp, ''SumStmt] allClasses012

data SumProg l r
  = SumProgL l
  | SumProgR r
  deriving (Generic)

deriveGADT
  [''SumProg]
  (allClasses012 \\ (showClasses ++ pprintClasses))

instance
  {-# OVERLAPPABLE #-}
  (ToCon l c, ToCon r c) =>
  ToCon (SumProg l r) c
  where
  toCon (SumProgL l) = toCon l
  toCon (SumProgR r) = toCon r

instance Show2 SumProg where
  liftShowsPrec2 sp1 _ _ _ n (SumProgL l) = sp1 n l
  liftShowsPrec2 _ _ sp2 _ n (SumProgR r) = sp2 n r

instance (Show l) => Show1 (SumProg l) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show l, Show r) => Show (SumProg l r) where
  showsPrec = showsPrec1

instance PPrint2 SumProg where
  liftPFormatPrec2 pp1 _ _ _ n (SumProgL l) = pp1 n l
  liftPFormatPrec2 _ _ pp2 _ n (SumProgR r) = pp2 n r

instance (PPrint l) => PPrint1 (SumProg l) where
  liftPFormatPrec = liftPFormatPrec2 pformatPrec pformatList

instance (PPrint l, PPrint r) => PPrint (SumProg l r) where
  pformatPrec = pformatPrec1

instance
  ( ProgSemantics semObj l val ctx,
    ProgSemantics semObj r val ctx,
    ProgTypeType l ~ ProgTypeType r
  ) =>
  ProgSemantics semObj (SumProg l r) val ctx
  where
  runProg semObj table (SumProgL l) = runProg semObj table l
  runProg semObj table (SumProgR r) = runProg semObj table r

instance
  ( ProgTyping l,
    ProgTyping r,
    ProgTypeType l ~ ProgTypeType r
  ) =>
  ProgTyping (SumProg l r)
  where
  typeProg (SumProgL l) = typeProg l
  typeProg (SumProgR r) = typeProg r

instance
  ( ProgPPrint l,
    ProgPPrint r,
    ProgTypeType l ~ ProgTypeType r
  ) =>
  ProgPPrint (SumProg l r)
  where
  pformatProg key (SumProgL l) = pformatProg key l
  pformatProg key (SumProgR r) = pformatProg key r

instance
  ( ProgToDot l,
    ProgToDot r,
    ProgTypeType l ~ ProgTypeType r
  ) =>
  ProgToDot (SumProg l r)
  where
  toDotProg key (SumProgL l) = toDotProg key l
  toDotProg key (SumProgR r) = toDotProg key r

instance
  ( StmtUtil l,
    StmtUtil r,
    lop ~ StmtOpType l,
    rop ~ StmtOpType r,
    lid ~ StmtVarIdType l,
    rid ~ StmtVarIdType r
  ) =>
  StmtUtilImpl (SumStmt l r) (SumOp lop rop) (SumVarId lid rid)
  where
  getStmtArgIds (SumStmtL l) = SumVarIdL <$> getStmtArgIds l
  getStmtArgIds (SumStmtR r) = SumVarIdR <$> getStmtArgIds r
  getStmtResIds (SumStmtL l) = SumVarIdL <$> getStmtResIds l
  getStmtResIds (SumStmtR r) = SumVarIdR <$> getStmtResIds r
  getStmtOp (SumStmtL l) = SumOpL $ getStmtOp l
  getStmtOp (SumStmtR r) = SumOpR $ getStmtOp r
  getStmtDisabled (SumStmtL l) = getStmtDisabled l
  getStmtDisabled (SumStmtR r) = getStmtDisabled r

instance (StmtUtil l, StmtUtil r) => StmtUtil (SumStmt l r) where
  type
    StmtVarIdType (SumStmt l r) =
      SumVarId (StmtVarIdType l) (StmtVarIdType r)
  type
    StmtOpType (SumStmt l r) =
      SumOp (StmtOpType l) (StmtOpType r)

instance
  ( ProgUtil l,
    ProgUtil r,
    lop ~ ProgOpType l,
    rop ~ ProgOpType r,
    lid ~ ProgVarIdType l,
    rid ~ ProgVarIdType r,
    lstmt ~ ProgStmtType l,
    rstmt ~ ProgStmtType r,
    ProgTypeType l ~ ProgTypeType r
  ) =>
  ProgUtilImpl
    (SumProg l r)
    (SumOp lop rop)
    (SumStmt lstmt rstmt)
    (SumVarId lid rid)
  where
  getProgArgIds (SumProgL l) = SumVarIdL <$> getProgArgIds l
  getProgArgIds (SumProgR r) = SumVarIdR <$> getProgArgIds r
  getProgResIds (SumProgL l) = SumVarIdL <$> getProgResIds l
  getProgResIds (SumProgR r) = SumVarIdR <$> getProgResIds r
  getProgNumStmts (SumProgL l) = getProgNumStmts l
  getProgNumStmts (SumProgR r) = getProgNumStmts r
  getProgStmtAtIdx (SumProgL l) i = SumStmtL <$> getProgStmtAtIdx l i
  getProgStmtAtIdx (SumProgR r) i = SumStmtR <$> getProgStmtAtIdx r i

instance
  (ProgUtil l, ProgUtil r, ProgTypeType l ~ ProgTypeType r) =>
  ProgUtil (SumProg l r)
  where
  type ProgTypeType (SumProg l r) = ProgTypeType l
  type
    ProgStmtType (SumProg l r) =
      SumStmt (ProgStmtType l) (ProgStmtType r)
  type
    ProgVarIdType (SumProg l r) =
      SumVarId (ProgVarIdType l) (ProgVarIdType r)
  type
    ProgOpType (SumProg l r) =
      SumOp (ProgOpType l) (ProgOpType r)

instance
  (ProgCost costObj prog0 cost ctx, ProgCost costObj prog1 cost ctx) =>
  ProgCost costObj (SumProg prog0 prog1) cost ctx
  where
  progCost costObj table (SumProgL l) = progCost costObj table l
  progCost costObj table (SumProgR r) = progCost costObj table r
