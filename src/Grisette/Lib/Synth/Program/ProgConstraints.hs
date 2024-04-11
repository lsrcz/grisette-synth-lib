{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.Program.ProgConstraints
  ( ProgConstraints (..),
    WithConstraints (..),
    runProgWithConstraints,
    OpSubProgConstraints (..),
  )
where

import Grisette (Mergeable, MonadUnion, UnionM, liftUnionM, mrgReturn)
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Program.ProgSemantics (ProgSemantics (runProg))

class (MonadContext ctx) => ProgConstraints constObj prog ctx where
  constrainProg :: constObj -> prog -> ctx ()

instance (MonadContext ctx) => ProgConstraints () prog ctx where
  constrainProg _ _ = mrgReturn ()

instance
  ( ProgConstraints constObj1 prog ctx,
    ProgConstraints constObj2 prog ctx
  ) =>
  ProgConstraints (constObj1, constObj2) prog ctx
  where
  constrainProg (obj1, obj2) prog = do
    constrainProg obj1 prog
    constrainProg obj2 prog

instance
  ( ProgConstraints constObj1 prog ctx,
    ProgConstraints constObj2 prog ctx,
    ProgConstraints constObj3 prog ctx
  ) =>
  ProgConstraints (constObj1, constObj2, constObj3) prog ctx
  where
  constrainProg (obj1, obj2, obj3) = constrainProg (obj1, (obj2, obj3))

instance
  ( ProgConstraints constObj1 prog ctx,
    ProgConstraints constObj2 prog ctx,
    ProgConstraints constObj3 prog ctx,
    ProgConstraints constObj4 prog ctx
  ) =>
  ProgConstraints (constObj1, constObj2, constObj3, constObj4) prog ctx
  where
  constrainProg (obj1, obj2, obj3, obj4) =
    constrainProg (obj1, (obj2, obj3, obj4))

data WithConstraints semObj constObj = WithConstraints semObj constObj

runProgWithConstraints ::
  (ProgConstraints constObj prog ctx, ProgSemantics semObj prog val ctx) =>
  WithConstraints semObj constObj ->
  prog ->
  [val] ->
  ctx [val]
runProgWithConstraints (WithConstraints semObj constObj) prog inputs = do
  constrainProg constObj prog
  runProg semObj prog inputs

class (MonadContext ctx) => OpSubProgConstraints constObj op ctx where
  constrainOpSubProg :: constObj -> op -> ctx ()
  constrainOpSubProg _ _ = mrgReturn ()

instance
  (MonadUnion ctx, OpSubProgConstraints constObj op ctx, Mergeable op) =>
  OpSubProgConstraints constObj (UnionM op) ctx
  where
  constrainOpSubProg constObj op =
    liftUnionM op >>= constrainOpSubProg constObj
