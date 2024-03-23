{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.Program.ProgConstraints
  ( ProgConstraints (..),
    WithConstraints (..),
  )
where

import Grisette (mrgReturn)
import Grisette.Lib.Synth.Context (MonadContext)

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
