{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Lib.Synth.Program.ProgCost
  ( ProgCost (..),
  )
where

import Grisette.Lib.Synth.Context (MonadContext)

class (MonadContext ctx) => ProgCost costObj prog cost ctx where
  progCost :: costObj -> prog -> ctx cost
