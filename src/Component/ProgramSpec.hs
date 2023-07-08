{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module Component.ProgramSpec where

import Data.Maybe
import Grisette

class CSpec spec e c | spec -> c e where
  cspec :: spec -> [c] -> Either e [c] -> Bool

newtype PlainCSpec e c = PlainCSpec
  { unPlainCSpec :: [c] -> Either e [c] -> Bool
  }

instance CSpec (PlainCSpec e c) e c where
  cspec = unPlainCSpec

class SSpec spec e s | spec -> e s where
  sspec :: spec -> [s] -> Either e [s] -> SymBool

newtype PlainSSpec e s = PlainSSpec
  { unPlainSSpec :: [s] -> Either e [s] -> SymBool
  }

instance SSpec (PlainSSpec e s) e s where
  sspec = unPlainSSpec

data PlainUSpec e s ce c where
  PlainUSpec ::
    (ToSym c s, ToCon s c, ToSym ce e, ToCon e ce) =>
    { unPlainUSpec :: [s] -> Either e [s] -> SymBool
    } ->
    PlainUSpec e s ce c

instance CSpec (PlainUSpec e s ce c) ce c where
  cspec (PlainUSpec s) cin cres =
    fromJust $ toCon $ s (toSym cin) (toSym cres)

instance SSpec (PlainUSpec e s ce c) e s where
  sspec = unPlainUSpec
