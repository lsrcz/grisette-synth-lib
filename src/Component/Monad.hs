module Component.Monad where

import Control.Monad.Except
import Grisette

type M e v = FreshT (ExceptT e UnionM) v
