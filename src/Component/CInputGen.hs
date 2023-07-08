{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Component.CInputGen where

import Test.QuickCheck

class CInputGen gen c where
  cInputGen :: gen -> Int -> Gen [c]

newtype SizedCGen c = SizedCGen
  { unSizedCGen :: Gen [c]
  }

instance CInputGen (SizedCGen c) c where
  cInputGen (SizedCGen g) i = resize i g

newtype SimpleCGen c = SimpleCGen
  { unSimpleCGen :: Int -> Gen [c]
  }

instance CInputGen (SimpleCGen c) c where
  cInputGen = unSimpleCGen

newtype UnsizedCGen c = UnsizedCGen
  { unUnsizedCGen :: Gen [c]
  }

instance CInputGen (UnsizedCGen c) c where
  cInputGen (UnsizedCGen g) _ = g
