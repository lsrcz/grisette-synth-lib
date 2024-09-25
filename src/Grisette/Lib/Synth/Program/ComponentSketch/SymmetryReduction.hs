module Grisette.Lib.Synth.Program.ComponentSketch.SymmetryReduction
  ( OpSymmetryReduction (..),
  )
where

import Grisette (SymBool, Union, simpleMerge)

class OpSymmetryReduction op where
  opUnreorderable :: op -> op -> SymBool

instance (OpSymmetryReduction op) => OpSymmetryReduction (Union op) where
  opUnreorderable op1 op2 = simpleMerge $ do
    o1 <- op1
    opUnreorderable o1 <$> op2
