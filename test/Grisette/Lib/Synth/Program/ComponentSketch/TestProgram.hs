{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.ComponentSketch.TestProgram
  ( goodConcreteProg,
  )
where

import Grisette (Solvable (con), SymInteger)
import Grisette.Lib.Synth.Program.ComponentSketch
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import Grisette.Lib.Synth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add, DivMod),
    TestSemanticsType (IntType),
  )

goodConcreteProg :: Prog TestSemanticsOp SymInteger TestSemanticsType
goodConcreteProg =
  Prog
    "test"
    [ProgArg "x" IntType, ProgArg "y" IntType]
    [ Stmt Add [0, 1] 2 [2] 1 $ con False,
      Stmt DivMod [2, 0] 2 [3, 4] 2 $ con False
    ]
    [ProgRes 3 IntType, ProgRes 4 IntType]
