{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Program.ProgToDot (ProgToDot (..)) where

import Data.GraphViz
  ( DotGraph
      ( DotGraph,
        directedGraph,
        graphID,
        graphStatements,
        strictGraph
      ),
    DotStatements (DotStmts, attrStmts, edgeStmts, nodeStmts, subGraphs),
    DotSubGraph,
    PrintDot (unqtDot),
  )
import qualified Data.Text as T
import Grisette.Lib.Synth.Program.ProgTyping
  ( ProgTypeTable,
    ProgTyping,
    typeSymbolTable,
  )
import Grisette.Lib.Synth.Program.ProgUtil (ProgUtil (ProgTypeType))
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable (SymbolTable))

class ProgToDot prog where
  toDotProg ::
    ProgTypeTable (ProgTypeType prog) ->
    prog ->
    DotSubGraph T.Text

instance
  (ProgToDot prog, ProgUtil prog, ProgTyping prog) =>
  PrintDot (SymbolTable prog)
  where
  unqtDot table@(SymbolTable lst) =
    let tyTable = typeSymbolTable table
     in unqtDot $
          DotGraph
            { strictGraph = False,
              directedGraph = True,
              graphID = Nothing,
              graphStatements =
                DotStmts
                  { attrStmts = [],
                    subGraphs = toDotProg tyTable . snd <$> lst,
                    nodeStmts = [],
                    edgeStmts = []
                  }
            }
