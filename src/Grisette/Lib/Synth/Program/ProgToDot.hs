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
import Grisette.Lib.Synth.Program.ProgTyping (ProgTyping)
import Grisette.Lib.Synth.Program.ProgUtil (ProgUtil)
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable (SymbolTable))

class ProgToDot prog where
  toDotProg :: prog -> DotSubGraph T.Text

instance
  (ProgToDot prog, ProgUtil prog, ProgTyping prog) =>
  PrintDot (SymbolTable prog)
  where
  unqtDot (SymbolTable lst) =
    unqtDot $
      DotGraph
        { strictGraph = False,
          directedGraph = True,
          graphID = Nothing,
          graphStatements =
            DotStmts
              { attrStmts = [],
                subGraphs = toDotProg . snd <$> lst,
                nodeStmts = [],
                edgeStmts = []
              }
        }
