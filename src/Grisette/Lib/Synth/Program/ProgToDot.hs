module Grisette.Lib.Synth.Program.ProgToDot (ProgToDot (..), progToDot) where

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
import Data.GraphViz.Printing (DotCode)
import qualified Data.Map.Ordered as OM
import qualified Data.Text as T

class ProgToDot prog where
  topologicalProgToDot ::
    prog ->
    OM.OMap T.Text (DotSubGraph T.Text) ->
    OM.OMap T.Text (DotSubGraph T.Text)

progToDot :: (ProgToDot prog) => prog -> DotCode
progToDot prog = unqtDot dotGraph
  where
    allDots = topologicalProgToDot prog OM.empty
    dotGraph =
      DotGraph
        { strictGraph = False,
          directedGraph = True,
          graphID = Nothing,
          graphStatements =
            DotStmts
              { attrStmts = [],
                subGraphs = snd <$> OM.assocs allDots,
                nodeStmts = [],
                edgeStmts = []
              }
        }
