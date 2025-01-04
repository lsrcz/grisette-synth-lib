{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Reasoning.Parallel.NodeStatus
  ( NodeStatus (..),
    NodeAction (..),
    nodeStatusIsRunning,
    nodeStatusIsNotYetStarted,
    nodeStatusBestProgWithCost,
    nodeStatusIsFastTrackViable,
    nodeStatusIsFastTrackRefining,
    nodeStatusIsFastTrackEasySynthFailure,
    nodeStatusIsSlowTrackRefining,
    nodeStatusIsFastSuccess,
    nodeStatusIsSlowSuccess,
    nodeStatusIsFastUnsat,
    nodeStatusIsSlowUnsat,
    nodeStatusIsFastUnknown,
    nodeStatusIsSlowUnknown,
    nodeStatusIsFastTerminated,
    nodeStatusIsSlowTerminated,
    nodeStatusIsInferredFailure,
    nodeStatusIsJustStarted,
    nodeStatusInferFailureTransition,
    nodeStatusTransition,
  )
where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette (PPrint (pformat), SolvingFailure (Unsat), nest, vsep)
import Grisette.Lib.Synth.Program.Concrete (ProgPPrint)
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable)
import Grisette.Lib.Synth.Reasoning.Parallel.Process
  ( Message
      ( Failure,
        FastTrackEasySynthFailure,
        FastTrackViable,
        GotExample,
        Success
      ),
    ProcessResponse,
    Track (FastTrack, SlowTrack),
  )
import Grisette.Lib.Synth.Util.Show (showText)

data NodeStatus conProg
  = NodeFastTrackViable
      { _curTrack :: Track,
        _viableProg :: SymbolTable conProg
      }
  | NodeFastTrackEasySynthFailure -- initial examples
  | NodeFastTrackRefining {_cost :: Int, _prog :: SymbolTable conProg}
  | NodeSlowTrackRefining {_cost :: Int, _prog :: SymbolTable conProg}
  | NodeSucceeded
      { _lastIterErrorMsg :: Maybe T.Text,
        _track :: Track,
        _cost :: Int,
        _prog :: SymbolTable conProg
      }
  | NodeFailed {_fastTrackViable :: Bool, _failure :: SolvingFailure}
  | NodeTerminated {_fastTrackViable :: Bool, _errorMsg :: T.Text}
  | NodeInferredFailure
  | NodeStarted
  | NodeNotYetStarted
  deriving (Eq, Show, Generic)

instance (ProgPPrint conProg) => PPrint (NodeStatus conProg) where
  pformat NodeInferredFailure = "NodeInferredFailure"
  pformat (NodeTerminated fastTrackViable t) =
    "NodeTerminated "
      <> (if fastTrackViable then "(fast track viable) " else "")
      <> pformat t
  pformat NodeNotYetStarted = "NodeNotYetStarted"
  pformat (NodeFastTrackRefining cost m) =
    nest 2 $
      vsep
        [ "NodeFastTrackRefining (cost "
            <> pformat cost
            <> ")",
          pformat m
        ]
  pformat NodeFastTrackEasySynthFailure =
    nest 2 $
      vsep
        [ "NodeFastTrackEasySynthFailure"
        ]
  pformat (NodeSlowTrackRefining cost m) =
    nest 2 $
      vsep
        [ "NodeSlowTrackRefining (cost "
            <> pformat cost,
          pformat m
        ]
  pformat (NodeFastTrackViable track m) =
    nest 2 $
      vsep
        [ "NodeFastTrackViable ",
          "Next track is " <> pformat track,
          pformat m
        ]
  pformat (NodeSucceeded maybeCrashMessage track cost m) =
    nest 2 $
      vsep $
        concat
          [ [ "NodeSucceeded (cost "
                <> pformat cost
                <> ", track"
                <> pformat track
                <> ")"
            ],
            case maybeCrashMessage of
              Nothing -> []
              Just msg -> ["Not finished because " <> pformat msg],
            [pformat m]
          ]
  pformat (NodeFailed fastTrackViable r) =
    "NodeFailed "
      <> (if fastTrackViable then "(fast track viable) " else "")
      <> pformat r
  pformat NodeStarted =
    "NodeStarted"

nodeStatusIsRunning :: NodeStatus conProg -> Bool
nodeStatusIsRunning NodeStarted {} = True
nodeStatusIsRunning NodeFastTrackViable {} = True
nodeStatusIsRunning NodeFastTrackRefining {} = True
nodeStatusIsRunning NodeFastTrackEasySynthFailure {} = True
nodeStatusIsRunning NodeSlowTrackRefining {} = True
nodeStatusIsRunning _ = False

nodeStatusIsNotYetStarted :: NodeStatus conProg -> Bool
nodeStatusIsNotYetStarted NodeNotYetStarted = True
nodeStatusIsNotYetStarted _ = False

nodeStatusBestProgWithCost :: NodeStatus conProg -> Maybe (Int, SymbolTable conProg)
nodeStatusBestProgWithCost NodeFastTrackViable {} = Nothing
nodeStatusBestProgWithCost (NodeFastTrackRefining cost m) = Just (cost, m)
nodeStatusBestProgWithCost NodeFastTrackEasySynthFailure = Nothing
nodeStatusBestProgWithCost (NodeSlowTrackRefining cost m) = Just (cost, m)
nodeStatusBestProgWithCost (NodeSucceeded _ _ cost m) = Just (cost, m)
nodeStatusBestProgWithCost NodeFailed {} = Nothing
nodeStatusBestProgWithCost (NodeTerminated _ _) = Nothing
nodeStatusBestProgWithCost NodeInferredFailure = Nothing
nodeStatusBestProgWithCost NodeStarted {} = Nothing
nodeStatusBestProgWithCost NodeNotYetStarted = Nothing

nodeStatusIsFastTrackViable :: NodeStatus conProg -> Bool
nodeStatusIsFastTrackViable NodeFastTrackViable {} = True
nodeStatusIsFastTrackViable _ = False

nodeStatusIsFastTrackRefining :: NodeStatus conProg -> Bool
nodeStatusIsFastTrackRefining NodeFastTrackRefining {} = True
nodeStatusIsFastTrackRefining _ = False

nodeStatusIsFastTrackEasySynthFailure :: NodeStatus conProg -> Bool
nodeStatusIsFastTrackEasySynthFailure NodeFastTrackEasySynthFailure {} = True
nodeStatusIsFastTrackEasySynthFailure _ = False

nodeStatusIsSlowTrackRefining :: NodeStatus conProg -> Bool
nodeStatusIsSlowTrackRefining NodeSlowTrackRefining {} = True
nodeStatusIsSlowTrackRefining _ = False

nodeStatusIsFastSuccess :: NodeStatus conProg -> Bool
nodeStatusIsFastSuccess NodeSucceeded {_track = FastTrack} = True
nodeStatusIsFastSuccess _ = False

nodeStatusIsSlowSuccess :: NodeStatus conProg -> Bool
nodeStatusIsSlowSuccess NodeSucceeded {_track = SlowTrack} = True
nodeStatusIsSlowSuccess _ = False

nodeStatusIsFastUnsat :: NodeStatus conProg -> Bool
nodeStatusIsFastUnsat (NodeFailed False Unsat) = True
nodeStatusIsFastUnsat _ = False

nodeStatusIsSlowUnsat :: NodeStatus conProg -> Bool
nodeStatusIsSlowUnsat (NodeFailed True Unsat) = True
nodeStatusIsSlowUnsat _ = False

nodeStatusIsFastUnknown :: NodeStatus conProg -> Bool
nodeStatusIsFastUnknown (NodeFailed _ Unsat) = False
nodeStatusIsFastUnknown (NodeFailed False _) = True
nodeStatusIsFastUnknown _ = False

nodeStatusIsSlowUnknown :: NodeStatus conProg -> Bool
nodeStatusIsSlowUnknown (NodeFailed _ Unsat) = False
nodeStatusIsSlowUnknown (NodeFailed True _) = True
nodeStatusIsSlowUnknown _ = False

nodeStatusIsFastTerminated :: NodeStatus conProg -> Bool
nodeStatusIsFastTerminated (NodeTerminated False _) = True
nodeStatusIsFastTerminated _ = False

nodeStatusIsSlowTerminated :: NodeStatus conProg -> Bool
nodeStatusIsSlowTerminated (NodeTerminated True _) = True
nodeStatusIsSlowTerminated _ = False

nodeStatusIsInferredFailure :: NodeStatus conProg -> Bool
nodeStatusIsInferredFailure NodeInferredFailure = True
nodeStatusIsInferredFailure _ = False

nodeStatusIsJustStarted :: NodeStatus conProg -> Bool
nodeStatusIsJustStarted NodeStarted {} = True
nodeStatusIsJustStarted _ = False

data NodeAction
  = Refine {_succeed :: Bool, _nodeBestCostKnowledge :: Maybe Int}
  | RefineAndSplitSketch {_nodeBestCostKnowledge :: Maybe Int}
  | CleanUpAndSplitSketch
  | MarkFailure

_ensureNewCostIsSmaller :: Int -> Int -> a -> a
_ensureNewCostIsSmaller newCost oldCost x =
  if newCost >= oldCost
    then error "Should not happen"
    else x

nodeStatusInferFailureTransition :: NodeStatus conProg -> NodeStatus conProg
nodeStatusInferFailureTransition NodeFailed {} =
  error "Should not happen" -- status
nodeStatusInferFailureTransition status@NodeInferredFailure {} = status
nodeStatusInferFailureTransition NodeTerminated {} = NodeInferredFailure
nodeStatusInferFailureTransition (NodeSucceeded _ track cost prog) =
  NodeSucceeded Nothing track cost prog
nodeStatusInferFailureTransition (NodeFastTrackViable {}) =
  NodeInferredFailure
nodeStatusInferFailureTransition NodeFastTrackEasySynthFailure =
  NodeInferredFailure
nodeStatusInferFailureTransition
  (NodeFastTrackRefining cost prog) =
    NodeSucceeded Nothing FastTrack cost prog
nodeStatusInferFailureTransition
  (NodeSlowTrackRefining cost prog) =
    NodeSucceeded Nothing SlowTrack cost prog
nodeStatusInferFailureTransition NodeStarted {} = NodeInferredFailure
nodeStatusInferFailureTransition NodeNotYetStarted = NodeInferredFailure

nodeStatusTransition ::
  NodeStatus conProg ->
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher ->
  (NodeStatus conProg, NodeAction)
nodeStatusTransition NodeSucceeded {} = error "Should not happen"
nodeStatusTransition NodeFailed {} = error "Should not happen"
nodeStatusTransition NodeInferredFailure = error "Should not happen"
nodeStatusTransition NodeTerminated {} = error "Should not happen"
nodeStatusTransition NodeNotYetStarted = error "Shouldnot happen"
nodeStatusTransition NodeStarted = nodeStartedTransition
nodeStatusTransition (NodeFastTrackViable FastTrack oldProg) =
  nodeFastTrackViableFastTrackTransition oldProg
nodeStatusTransition (NodeFastTrackViable SlowTrack oldProg) =
  nodeFastTrackViableSlowTrackTransition oldProg
nodeStatusTransition NodeFastTrackEasySynthFailure =
  nodeFastTrackEasySynthFailureTransition
nodeStatusTransition (NodeFastTrackRefining oldCost oldProg) =
  nodeFastTrackRefiningTransition oldCost oldProg
nodeStatusTransition (NodeSlowTrackRefining oldCost oldProg) =
  nodeSlowTrackRefiningTransition oldCost oldProg

nodeStartedTransition ::
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher ->
  (NodeStatus conProg, NodeAction)
nodeStartedTransition (Left msg) =
  (NodeTerminated False msg, CleanUpAndSplitSketch)
nodeStartedTransition (Right (Failure _ failure)) =
  (NodeFailed False failure, MarkFailure)
nodeStartedTransition (Right (FastTrackViable FastTrack _ cost prog)) =
  (NodeFastTrackViable FastTrack prog, RefineAndSplitSketch cost)
nodeStartedTransition (Right (FastTrackViable SlowTrack _ cost prog)) =
  (NodeFastTrackViable SlowTrack prog, RefineAndSplitSketch cost)
nodeStartedTransition (Right (FastTrackEasySynthFailure _ _)) =
  error "Should not happen"
nodeStartedTransition (Right (Success FastTrack _ _ _)) =
  error "Should not happen"
nodeStartedTransition (Right (Success SlowTrack _ cost prog)) =
  (NodeSlowTrackRefining cost prog, RefineAndSplitSketch $ Just cost)
nodeStartedTransition (Right (GotExample _ cost)) =
  (NodeStarted, Refine False cost)

nodeFastTrackViableFastTrackTransition ::
  SymbolTable conProg ->
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher ->
  (NodeStatus conProg, NodeAction)
nodeFastTrackViableFastTrackTransition _ (Left msg) =
  (NodeTerminated True msg, CleanUpAndSplitSketch)
nodeFastTrackViableFastTrackTransition _ (Right (Failure _ failure)) =
  (NodeFailed True failure, MarkFailure)
nodeFastTrackViableFastTrackTransition _ (Right FastTrackViable {}) =
  error "Should not happen"
nodeFastTrackViableFastTrackTransition
  _
  (Right (FastTrackEasySynthFailure cost _)) =
    (NodeFastTrackEasySynthFailure, Refine True cost)
nodeFastTrackViableFastTrackTransition
  _
  (Right (Success FastTrack _ cost newProg)) =
    (NodeFastTrackRefining cost newProg, Refine True $ Just cost)
nodeFastTrackViableFastTrackTransition _ (Right (Success SlowTrack _ _ _)) =
  error "Should not happen"
nodeFastTrackViableFastTrackTransition oldProg (Right (GotExample _ cost)) =
  (NodeFastTrackViable FastTrack oldProg, Refine True cost)

nodeFastTrackViableSlowTrackTransition ::
  SymbolTable conProg ->
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher ->
  (NodeStatus conProg, NodeAction)
nodeFastTrackViableSlowTrackTransition _ (Left msg) =
  (NodeTerminated True msg, CleanUpAndSplitSketch)
nodeFastTrackViableSlowTrackTransition _ (Right (Failure _ failure)) =
  (NodeFailed True failure, MarkFailure)
nodeFastTrackViableSlowTrackTransition _ (Right FastTrackViable {}) =
  error "Should not happen"
nodeFastTrackViableSlowTrackTransition
  _
  (Right (FastTrackEasySynthFailure cost _)) =
    (NodeFastTrackEasySynthFailure, Refine True cost)
nodeFastTrackViableSlowTrackTransition
  _
  (Right (Success FastTrack _ cost prog)) =
    (NodeFastTrackRefining cost prog, Refine True $ Just cost)
nodeFastTrackViableSlowTrackTransition
  _
  (Right (Success SlowTrack _ cost prog)) =
    (NodeSlowTrackRefining cost prog, Refine True $ Just cost)
nodeFastTrackViableSlowTrackTransition
  oldProg
  (Right (GotExample _ bestKnownCost)) =
    (NodeFastTrackViable SlowTrack oldProg, Refine True bestKnownCost)

nodeFastTrackEasySynthFailureTransition ::
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher ->
  (NodeStatus conProg, NodeAction)
nodeFastTrackEasySynthFailureTransition (Left msg) =
  (NodeTerminated True msg, CleanUpAndSplitSketch)
nodeFastTrackEasySynthFailureTransition (Right (Failure _ failure)) =
  (NodeFailed True failure, MarkFailure)
nodeFastTrackEasySynthFailureTransition (Right (FastTrackViable {})) =
  error "Should not happen"
nodeFastTrackEasySynthFailureTransition
  (Right (FastTrackEasySynthFailure _ _)) =
    error "Should not happen"
nodeFastTrackEasySynthFailureTransition (Right (Success FastTrack _ _ _)) =
  error "Should not happen"
nodeFastTrackEasySynthFailureTransition
  (Right (Success SlowTrack _ cost prog)) =
    (NodeSlowTrackRefining cost prog, Refine True $ Just cost)
nodeFastTrackEasySynthFailureTransition (Right (GotExample _ bestKnownCost)) =
  (NodeFastTrackEasySynthFailure, Refine True bestKnownCost)

nodeFastTrackRefiningTransition ::
  Int ->
  SymbolTable conProg ->
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher ->
  (NodeStatus conProg, NodeAction)
nodeFastTrackRefiningTransition oldCost oldProg (Left msg) =
  (NodeSucceeded (Just msg) FastTrack oldCost oldProg, CleanUpAndSplitSketch)
nodeFastTrackRefiningTransition oldCost oldProg (Right (Failure _ Unsat)) =
  (NodeSucceeded Nothing FastTrack oldCost oldProg, MarkFailure)
nodeFastTrackRefiningTransition oldCost oldProg (Right (Failure _ failure)) =
  ( NodeSucceeded
      (Just $ "Solver finally failed with: " <> showText failure)
      FastTrack
      oldCost
      oldProg,
    CleanUpAndSplitSketch
  )
nodeFastTrackRefiningTransition
  oldCost
  oldProg
  (Right (FastTrackViable FastTrack _ bestKnownCost _)) =
    (NodeFastTrackRefining oldCost oldProg, Refine True bestKnownCost)
nodeFastTrackRefiningTransition _ _ (Right (FastTrackViable SlowTrack _ _ _)) =
  error "Should not happen"
nodeFastTrackRefiningTransition
  oldCost
  oldProg
  (Right (FastTrackEasySynthFailure bestKnownCost _)) =
    (NodeSlowTrackRefining oldCost oldProg, Refine True bestKnownCost)
nodeFastTrackRefiningTransition
  oldCost
  _
  (Right (Success FastTrack _ newCost newProg)) =
    _ensureNewCostIsSmaller
      newCost
      oldCost
      (NodeFastTrackRefining newCost newProg, Refine True $ Just newCost)
nodeFastTrackRefiningTransition _ _ (Right (Success SlowTrack _ _ _)) =
  error "Should not happen"
nodeFastTrackRefiningTransition
  oldCost
  oldProg
  (Right (GotExample _ bestKnownCost)) =
    (NodeFastTrackRefining oldCost oldProg, Refine True bestKnownCost)

nodeSlowTrackRefiningTransition ::
  Int ->
  SymbolTable conProg ->
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher ->
  (NodeStatus conProg, NodeAction)
nodeSlowTrackRefiningTransition oldCost oldProg (Left msg) =
  (NodeSucceeded (Just msg) SlowTrack oldCost oldProg, CleanUpAndSplitSketch)
nodeSlowTrackRefiningTransition oldCost oldProg (Right (Failure _ Unsat)) =
  (NodeSucceeded Nothing SlowTrack oldCost oldProg, MarkFailure)
nodeSlowTrackRefiningTransition oldCost oldProg (Right (Failure _ failure)) =
  ( NodeSucceeded
      (Just $ "Solver finally failed with: " <> showText failure)
      SlowTrack
      oldCost
      oldProg,
    CleanUpAndSplitSketch
  )
nodeSlowTrackRefiningTransition _ _ (Right (FastTrackViable {})) =
  error "Should not happen"
nodeSlowTrackRefiningTransition _ _ (Right (FastTrackEasySynthFailure {})) =
  error "Should not happen"
nodeSlowTrackRefiningTransition _ _ (Right (Success FastTrack _ _ _)) =
  error "Should not happen"
nodeSlowTrackRefiningTransition
  oldCost
  _
  (Right (Success SlowTrack _ newCost newProg)) =
    _ensureNewCostIsSmaller
      newCost
      oldCost
      (NodeSlowTrackRefining newCost newProg, Refine True $ Just newCost)
nodeSlowTrackRefiningTransition
  oldCost
  oldProg
  (Right (GotExample _ bestKnownCost)) =
    (NodeSlowTrackRefining oldCost oldProg, Refine True bestKnownCost)
