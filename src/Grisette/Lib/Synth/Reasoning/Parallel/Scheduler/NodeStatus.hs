{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.NodeStatus
  ( NodeStatus (..),
    NodeAction (..),
    nodeStatusIsRunning,
    nodeStatusIsNotYetStarted,
    nodeStatusBestProgWithCost,
    nodeStatusIsViable,
    nodeStatusIsRefining,
    nodeStatusIsSuccess,
    nodeStatusIsUnsat,
    nodeStatusIsUnknown,
    nodeStatusIsTerminated,
    nodeStatusIsInferredFailure,
    nodeStatusIsJustStarted,
    nodeStatusInferFailureTransition,
    nodeStatusTransition,
    pformatNodeStatusSummary,
    nodeStatusIsEnded,
    nodeStatusIsDetermined,
  )
where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette (Doc, PPrint (pformat), SolvingFailure (Unsat), nest, vsep)
import Grisette.Lib.Synth.Program.Concrete (ProgPPrint)
import Grisette.Lib.Synth.Program.SymbolTable (SymbolTable)
import Grisette.Lib.Synth.Reasoning.Parallel.Scheduler.Process
  ( Message
      ( EasySynthFailure,
        Failure,
        GotExample,
        Success,
        Viable
      ),
    ProcessResponse,
  )
import Grisette.Lib.Synth.Util.Show (showAsText)

data NodeStatus conProg
  = NodeViable
      { _curTrack :: Int,
        _viableProg :: SymbolTable conProg
      }
  | NodeRefining {_curTrack :: Int, _cost :: Int, _prog :: SymbolTable conProg}
  | NodeSucceeded
      { _lastIterErrorMsg :: Maybe T.Text,
        _cost :: Int,
        _prog :: SymbolTable conProg
      }
  | NodeFailed {_failure :: SolvingFailure}
  | NodeTerminated {_errorMsg :: T.Text}
  | NodeInferredFailure
  | NodeStarted
  | NodeNotYetStarted
  deriving (Eq, Show, Generic)

pformatNodeStatusSummary :: NodeStatus conProg -> Doc ann
pformatNodeStatusSummary NodeInferredFailure = "NodeInferredFailure"
pformatNodeStatusSummary (NodeTerminated _) =
  "NodeTerminated"
pformatNodeStatusSummary NodeNotYetStarted = "NodeNotYetStarted"
pformatNodeStatusSummary (NodeRefining track cost _) =
  "NodeRefining (track "
    <> pformat track
    <> ", cost "
    <> pformat cost
    <> ")"
pformatNodeStatusSummary (NodeViable track _) =
  "NodeViable (track: " <> pformat track <> ")"
pformatNodeStatusSummary (NodeSucceeded maybeCrashMessage cost _) =
  "NodeSucceeded (cost "
    <> pformat cost
    <> case maybeCrashMessage of
      Nothing -> ")"
      Just _ -> ", not finished)"
pformatNodeStatusSummary (NodeFailed r) =
  "NodeFailed "
    <> pformat r
pformatNodeStatusSummary NodeStarted =
  "NodeStarted"

instance (ProgPPrint conProg) => PPrint (NodeStatus conProg) where
  pformat NodeInferredFailure = "NodeInferredFailure"
  pformat (NodeTerminated t) =
    "NodeTerminated "
      <> pformat t
  pformat NodeNotYetStarted = "NodeNotYetStarted"
  pformat (NodeRefining track cost m) =
    nest 2 $
      vsep
        [ "NodeRefining (track "
            <> pformat track
            <> ", cost "
            <> pformat cost
            <> ")",
          pformat m
        ]
  pformat (NodeViable track m) =
    nest 2 $
      vsep
        [ "NodeViable (track "
            <> pformat track
            <> ")",
          pformat m
        ]
  pformat (NodeSucceeded maybeCrashMessage cost m) =
    nest 2 $
      vsep $
        concat
          [ [ "NodeSucceeded (cost "
                <> pformat cost
                <> ")"
            ],
            case maybeCrashMessage of
              Nothing -> []
              Just msg -> ["Not finished because " <> pformat msg],
            [pformat m]
          ]
  pformat (NodeFailed r) = "NodeFailed " <> pformat r
  pformat NodeStarted =
    "NodeStarted"

nodeStatusIsRunning :: NodeStatus conProg -> Bool
nodeStatusIsRunning NodeStarted {} = True
nodeStatusIsRunning NodeViable {} = True
nodeStatusIsRunning NodeRefining {} = True
nodeStatusIsRunning _ = False

nodeStatusIsDetermined :: NodeStatus conProg -> Bool
nodeStatusIsDetermined NodeSucceeded {} = True
nodeStatusIsDetermined NodeFailed {} = True
nodeStatusIsDetermined NodeInferredFailure = True
nodeStatusIsDetermined _ = False

nodeStatusIsNotYetStarted :: NodeStatus conProg -> Bool
nodeStatusIsNotYetStarted NodeNotYetStarted = True
nodeStatusIsNotYetStarted _ = False

nodeStatusBestProgWithCost ::
  NodeStatus conProg -> Maybe (Int, SymbolTable conProg)
nodeStatusBestProgWithCost NodeViable {} = Nothing
nodeStatusBestProgWithCost (NodeRefining _ cost m) = Just (cost, m)
nodeStatusBestProgWithCost (NodeSucceeded _ cost m) = Just (cost, m)
nodeStatusBestProgWithCost NodeFailed {} = Nothing
nodeStatusBestProgWithCost (NodeTerminated _) = Nothing
nodeStatusBestProgWithCost NodeInferredFailure = Nothing
nodeStatusBestProgWithCost NodeStarted {} = Nothing
nodeStatusBestProgWithCost NodeNotYetStarted = Nothing

nodeStatusIsViable :: NodeStatus conProg -> Bool
nodeStatusIsViable NodeViable {} = True
nodeStatusIsViable _ = False

nodeStatusIsRefining :: NodeStatus conProg -> Bool
nodeStatusIsRefining NodeRefining {} = True
nodeStatusIsRefining _ = False

nodeStatusIsSuccess :: NodeStatus conProg -> Bool
nodeStatusIsSuccess NodeSucceeded {} = True
nodeStatusIsSuccess _ = False

nodeStatusIsUnsat :: NodeStatus conProg -> Bool
nodeStatusIsUnsat (NodeFailed Unsat) = True
nodeStatusIsUnsat _ = False

nodeStatusIsUnknown :: NodeStatus conProg -> Bool
nodeStatusIsUnknown (NodeFailed Unsat) = False
nodeStatusIsUnknown (NodeFailed _) = True
nodeStatusIsUnknown _ = False

nodeStatusIsTerminated :: NodeStatus conProg -> Bool
nodeStatusIsTerminated (NodeTerminated _) = True
nodeStatusIsTerminated _ = False

nodeStatusIsInferredFailure :: NodeStatus conProg -> Bool
nodeStatusIsInferredFailure NodeInferredFailure = True
nodeStatusIsInferredFailure _ = False

nodeStatusIsJustStarted :: NodeStatus conProg -> Bool
nodeStatusIsJustStarted NodeStarted {} = True
nodeStatusIsJustStarted _ = False

nodeStatusIsEnded :: NodeStatus conProg -> Bool
nodeStatusIsEnded NodeSucceeded {} = True
nodeStatusIsEnded NodeFailed {} = True
nodeStatusIsEnded NodeTerminated {} = True
nodeStatusIsEnded NodeInferredFailure = True
nodeStatusIsEnded NodeViable {} = False
nodeStatusIsEnded NodeRefining {} = False
nodeStatusIsEnded NodeStarted = False
nodeStatusIsEnded NodeNotYetStarted = False

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
nodeStatusInferFailureTransition (NodeSucceeded _ cost prog) =
  NodeSucceeded Nothing cost prog
nodeStatusInferFailureTransition (NodeViable {}) = NodeInferredFailure
nodeStatusInferFailureTransition (NodeRefining _ cost prog) =
  NodeSucceeded Nothing cost prog
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
nodeStatusTransition oldStatus@(NodeViable {}) = nodeViableTransition oldStatus
nodeStatusTransition NodeRefining {..} =
  nodeRefiningTransition _curTrack _cost _prog

nodeStartedTransition ::
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher ->
  (NodeStatus conProg, NodeAction)
nodeStartedTransition (Left msg) =
  (NodeTerminated msg, CleanUpAndSplitSketch)
nodeStartedTransition (Right (Failure _ failure)) =
  (NodeFailed failure, MarkFailure)
nodeStartedTransition (Right (Viable curTrack _ cost prog)) =
  (NodeViable curTrack prog, RefineAndSplitSketch cost)
nodeStartedTransition (Right EasySynthFailure {}) =
  error "Should not happen"
nodeStartedTransition (Right (Success _ curTrack _ cost prog)) =
  (NodeRefining curTrack cost prog, RefineAndSplitSketch $ Just cost)
nodeStartedTransition (Right (GotExample _ cost)) =
  (NodeStarted, Refine False cost)

nodeViableTransition ::
  NodeStatus conProg ->
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher ->
  (NodeStatus conProg, NodeAction)
nodeViableTransition _ (Left msg) =
  (NodeTerminated msg, CleanUpAndSplitSketch)
nodeViableTransition _ (Right (Failure _ failure)) =
  (NodeFailed failure, MarkFailure)
nodeViableTransition _ (Right (Viable curTrack _ cost prog)) =
  (NodeViable curTrack prog, RefineAndSplitSketch cost)
nodeViableTransition oldStatus (Right (EasySynthFailure _ cost _)) =
  (oldStatus, RefineAndSplitSketch cost)
nodeViableTransition _ (Right (Success _ curTrack _ cost prog)) =
  (NodeRefining curTrack cost prog, Refine True $ Just cost)
nodeViableTransition oldStatus (Right (GotExample _ cost)) =
  (oldStatus, Refine False cost)

nodeRefiningTransition ::
  Int ->
  Int ->
  SymbolTable conProg ->
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher ->
  (NodeStatus conProg, NodeAction)
nodeRefiningTransition _ oldCost oldProg (Left msg) =
  (NodeSucceeded (Just msg) oldCost oldProg, CleanUpAndSplitSketch)
nodeRefiningTransition _ oldCost oldProg (Right (Failure _ Unsat)) =
  (NodeSucceeded Nothing oldCost oldProg, MarkFailure)
nodeRefiningTransition _ oldCost oldProg (Right (Failure _ failure)) =
  ( NodeSucceeded
      (Just $ "Solver finally failed with: " <> showAsText failure)
      oldCost
      oldProg,
    CleanUpAndSplitSketch
  )
nodeRefiningTransition
  _
  oldCost
  oldProg
  (Right (Viable newTrack _ bestKnownCost _)) =
    (NodeRefining newTrack oldCost oldProg, Refine True bestKnownCost)
nodeRefiningTransition
  _
  oldCost
  oldProg
  (Right (EasySynthFailure newTrack bestKnownCost _)) =
    (NodeRefining newTrack oldCost oldProg, Refine True bestKnownCost)
nodeRefiningTransition
  _
  oldCost
  _
  (Right (Success _ newTrack _ newCost newProg)) =
    _ensureNewCostIsSmaller
      newCost
      oldCost
      (NodeRefining newTrack newCost newProg, Refine True $ Just newCost)
nodeRefiningTransition
  oldTrack
  oldCost
  oldProg
  (Right (GotExample _ bestKnownCost)) =
    (NodeRefining oldTrack oldCost oldProg, Refine True bestKnownCost)
