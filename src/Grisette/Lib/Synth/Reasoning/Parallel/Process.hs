{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Lib.Synth.Reasoning.Parallel.Process
  ( ProcessConfig (..),
    NewMinimalCostMessage (..),
    TwoTrackVerifiers (..),
    Track (..),
    Message (..),
    Process (..),
    LogConfig (..),
    ProcessResponse,
    ProcessCostConstraint,
    ProcessConstraint,
    ConProgConstraint,
    runRequestInSubProcess,
    getProcessResponse,
    sendNewMinimalCost,
    processResponseNewCost,
    processResponseIsGotExample,
    processResponseIsFastTrackSuccess,
    processResponseIsSlowTrackSuccess,
    processResponseIsFastTrackEasySynthFailure,
    processResponseIsFastTrackViable,
  )
where

import Control.Concurrent
  ( newEmptyMVar,
    putMVar,
    takeMVar,
    threadDelay,
    tryPutMVar,
  )
import Control.Concurrent.Async (async, cancelWith)
import Control.Exception (AsyncException (ThreadKilled))
import Control.Monad (unless, when)
import Control.Monad.Except (runExceptT)
import Data.Bytes.Serial (Serial)
import Data.Either (fromRight)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable (hashWithSalt))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import Data.Typeable (Typeable, cast)
import GHC.Conc.Signal (setHandler)
import GHC.Generics (Generic)
import Grisette
  ( Doc,
    EvalSym,
    GenSymSimple,
    GrisetteSMTConfig (sbvConfig),
    LogicalOp (true),
    Mergeable,
    PPrint (pformat),
    Solvable (con),
    Solver,
    SolvingFailure,
    SymEq ((.==)),
    SymOrd ((.<)),
    ToCon,
    allClasses0,
    deriveGADT,
    genSymSimple,
    nest,
    simpleMerge,
    viaShow,
    vsep,
    withSolver,
  )
import Grisette.Lib.Synth.Context
  ( AngelicContext,
    ConcreteContext,
    SymbolicContext,
  )
import Grisette.Lib.Synth.Program.Choice.Counting (CountNumProgsEvidence, countNumChoicesWithEvidence, countNumProgsWithEvidence)
import Grisette.Lib.Synth.Program.Concrete
  ( ProgPPrint,
    eliminateProgTableDeadCode,
  )
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.ProgCost (ProgCost, symbolCost)
import Grisette.Lib.Synth.Program.SymbolTable
  ( ProgReachableSymbols,
    SymbolTable,
    filterByReachableSymbols,
  )
import Grisette.Lib.Synth.Reasoning.Parallel.DCTree (NodeId (NodeId))
import Grisette.Lib.Synth.Reasoning.Parallel.LogConfig (LogConfig (LogConfig, baseDir, progName), logRootDir)
import Grisette.Lib.Synth.Reasoning.Parallel.LogMultiLine (logMultiLineDoc)
import Grisette.Lib.Synth.Reasoning.Parallel.Serialize
  ( byteStringToWord64,
    nonBlockingReadObject,
    readObject,
    word64ToByteString,
    writeObject,
  )
import Grisette.Lib.Synth.Reasoning.Synthesis
  ( Example,
    RunSynthesisTask (solverRunSynthesisTaskExtractCex),
    SomeExample (SomeExample),
    SomeVerifier,
    SynthesisBoundCostTask
      ( SynthesisBoundCostTask,
        synthesisExtraConstraints,
        synthesisInitialExamples,
        synthesisInitialMaxCost,
        synthesisPrecondition,
        synthesisSketchSymbol,
        synthesisSketchTable,
        synthesisSymCostObj,
        synthesisVerifiers
      ),
    SynthesisResult
      ( SynthesisSolverFailure,
        SynthesisSuccess,
        SynthesisVerifierFailure
      ),
  )
import Grisette.Lib.Synth.VarId (ConcreteVarId)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (LogHandler (setFormatter))
import System.Log.Handler.Simple (fileHandler)
import System.Log.Logger
  ( Logger,
    Priority (DEBUG, NOTICE),
    getLogger,
    removeHandler,
    setHandlers,
    setLevel,
    updateGlobalLogger,
  )
import System.Posix
  ( FdOption (NonBlockingRead),
    ProcessStatus (Exited, Stopped, Terminated),
    closeFd,
    createPipe,
    createProcessGroupFor,
    forkProcess,
    getProcessID,
    getProcessStatus,
    sigHUP,
    sigINT,
    sigTERM,
  )
import System.Posix.ByteString (fdRead, fdWrite, setFdOption)
import System.Posix.Types (CPid (CPid), Fd, ProcessGroupID, ProcessID)
import Grisette.Lib.Synth.Program.Choice.Split (LowestSeqNum, PartitionSpec)

_createLogger :: LogConfig -> NodeId -> IO Logger
_createLogger logConfig@LogConfig {..} (NodeId nodeId) = do
  let loggerName = T.unpack progName <> "-" <> show nodeId
  let defaultFormatter lh =
        return $
          setFormatter
            lh
            ( simpleLogFormatter $
                "[$time : $loggername@" <> show nodeId <> " : $prio] $msg"
            )
  let rootDir = logRootDir logConfig
  h <-
    fileHandler
      (rootDir <> "/" <> show nodeId <> ".log")
      DEBUG
      >>= defaultFormatter
  updateGlobalLogger
    loggerName
    (setHandlers [h] . setLevel DEBUG . removeHandler)
  getLogger loggerName

data TwoTrackVerifiers sketch conProg = TwoTrackVerifiers
  { fastTrackVerifiers :: [SomeVerifier sketch conProg],
    slowTrackVerifiers :: [SomeVerifier sketch conProg]
  }

type ProcessCostConstraint costObj cost conProg sketch =
  ( ProgCost costObj conProg Int ConcreteContext,
    ProgCost costObj sketch cost SymbolicContext,
    ProgCost costObj sketch cost AngelicContext
  )

type ProcessConstraint
  sketchSpec
  sketch
  conProg
  costObj
  cost
  symSemObj
  symVal
  conSemObj
  conVal
  matcher =
  ( GenSymSimple sketchSpec sketch,
    ProcessCostConstraint costObj cost conProg sketch,
    ProgPPrint sketchSpec,
    ProgPPrint conProg,
    EvalSym sketch,
    ToCon sketch conProg,
    ProgReachableSymbols conProg,
    SymOrd cost,
    Mergeable cost,
    Num cost,
    Serial conProg,
    Serial cost,
    Serial conVal,
    Serial symSemObj,
    Serial conSemObj,
    Serial matcher,
    Typeable sketch,
    Typeable symSemObj,
    Typeable conSemObj,
    Typeable conVal,
    Typeable matcher,
    Typeable symVal,
    PPrint conVal,
    Hashable sketchSpec,
    LowestSeqNum sketchSpec,
    PartitionSpec sketchSpec
  )

type ConProgConstraint conProg conOp conVarId conType =
  ( ConcreteVarId conVarId,
    conProg ~ Concrete.Prog conOp conVarId conType,
    Concrete.OpPPrint conOp,
    Show conOp,
    PPrint conType,
    Show conType
  )

data
  ProcessConfig
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher
  where
  ProcessConfig ::
    ( ProcessConstraint
        sketchSpec
        sketch
        conProg
        costObj
        cost
        symSemObj
        symVal
        conSemObj
        conVal
        matcher,
      ConProgConstraint conProg conOp conVarId conType
    ) =>
    { costObj :: costObj,
      sketchSpec :: SymbolTable sketchSpec,
      sketchSymbol :: T.Text,
      logConfig :: LogConfig,
      nodeId :: NodeId,
      verifiers :: Logger -> TwoTrackVerifiers sketch conProg,
      easySketchFromFastResult ::
        Maybe (SymbolTable conProg -> SymbolTable sketchSpec),
      transcriptSMT :: Bool,
      doDeadCodeElimination :: Bool,
      easySynthTimeout :: Int,
      exactCost :: Maybe Int,
      initialCost :: Maybe Int,
      countNumProgsEvidence ::
        Maybe (CountNumProgsEvidence (SymbolTable sketchSpec))
    } ->
    ProcessConfig
      sketchSpec
      sketch
      conProg
      costObj
      cost
      symSemObj
      symVal
      conSemObj
      conVal
      matcher

data NewMinimalCostMessage = NewMinimalCostMessage
  { newMinimalCost :: Maybe Int
  }

deriveGADT [''NewMinimalCostMessage] allClasses0

data ProcessState = ProcessState
  { knownMinimalCost :: Maybe Int,
    wrChild :: Fd,
    rdChild :: Fd,
    logger :: Logger
  }

_readLogger :: IORef ProcessState -> IO Logger
_readLogger stateRef = do
  ProcessState {..} <- readIORef stateRef
  return logger

_readKnownMinimalCost :: IORef ProcessState -> IO (Maybe Int)
_readKnownMinimalCost stateRef = do
  ProcessState {..} <- readIORef stateRef
  return knownMinimalCost

_readWrChild :: IORef ProcessState -> IO Fd
_readWrChild stateRef = do
  ProcessState {..} <- readIORef stateRef
  return wrChild

_readRdChild :: IORef ProcessState -> IO Fd
_readRdChild stateRef = do
  ProcessState {..} <- readIORef stateRef
  return rdChild

data Track = FastTrack | SlowTrack

deriveGADT [''Track] allClasses0

data Message conProg symSemObj symVal conSemObj conVal matcher
  = FastTrackViable
      { _nextTrack :: Track,
        _examples :: [Example symSemObj symVal conSemObj conVal matcher],
        _knownCost :: Maybe Int,
        _prog :: SymbolTable conProg
      }
  | FastTrackEasySynthFailure
      { _knownCost :: Maybe Int,
        _examples :: [Example symSemObj symVal conSemObj conVal matcher]
      }
  | GotExample
      { _example :: Example symSemObj symVal conSemObj conVal matcher,
        _knownCost :: Maybe Int
      }
  | Success
      { _track :: Track,
        _examples :: [Example symSemObj symVal conSemObj conVal matcher],
        _cost :: Int,
        _prog :: SymbolTable conProg
      }
  | Failure
      { _examples :: [Example symSemObj symVal conSemObj conVal matcher],
        _failure :: SolvingFailure
      }
  deriving (Show, Generic)

pformatMessageSummary ::
  Message conProg symSemObj symVal conSemObj conVal matcher ->
  Doc ann
pformatMessageSummary (FastTrackViable nextTrack examples cost _) =
  "FastTrackViable (with "
    <> pformat (length examples)
    <> " examples, proceeding with "
    <> pformat nextTrack
    <> " track, best known cost: "
    <> pformat cost
    <> ")"
pformatMessageSummary (FastTrackEasySynthFailure cost examples) =
  "FastTrackEasySynthFailure (with "
    <> pformat (length examples)
    <> " examples, best known cost: "
    <> pformat cost
    <> ")"
pformatMessageSummary (Success track examples cost _) =
  "Success "
    <> pformat track
    <> " (with "
    <> pformat (length examples)
    <> " examples, cost "
    <> pformat cost
    <> ")"
pformatMessageSummary (GotExample _ cost) =
  "GotExample (best known cost: " <> pformat cost <> ")"
pformatMessageSummary (Failure examples result) =
  "Failure (with "
    <> pformat (length examples)
    <> " examples): "
    <> pformat result

instance
  (ProgPPrint conProg, PPrint conVal) =>
  PPrint (Message conProg symSemObj symVal conSemObj conVal matcher)
  where
  pformat message@(FastTrackViable _ _ _ prog) =
    nest 2 $
      vsep
        [ pformatMessageSummary message,
          pformat prog
        ]
  pformat message@(FastTrackEasySynthFailure _ _) = pformatMessageSummary message
  pformat message@(GotExample example currentCost) =
    nest 2 $
      vsep
        [ pformatMessageSummary message,
          pformat example,
          pformat currentCost
        ]
  pformat message@(Success _ _ _ prog) =
    nest 2 $ vsep [pformatMessageSummary message, pformat prog]
  pformat message@(Failure _ _) = pformatMessageSummary message

instance
  ( Serial conProg,
    Serial symSemObj,
    Serial conSemObj,
    Serial conVal,
    Serial matcher
  ) =>
  Serial (Message conProg symSemObj symVal conSemObj conVal matcher)

_updateKnownMinimalCost :: IORef ProcessState -> Maybe Int -> IO ()
_updateKnownMinimalCost stateRef newMinimalCost =
  modifyIORef' stateRef $ \s ->
    s
      { knownMinimalCost =
          case (knownMinimalCost s, newMinimalCost) of
            (Nothing, c) -> c
            (Just _, Nothing) -> knownMinimalCost s
            (Just oldMinimalCost, Just newMinimalCost) ->
              Just $ min oldMinimalCost newMinimalCost
      }

_sendAndWaitForNewMinimalCost ::
  ( Serial conProg,
    Serial symSemObj,
    Serial conSemObj,
    Serial conVal,
    Serial matcher
  ) =>
  IORef ProcessState ->
  Message conProg symSemObj symVal conSemObj conVal matcher ->
  IO NewMinimalCostMessage
_sendAndWaitForNewMinimalCost stateRef message = do
  wrChild <- wrChild <$> readIORef stateRef
  rdChild <- rdChild <$> readIORef stateRef
  writeObject wrChild message
  request@(NewMinimalCostMessage newMinimalCost) <- readObject rdChild
  _updateKnownMinimalCost stateRef newMinimalCost
  return request

_solverRunSynthRequest ::
  forall
    handle
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher.
  (Solver handle) =>
  handle ->
  Bool ->
  ProcessConfig
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  NewMinimalCostMessage ->
  IORef ProcessState ->
  IO
    ( Int,
      [Example symSemObj symVal conSemObj conVal matcher],
      SynthesisResult conProg
    )
_solverRunSynthRequest
  handle
  isFastTrack
  ProcessConfig {..}
  NewMinimalCostMessage {..}
  stateRef = do
    logger <- _readLogger stateRef
    let sketch = genSymSimple sketchSpec "sketch" :: SymbolTable sketch
    let TwoTrackVerifiers {..} = verifiers logger
    let task =
          SynthesisBoundCostTask
            { synthesisVerifiers =
                fastTrackVerifiers
                  ++ (if isFastTrack then slowTrackVerifiers else []),
              synthesisSymCostObj = costObj,
              synthesisSketchTable = sketch,
              synthesisSketchSymbol = sketchSymbol,
              synthesisPrecondition = true,
              synthesisInitialMaxCost =
                fromIntegral <$> newMinimalCost :: Maybe cost,
              synthesisInitialExamples = [],
              synthesisExtraConstraints = \(SomeExample ex) -> do
                let symProgCost =
                      symbolCost costObj sketch sketchSymbol ::
                        SymbolicContext cost
                let symProgCostLessThanMaxCost maxCost = simpleMerge $ do
                      eitherCost <- runExceptT symProgCost
                      case eitherCost of
                        Left _ -> return $ con False
                        Right cost -> return $ cost .< maxCost
                let symProgCostEqualToCost expectedCost = simpleMerge $ do
                      eitherCost <- runExceptT symProgCost
                      case eitherCost of
                        Left _ -> return $ con False
                        Right cost -> return $ cost .== expectedCost
                case cast ex of
                  Just (ex :: Example symSemObj symVal conSemObj conVal matcher) -> do
                    currentCost <- _readKnownMinimalCost stateRef
                    _sendAndWaitForNewMinimalCost
                      stateRef
                      ( GotExample ex currentCost ::
                          Message conProg symSemObj symVal conSemObj conVal matcher
                      )
                    newCost <- _readKnownMinimalCost stateRef
                    logMultiLineDoc logger DEBUG $
                      nest 2 $
                        vsep ["Got cex: ", pformat ex]
                    case exactCost of
                      Just cost -> return $ symProgCostEqualToCost (fromIntegral cost)
                      Nothing ->
                        if newCost /= currentCost
                          then do
                            logMultiLineDoc logger DEBUG $
                              nest 2 $
                                vsep ["Update cost: ", pformat newCost]
                            return $
                              symProgCostLessThanMaxCost
                                (fromIntegral $ fromJust newCost)
                          else return $ con True
                  _ -> error "Should not happen"
            }
    (example, r) <- solverRunSynthesisTaskExtractCex handle task
    let castExample ::
          SomeExample sketch conProg ->
          Maybe (Example symSemObj symVal conSemObj conVal matcher)
        castExample (SomeExample e) = cast e
    let example' =
          traverse castExample example ::
            Maybe [Example symSemObj symVal conSemObj conVal matcher]
    case example' of
      Nothing -> error "Failed to cast example"
      Just example'' ->
        case r of
          SynthesisSuccess s -> do
            let prog =
                  if doDeadCodeElimination
                    then
                      eliminateProgTableDeadCode $
                        fromRight undefined $
                          filterByReachableSymbols (HS.singleton sketchSymbol) s
                    else s
            return
              ( fromRight (error "Failed to compute symbolCost") $
                  symbolCost costObj prog sketchSymbol,
                example'',
                SynthesisSuccess prog
              )
          _ -> return (-1, example'', r)

data Process = Process
  { pid :: ProcessID,
    pgid :: ProcessGroupID,
    pipeRd :: Fd,
    pipeWr :: Fd
  }

instance Eq Process where
  Process (CPid pid1) _ _ _ == Process (CPid pid2) _ _ _ = pid1 == pid2

instance Show Process where
  show Process {..} = "Process " <> show pid

instance PPrint Process where
  pformat = viaShow

instance Hashable Process where
  hashWithSalt salt Process {pid = CPid pid} = hashWithSalt salt pid

data ProcessStep conProg
  = InitialStep
  | FastTrackSynthStep
  | FastTrackEasySynthStep (SymbolTable conProg)
  | SlowTrackSynthStep
  | TerminationStep

isTerminationStep :: ProcessStep conProg -> Bool
isTerminationStep TerminationStep = True
isTerminationStep _ = False

runRequestInSubProcess ::
  forall
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher.
  GrisetteSMTConfig ->
  ProcessConfig
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  IO Process
runRequestInSubProcess config processConfig@ProcessConfig {..} = do
  (rdHost, wrChild) <- createPipe
  (rdChild, wrHost) <- createPipe
  pid <- forkProcess $ do
    setHandler sigINT Nothing
    setHandler sigTERM Nothing
    setHandler sigHUP Nothing
    closeFd rdHost
    closeFd wrHost
    pid <- getProcessID
    pgid <- createProcessGroupFor pid
    fdWrite wrChild $ word64ToByteString $ fromIntegral pgid
    logger <- _createLogger logConfig nodeId
    stateRef <-
      newIORef $
        ProcessState
          { knownMinimalCost = initialCost,
            wrChild,
            rdChild,
            logger
          }
    let loop :: (Solver handle) => handle -> ProcessStep conProg -> IO ()
        loop solver st = do
          nextStep <- case st of
            InitialStep -> initialStep processConfig stateRef
            FastTrackSynthStep ->
              fastTrackSynthStep solver processConfig stateRef
            FastTrackEasySynthStep prog ->
              fastTrackEasySynthStep config processConfig stateRef prog
            SlowTrackSynthStep ->
              slowTrackSynthStep solver processConfig stateRef
            TerminationStep -> error "Should not happen"
          when (not $ isTerminationStep nextStep) $ loop solver nextStep
    withSolver config {sbvConfig = (sbvConfig config)} $ \solver ->
      loop solver InitialStep
  closeFd rdChild
  closeFd wrChild
  pgidBs <- fdRead rdHost 8
  setFdOption rdHost NonBlockingRead True
  let pgid = fromIntegral $ byteStringToWord64 pgidBs
  return $ Process pid pgid rdHost wrHost

initialStep ::
  ProcessConfig
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  IORef ProcessState ->
  IO (ProcessStep conProg)
initialStep ProcessConfig {..} stateRef = do
  logger <- _readLogger stateRef
  let TwoTrackVerifiers {..} = verifiers logger

  case countNumProgsEvidence of
    Nothing ->
      logMultiLineDoc logger NOTICE $
        nest 2 $
          vsep
            [ "Start synthesizing with sketch: ",
              pformat sketchSpec,
              ", entry symbol:",
              pformat sketchSymbol
            ]
    Just evidence -> do
      let numOfChoices = countNumChoicesWithEvidence evidence sketchSpec
      let numOfProgs = countNumProgsWithEvidence evidence sketchSpec
      logMultiLineDoc logger NOTICE $
        nest 2 $
          vsep
            [ "Start synthesizing with sketch: ",
              pformat sketchSpec,
              ", entry symbol:",
              pformat sketchSymbol,
              "number of choices: ",
              pformat numOfChoices,
              "number of well-typed progs: ",
              pformat numOfProgs
            ]

  let haveTwoTracks = not $ null slowTrackVerifiers
  if haveTwoTracks
    then return FastTrackSynthStep
    else return SlowTrackSynthStep

_currentNewMinimalCostMessage :: IORef ProcessState -> IO NewMinimalCostMessage
_currentNewMinimalCostMessage stateRef = do
  ProcessState {..} <- readIORef stateRef
  return $ NewMinimalCostMessage knownMinimalCost

fastTrackSynthStep ::
  forall
    handle
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher.
  (Solver handle) =>
  handle ->
  ProcessConfig
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher ->
  IORef ProcessState ->
  IO (ProcessStep conProg)
fastTrackSynthStep handle processConfig@ProcessConfig {..} stateRef = do
  logger <- _readLogger stateRef
  wrChild <- _readWrChild stateRef
  minimalCostMessage@NewMinimalCostMessage {..} <-
    _currentNewMinimalCostMessage stateRef
  logMultiLineDoc logger NOTICE $
    nest 2 $
      vsep ["Fast track synth with minimal cost: ", pformat newMinimalCost]
  (_, examples, result) <-
    _solverRunSynthRequest handle True processConfig minimalCostMessage stateRef
  case result of
    SynthesisSuccess prog -> do
      logMultiLineDoc logger NOTICE $
        nest 2 $
          vsep ["Fast track synth success: ", pformat prog]
      cost <- _readKnownMinimalCost stateRef
      let nextTrack =
            if isJust easySketchFromFastResult then FastTrack else SlowTrack
      _sendAndWaitForNewMinimalCost stateRef $
        FastTrackViable nextTrack examples cost prog
      if isJust easySketchFromFastResult
        then return $ FastTrackEasySynthStep prog
        else return $ SlowTrackSynthStep
    SynthesisSolverFailure failure -> do
      logMultiLineDoc logger NOTICE $
        nest 2 $
          vsep ["Fast track synth failure: ", pformat failure]
      writeObject
        wrChild
        ( Failure examples failure ::
            Message conProg symSemObj symVal conSemObj conVal matcher
        )
      return TerminationStep
    SynthesisVerifierFailure err ->
      error $
        T.unpack $
          "Verification crashed, please check the code, reason: " <> err

fastTrackEasySynthStep ::
  forall
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher.
  GrisetteSMTConfig ->
  ProcessConfig sketchSpec sketch conProg costObj cost symSemObj symVal conSemObj conVal matcher ->
  IORef ProcessState ->
  SymbolTable conProg ->
  IO (ProcessStep conProg)
fastTrackEasySynthStep
  config
  processConfig@ProcessConfig {..}
  stateRef
  conProg = do
    unless (isJust easySketchFromFastResult) $ error "Should not happen"
    let sketchSpec = fromJust easySketchFromFastResult conProg
    let fastTrackEasySynthTask =
          processConfig
            { sketchSpec = sketchSpec,
              verifiers = \logger ->
                TwoTrackVerifiers
                  (slowTrackVerifiers $ verifiers logger)
                  []
            } ::
            ProcessConfig
              sketchSpec
              sketch
              conProg
              costObj
              cost
              symSemObj
              symVal
              conSemObj
              conVal
              matcher
    minimalCostMessage@NewMinimalCostMessage {..} <-
      _currentNewMinimalCostMessage stateRef
    logger <- _readLogger stateRef
    logMultiLineDoc logger NOTICE $
      nest 2 $
        vsep
          [ "Fast track easy synth with minimal cost: ",
            pformat newMinimalCost,
            "sketch: " <> pformat sketchSpec,
            "sketch symbol: " <> pformat sketchSymbol
          ]
    r <- newEmptyMVar
    a <- async $ do
      res <- withSolver config $ \localHandle ->
        _solverRunSynthRequest
          localHandle
          False
          fastTrackEasySynthTask
          minimalCostMessage
          stateRef
      putMVar r $ Just res
    _ <- async $ do
      threadDelay (easySynthTimeout * 1000000)
      cancelWith a ThreadKilled
      tryPutMVar r Nothing
    easySynthResult <- takeMVar r
    case easySynthResult of
      Just (cost, examples, SynthesisSuccess r) -> do
        logMultiLineDoc logger NOTICE $
          nest 2 $
            vsep
              [ "Fast track easy synth success: ",
                pformat r,
                "cost: " <> pformat cost
              ]
        _updateKnownMinimalCost stateRef (Just cost)
        _sendAndWaitForNewMinimalCost stateRef $
          Success FastTrack examples cost r
        return FastTrackSynthStep
      Just (_, examples, _) -> do
        logMultiLineDoc
          logger
          NOTICE
          "Fast track easy synth failed, switch to slow track."
        cost <- _readKnownMinimalCost stateRef
        _sendAndWaitForNewMinimalCost
          stateRef
          ( FastTrackEasySynthFailure cost examples ::
              Message conProg symSemObj symVal conSemObj conVal matcher
          )
        return SlowTrackSynthStep
      _ -> do
        logMultiLineDoc
          logger
          NOTICE
          "Fast track easy synth timed out or crashed, switch to slow track."
        cost <- _readKnownMinimalCost stateRef
        _sendAndWaitForNewMinimalCost stateRef $
          ( FastTrackEasySynthFailure cost [] ::
              Message conProg symSemObj symVal conSemObj conVal matcher
          )
        return SlowTrackSynthStep

slowTrackSynthStep ::
  forall
    handle
    sketchSpec
    sketch
    conProg
    costObj
    cost
    symSemObj
    symVal
    conSemObj
    conVal
    matcher.
  (Solver handle) =>
  handle ->
  ProcessConfig sketchSpec sketch conProg costObj cost symSemObj symVal conSemObj conVal matcher ->
  IORef ProcessState ->
  IO (ProcessStep conProg)
slowTrackSynthStep handle processConfig@ProcessConfig {..} stateRef = do
  minimalCostMessage@NewMinimalCostMessage {..} <-
    _currentNewMinimalCostMessage stateRef
  logger <- _readLogger stateRef
  logMultiLineDoc logger NOTICE $
    nest 2 $
      vsep
        [ "Slow track synth with minimal cost: ",
          pformat newMinimalCost
        ]
  (cost, examples, result) <-
    _solverRunSynthRequest handle False processConfig minimalCostMessage stateRef
  _updateKnownMinimalCost stateRef (Just cost)
  case result of
    SynthesisSuccess prog -> do
      logMultiLineDoc logger NOTICE $
        nest 2 $
          vsep
            [ "Slow track synth success: ",
              pformat prog,
              "cost: " <> pformat cost
            ]
      _sendAndWaitForNewMinimalCost stateRef $
        Success SlowTrack examples cost prog
      return SlowTrackSynthStep
    SynthesisSolverFailure failure -> do
      logMultiLineDoc logger NOTICE $
        nest 2 $
          vsep
            [ "Slow track synth failure: ",
              pformat failure
            ]
      wrChild <- _readWrChild stateRef
      writeObject
        wrChild
        ( Failure examples failure ::
            Message conProg symSemObj symVal conSemObj conVal matcher
        )
      return TerminationStep
    SynthesisVerifierFailure err -> do
      error $
        T.unpack $
          "Verification crashed, please check the code, reason: " <> err

_closeProcessPipes :: Process -> IO ()
_closeProcessPipes Process {..} = do
  closeFd pipeRd
  closeFd pipeWr

_readProcessResponse ::
  ( Serial conProg,
    Serial symSemObj,
    Serial conSemObj,
    Serial conVal,
    Serial matcher
  ) =>
  Process ->
  ProcessStatus ->
  IO (Either T.Text (Message conProg symSemObj symVal conSemObj conVal matcher))
_readProcessResponse process@Process {..} (Exited ExitSuccess) = do
  msg <- readObject pipeRd
  _closeProcessPipes process
  return $ Right msg
_readProcessResponse process@Process {..} (Exited (ExitFailure e)) = do
  _closeProcessPipes process
  return $ Left $ "Process exited with error code " <> T.pack (show e)
_readProcessResponse process@Process {..} (Terminated signal dumped) = do
  _closeProcessPipes process
  return $
    Left $
      "Process terminated by signal "
        <> T.pack (show signal)
        <> (if dumped then " (core dumped)" else "")
_readProcessResponse _ (Stopped {}) = error "Should not happen"

type ProcessResponse conProg symSemObj symVal conSemObj conVal matcher =
  Either T.Text (Message conProg symSemObj symVal conSemObj conVal matcher)

processResponseNewCost ::
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher -> Maybe Int
processResponseNewCost (Right (Success _ _ cost _)) = Just cost
processResponseNewCost _ = Nothing

processResponseIsGotExample ::
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher -> Bool
processResponseIsGotExample (Right GotExample {}) = True
processResponseIsGotExample _ = False

processResponseIsFastTrackSuccess ::
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher -> Bool
processResponseIsFastTrackSuccess (Right (Success FastTrack _ _ _)) = True
processResponseIsFastTrackSuccess _ = False

processResponseIsSlowTrackSuccess ::
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher -> Bool
processResponseIsSlowTrackSuccess (Right (Success SlowTrack _ _ _)) = True
processResponseIsSlowTrackSuccess _ = False

processResponseIsFastTrackEasySynthFailure ::
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher -> Bool
processResponseIsFastTrackEasySynthFailure
  (Right FastTrackEasySynthFailure {}) = True
processResponseIsFastTrackEasySynthFailure _ = False

processResponseIsFastTrackViable ::
  ProcessResponse conProg symSemObj symVal conSemObj conVal matcher -> Bool
processResponseIsFastTrackViable (Right FastTrackViable {}) = True
processResponseIsFastTrackViable _ = False

getProcessResponse ::
  ( Serial conProg,
    Serial symSemObj,
    Serial conSemObj,
    Serial conVal,
    Serial matcher
  ) =>
  Bool ->
  Process ->
  IO
    ( Maybe
        (ProcessResponse conProg symSemObj symVal conSemObj conVal matcher)
    )
getProcessResponse blk process@Process {..} = do
  r <- getProcessStatus blk False pid
  case r of
    Nothing -> do
      when blk $ error "Should not happen"
      msg <- nonBlockingReadObject pipeRd
      case msg of
        Nothing -> return Nothing
        Just msg@(Failure _ _) -> do
          _ <- getProcessStatus True False pid
          _closeProcessPipes process
          return $ Just $ Right msg
        Just msg -> return $ Just $ Right msg
    Just status -> Just <$> _readProcessResponse process status

sendNewMinimalCost :: Maybe Int -> Process -> IO ()
sendNewMinimalCost newMinimalCost Process {..} = do
  writeObject pipeWr $ NewMinimalCostMessage newMinimalCost
