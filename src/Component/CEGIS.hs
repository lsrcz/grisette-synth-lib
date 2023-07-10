{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Component.CEGIS where

import Component.CInputGen
import Component.Circuit
import Component.ConcreteCircuit
import Component.Index
import Component.IntermediateGen
import Component.Monad
import Component.ProgramSpec
import Component.QuickCheck
import Component.SemMap
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.SBV as SBV
import qualified Data.SBV.Control as SBVC
import Grisette
import Grisette.Backend.SBV.Data.SMT.Lowering
import Grisette.Backend.SBV.Data.SMT.SymBiMap
import Grisette.Experimental
import Grisette.Internal.Backend.SBV

sbvCheckSatResult :: SBVC.CheckSatResult -> SolvingFailure
sbvCheckSatResult SBVC.Sat = error "Should not happen"
sbvCheckSatResult (SBVC.DSat msg) = DSat msg
sbvCheckSatResult SBVC.Unsat = Unsat
sbvCheckSatResult SBVC.Unk = Unk

cegis' ::
  forall n sprogram cinput guesserState inputGenCurrSize inputGenState cprogram e.
  ( ToCon sprogram cprogram,
    EvaluateSym sprogram,
    Mergeable e,
    Mergeable sprogram,
    EvaluateSym e,
    EvaluateSym sprogram
  ) =>
  GrisetteSMTConfig n ->
  (ExceptT e UnionM sprogram -> SymBool) ->
  (guesserState -> inputGenCurrSize -> cinput -> ExceptT e UnionM sprogram -> (SymBool, guesserState)) ->
  (inputGenState -> cprogram -> IO (Either SolvingFailure (cinput, inputGenCurrSize, inputGenState))) ->
  ExceptT e UnionM sprogram ->
  guesserState ->
  inputGenState ->
  IO (Either SolvingFailure ([cinput], cprogram))
cegis' config initialCondition sspec verifier prog initialGuesserState initialGenState =
  SBV.runSMTWith (sbvConfig config) $ do
    let SymBool t = initialCondition prog
    (newm, a) <- lowerSinglePrim config t
    SBVC.query $ do
      SBV.constrain a
      r <- SBVC.checkSat
      mr <- case r of
        SBVC.Sat -> do
          md <- SBVC.getModel
          let model = parseModel config md newm
          return $ Right $ case evaluateSym True model prog of
            (ExceptT (SingleU (Right v))) -> fromJust $ toCon v
        _ -> return $ Left $ sbvCheckSatResult r
      loop initialGuesserState initialGenState mr [] newm
  where
    guess ::
      guesserState ->
      inputGenCurrSize ->
      cinput ->
      SymBiMap ->
      SBVC.Query (SymBiMap, guesserState, Either SolvingFailure cprogram)
    guess st inputGenCurrentSize input origSymbolMap = do
      let (SymBool t, newGuesserState) = sspec st inputGenCurrentSize input prog
      (newSymbolMap, lowered) <- lowerSinglePrimCached config t origSymbolMap
      SBV.constrain lowered
      r <- SBVC.checkSat
      case r of
        SBVC.Sat -> do
          md <- SBVC.getModel
          let model = parseModel config md newSymbolMap
          return
            ( newSymbolMap,
              newGuesserState,
              Right $ case evaluateSym True model prog of
                (ExceptT (SingleU (Right v))) -> fromJust $ toCon v
            )
        _ -> return (newSymbolMap, newGuesserState, Left $ sbvCheckSatResult r)
    loop ::
      guesserState ->
      inputGenState ->
      Either SolvingFailure cprogram ->
      [cinput] ->
      SymBiMap ->
      SBVC.Query (Either SolvingFailure ([cinput], cprogram))
    loop guesserState inputGenState (Right cprog) cexes origSymbolMap = do
      r <- liftIO $ verifier inputGenState cprog
      case r of
        Left Unsat -> return $ Right (cexes, cprog)
        Left v -> return $ Left v
        Right (cex, inputGenCurrentSize, newInputGenState) -> do
          (newSymbolMap, newGuesserState, res) <-
            guess guesserState inputGenCurrentSize cex origSymbolMap
          loop newGuesserState newInputGenState res (cex : cexes) newSymbolMap
    loop _ _ (Left v) _ _ = return $ Left v

data CegisQCProblem e s ce c op cop idx cidx where
  CegisQCProblem ::
    ( CIndex cidx,
      Index idx,
      Num idx,
      GenSymSimpleConstrained (SOrdBound idx ()) idx,
      Mergeable op,
      Mergeable e,
      SemMap sm op g e s,
      CSemMap csm cop ce c,
      CSpec cspec ce c,
      SSpec sspec e s,
      CInputGen cgen c,
      Show c,
      ToSym c s,
      SEq s,
      EvaluateSym e,
      ToCon op cop,
      ToCon idx cidx,
      EvaluateSym op,
      EvaluateSym idx,
      IntermediateSGen intermediateGen op s,
      Mergeable s
    ) =>
    { cqpCGen :: cgen,
      cqpCGenSize :: [Int],
      cqpCSpec :: cspec,
      cqpCSemMap :: csm,
      cqpSSpec :: sspec,
      cqpSCircuitGenSpec :: CircuitSpec op sm,
      cqpSCircuitGenError :: e,
      cqpSCircuitInterpretError :: e,
      cqpSemMap :: sm,
      cqpSIntermediateGen :: intermediateGen
    } ->
    CegisQCProblem e s ce c op cop idx cidx

cegisQC ::
  forall n e s ce c op cop idx cidx.
  GrisetteSMTConfig n ->
  CegisQCProblem e s ce c op cop idx cidx ->
  IO (Either SolvingFailure ([[c]], CCircuit cop cidx))
cegisQC config (CegisQCProblem cgen cgenSize cspecFunc csem sspecFunc gen err ierr sem igen) =
  cegis' config initialCondition sspec' cspec' x 0 cgenSize
  where
    x :: ExceptT e UnionM (Circuit op idx)
    x = runFreshT (genCircuit err gen :: M e (Circuit op idx)) "prog"
    initialCondition :: ExceptT e UnionM (Circuit op idx) -> SymBool
    initialCondition e = simpleMerge $ do
      v <- runExceptT e
      case v of
        Left _ -> return $ con False
        Right _ -> return $ con True

    sspecCurrent :: Int -> Int -> [c] -> ExceptT e UnionM (Circuit op idx) -> ExceptT e UnionM [s]
    sspecCurrent idx inputGenCurrentSize input sprogram = flip runFreshT (FreshIdentWithInfo "x" idx) $ do
      s <- lift sprogram
      interpretCircuit ierr (toSym input :: [s]) s sem inputGenCurrentSize igen

    sspec' :: Int -> Int -> [c] -> ExceptT e UnionM (Circuit op idx) -> (SymBool, Int)
    sspec' idx inputGenCurrentSize input sprogram =
      ( simpleMerge $ do
          v <- runExceptT $ sspecCurrent idx inputGenCurrentSize input sprogram
          mrgReturn $ sspec sspecFunc (toSym input) v,
        idx + 1
      )

    cspec' :: [Int] -> CCircuit cop cidx -> IO (Either SolvingFailure ([c], Int, [Int]))
    cspec' [] _ = return $ Left Unsat
    cspec' sizes cprog = do
      r <- quickCheckCCircuit (QuickCheckProblem cgen sizes cspecFunc csem cprog)
      case r of
        NoCounterExample -> return $ Left Unsat
        CounterExample {ceCurrentSize = s', ceRemainingSize = ss', ceCounterExample = v} ->
          return $ Right (v, s', ss')
