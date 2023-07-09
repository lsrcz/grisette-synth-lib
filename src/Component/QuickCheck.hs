{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Component.QuickCheck where

import Component.CInputGen
import Component.ConcreteCircuit
import Component.Index
import Component.ProgramSpec
import Component.SemMap
import Test.QuickCheck.Counterexamples

data QuickCheckProblem e c cop csm cidx where
  QuickCheckProblem ::
    (CIndex cidx, CSemMap csm cop e c, CSpec cspec e c, CInputGen cgen c, Show c) =>
    { qcpGen :: cgen,
      qcpGenSize :: [Int],
      qcpSpec :: cspec,
      qcpSemMap :: csm,
      qcpProgram :: CCircuit cop cidx
    } ->
    QuickCheckProblem e c cop csm cidx

data QuickCheckResult e c
  = NoCounterExample
  | CounterExample
      { ceRemainingSize :: [Int],
        ceCounterExample :: [c],
        ceExecutionResult :: Either e [c]
      }
  deriving (Show)

quickCheckCCircuit ::
  forall e c cop csm cidx.
  QuickCheckProblem e c cop csm cidx ->
  IO (QuickCheckResult e c)
quickCheckCCircuit (QuickCheckProblem inputGen size spec csm c) =
  go size
  where
    go [] = return NoCounterExample
    go (s : ss) = do
      r <-
        quickCheckWith
          stdArgs {chatty = False}
          ( forAll (cInputGen inputGen s) $ \input ->
              let p = interpretCCircuit input c csm
               in cspec spec input p
          )
      case r of
        Just (v :&: ()) ->
          return $
            CounterExample
              (s : ss)
              v
              (interpretCCircuit v c csm)
        _ -> go ss
