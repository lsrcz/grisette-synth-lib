{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Component.ConcreteCircuit where

import Component.Circuit
import Component.Index
import Component.OpPrettyPrinter
import Component.PrettyPrinterUtil
import Component.SemMap
import Data.ByteString qualified as B
import Data.ByteString.UTF8 qualified as U
import Data.Either (fromRight)
import Data.HashMap.Strict qualified as M
import Data.Hashable
import Data.List
import GHC.Generics
import Grisette
import Prettyprinter

data CNode op idx = CNode
  { cnodeOp :: op,
    cnodeIdx :: [idx],
    cnodeInputIdx :: [idx]
  }
  deriving (Show, Eq, Generic)

instance (Pretty op, Pretty idx) => Pretty (CNode op idx) where
  pretty (CNode op idx iidx)
    | length idx == 1 =
        "r" <> pretty (head idx)
          <+> "="
          <+> pretty op
            <> commaSepParenLists (fmap pretty iidx)
    | otherwise =
        commaSepParenLists (fmap pretty idx)
          <+> "="
          <+> pretty op <> commaSepParenLists (fmap pretty iidx)

deriving via
  (Default (CNode cop cidx))
  instance
    (ToCon op cop, ToCon idx cidx) =>
    ToCon (Node op idx) (CNode cop cidx)

data CCircuit op idx = CCircuit
  { ccirInputNames :: [B.ByteString],
    ccirNodes :: [CNode op idx],
    ccirOutputIdx :: [idx]
  }
  deriving (Show, Eq, Generic)

prettyPrintCCircuit ::
  forall op idx ann.
  ( OpRegNamer op idx,
    OpPrinterDescription op,
    CIndex idx,
    Hashable idx
  ) =>
  CCircuit op idx ->
  Doc ann
prettyPrintCCircuit (CCircuit inames nodes oidx) =
  vsep
    [ nest 2 $
        vsep
          [ commaSepParenLists
              ((regNames M.!) <$> (mkCIndex ninput <$> [0 .. ninput - 1] :: [idx]))
              <+> "=>"
              <+> lbrace,
            vsep (prettyPrintNode <$> sortedNodes),
            "return"
              <+> ( if length oidx == 1
                      then regNames M.! head oidx
                      else commaSepParenLists ((regNames M.!) <$> oidx)
                  )
          ],
      rbrace
    ]
  where
    ninput = length inames
    sortedNodes = sortOn (head . cnodeIdx) nodes
    inputIndicesWithNames = zip (mkCIndex ninput <$> [0 .. ninput - 1] :: [idx]) inames
    nameRegs :: [CNode op idx] -> M.HashMap idx (Doc ann) -> M.HashMap idx (Doc ann)
    nameRegs [] m = m
    nameRegs (CNode op oidx iidx : ns) m =
      nameRegs ns $ nameOutputs op oidx m
    nameInputs :: [(idx, B.ByteString)] -> M.HashMap idx (Doc ann) -> M.HashMap idx (Doc ann)
    nameInputs [] m = m
    nameInputs ((x, nm) : xs) m =
      nameInputs xs (M.insert x (pretty (U.toString nm)) m)
    regNames = nameRegs sortedNodes $ nameInputs inputIndicesWithNames M.empty

    prettyPrintNode (CNode op idx iidx) = prettyPrintOpInst op idx iidx regNames

instance
  ( OpRegNamer op idx,
    OpPrinterDescription op,
    CIndex idx,
    Hashable idx
  ) =>
  Pretty (CCircuit op idx)
  where
  pretty = prettyPrintCCircuit

{-
instance (Pretty op, Pretty idx, CIndex idx) => Pretty (CCircuit op idx) where
  pretty (CCircuit ninput nodes oidx) =
    vsep
      [ nest 2 $
          vsep
            [ prettyPrintRegisters
                (mkCIndex ninput <$> [0 .. ninput - 1] :: [idx])
                <+> "=>"
                <+> lbrace,
              vsep (pretty <$> sortOn (head . cnodeIdx) nodes),
              "return"
                <+> ( if length oidx == 1
                        then "r" <> pretty (head oidx)
                        else prettyPrintRegisters oidx
                    )
            ],
        rbrace
      ]
      -}

deriving via
  (Default (CCircuit cop cidx))
  instance
    (ToCon op cop, ToCon idx cidx) =>
    ToCon (Circuit op idx) (CCircuit cop cidx)

interpretCCircuit ::
  forall cidx cfm cop c e.
  (CIndex cidx, CSemMap cfm cop e c) =>
  [c] ->
  CCircuit cop cidx ->
  cfm ->
  Either e [c]
interpretCCircuit inputs c@(CCircuit inames nodes oidx) sem | length inames /= length inputs = error "Bad inputs"
interpretCCircuit inputs c@(CCircuit inames nodes oidx) sem =
  go inputs (sortOn (head . cnodeIdx) nodes)
  where
    go l [] = traverse (atCIndex (error "Bad circuit") l) oidx
    go l (CNode _ o _ : _) | mkCIndex (length inames) (length l) /= head o = error "Bad circuit"
    go l (CNode op o i : xs) = do
      next <- applyCOp (opCSem sem op) $ fromRight (error "Bad circuit") . atCIndex (error "Bad circuit") l <$> i
      go (l ++ next) xs
