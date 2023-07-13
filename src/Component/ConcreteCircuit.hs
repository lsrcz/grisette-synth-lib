{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Component.ConcreteCircuit where

import Component.Circuit
import Component.Index
import Component.SemMap
import Data.Either (fromRight)
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

prettyPrintRegisters :: Pretty idx => [idx] -> Doc ann
prettyPrintRegisters =
  encloseSep lparen rparen (comma <> space)
    . fmap (\x -> "r" <> pretty x)

instance (Pretty op, Pretty idx) => Pretty (CNode op idx) where
  pretty (CNode op idx iidx)
    | length idx == 1 =
        "r" <> pretty (head idx)
          <+> "="
          <+> pretty op
            <> prettyPrintRegisters iidx
    | otherwise =
        prettyPrintRegisters idx
          <+> "="
          <+> pretty op <> prettyPrintRegisters iidx

deriving via
  (Default (CNode cop cidx))
  instance
    (ToCon op cop, ToCon idx cidx) =>
    ToCon (Node op idx) (CNode cop cidx)

data CCircuit op idx = CCircuit
  { ccirInputNum :: Int,
    ccirNodes :: [CNode op idx],
    ccirOutputIdx :: [idx]
  }
  deriving (Show, Eq, Generic)

instance (Pretty op, Pretty idx, CIndex idx) => Pretty (CCircuit op idx) where
  pretty (CCircuit ninput nodes oidx) =
    vsep
      [ nest 2 $
          vsep
            [ prettyPrintRegisters
                (mkCIndex ninput <$> [0 .. ninput - 1] :: [idx])
                <+> "=>"
                <+> lbracket,
              vsep (pretty <$> sortOn (head . cnodeIdx) nodes),
              "return"
                <+> ( if length oidx == 1
                        then "r" <> pretty (head oidx)
                        else prettyPrintRegisters oidx
                    )
            ],
        rbracket
      ]

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
interpretCCircuit inputs c@(CCircuit ninput nodes oidx) sem | ninput /= length inputs = error "Bad inputs"
interpretCCircuit inputs c@(CCircuit ninput nodes oidx) sem =
  go inputs (sortOn (head . cnodeIdx) nodes)
  where
    go l [] = traverse (atCIndex (error "Bad circuit") l) oidx
    go l (CNode _ o _ : _) | mkCIndex ninput (length l) /= (head o) = error "Bad circuit"
    go l (CNode op o i : xs) = do
      next <- applyCOp (opCSem sem op) $ fromRight (error "Bad circuit") . atCIndex (error "Bad circuit") l <$> i
      go (l ++ next) xs
