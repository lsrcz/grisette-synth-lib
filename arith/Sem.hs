{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Sem where

import Component.OpPrettyPrinter
import Component.SemMap
import Control.Monad.Except
import Data.ByteString qualified as B
import Data.HashMap.Strict qualified as M
import Data.Hashable
import Data.List
import GHC.Generics
import Grisette
import Prettyprinter

data OpCode a
  = Plus
  | Mul
  | Minus
  | UMinus
  | PlusMinus
  | PlusN a
  deriving (Show, Generic)
  deriving (Mergeable, EvaluateSym) via (Default (OpCode a))

deriving via
  (Default (OpCode s))
  instance
    (ToSym c s) => ToSym (OpCode c) (OpCode s)

deriving via
  (Default (OpCode c))
  instance
    (ToCon s c) => ToCon (OpCode s) (OpCode c)

instance (Pretty a) => Pretty (OpCode a) where
  pretty = \case
    Plus -> "+"
    Mul -> "*"
    Minus -> "-"
    UMinus -> "uminus"
    PlusMinus -> "+-"
    PlusN n -> "add" <> parens (pretty n)

instance (Pretty a, Pretty idx, Hashable idx) => OpRegNamer (OpCode a) idx where
  nameOutputs = \case
    Plus -> simpleRegName "r"
    Mul -> simpleRegName "r"
    Minus -> simpleRegName "r"
    UMinus -> simpleRegName "r"
    PlusMinus -> simpleRegName "r"
    PlusN _ -> simpleRegName "sum"

instance OpPrinterDescription (OpCode a) where
  describeArgs (PlusN _) 1 = [Just "value"]
  opIsBinaryInfix (PlusN _) = False
  opIsBinaryInfix UMinus = False
  opIsBinaryInfix _ = True

arithSem :: SimpleOpSemMap B.ByteString VerificationConditions SymInteger
arithSem =
  SimpleOpSemMap $
    M.fromList
      [ ( "+",
          OpSem "+" CommutativeAssociativeListOperands 1 $ mrgReturn . singleton . foldl1' (+)
        ),
        ( "*",
          OpSem "*" CommutativeAssociativeListOperands 1 $ mrgReturn . singleton . foldl1' (*)
        ),
        ( "-",
          OpSem "-" (NOperands 2) 1 $
            \case
              [a, b] -> mrgReturn [a - b]
              _ -> mrgThrowError AssertionViolation
        ),
        ( "uminus",
          OpSem "uminus" (NOperands 1) 1 $
            \case
              [a] -> mrgReturn [-a]
        ),
        ( "+-",
          OpSem "+-" (NOperands 2) 2 $
            \case
              [a, b] -> return [a + b, a - b]
              _ -> throwError AssertionViolation
        )
      ]

arithCSem :: SimpleOpCSemMap B.ByteString VerificationConditions Integer
arithCSem =
  SimpleOpCSemMap $
    M.fromList
      [ ( "+",
          OpCSem CommutativeAssociativeListOperands 1 $ return . singleton . foldl1' (+)
        ),
        ( "*",
          OpCSem CommutativeAssociativeListOperands 1 $ return . singleton . foldl1' (*)
        ),
        ( "-",
          OpCSem (NOperands 2) 1 $
            \case
              [a, b] -> return [a - b]
              _ -> throwError AssertionViolation
        ),
        ( "uminus",
          OpCSem (NOperands 1) 1 $
            \case
              [a] -> return [-a]
              _ -> throwError AssertionViolation
        ),
        ( "+-",
          OpCSem (NOperands 2) 2 $
            \case
              [a, b] -> return [a + b, a - b]
              _ -> throwError AssertionViolation
        )
      ]

arithUSem :: SimpleUniversalSemMap B.ByteString VerificationConditions SymInteger VerificationConditions Integer
arithUSem =
  SimpleUniversalSemMap $
    M.fromList
      [ ( "+",
          UniversalOpSem "+" CommutativeAssociativeListOperands 1 $ mrgReturn . singleton . foldl1' (+)
        ),
        ( "*",
          UniversalOpSem "*" CommutativeAssociativeListOperands 1 $ mrgReturn . singleton . foldl1' (*)
        ),
        ( "-",
          UniversalOpSem "-" (NOperands 2) 1 $
            \case
              [a, b] -> mrgReturn [a - b]
              _ -> mrgThrowError AssertionViolation
        ),
        ( "uminus",
          UniversalOpSem "uminus" (NOperands 1) 1 $
            \case
              [a] -> mrgReturn [-a]
        ),
        ( "+-",
          UniversalOpSem "+-" (NOperands 2) 2 $
            \case
              [a, b] -> mrgReturn $ [a + b, a - b]
              _ -> mrgThrowError AssertionViolation
        )
      ]

getArithOpCodeUSem :: OpCode SymInteger -> UniversalOpSem B.ByteString VerificationConditions SymInteger
getArithOpCodeUSem = \case
  Plus -> opUSem arithUSem "+"
  Mul -> opUSem arithUSem "*"
  Minus -> opUSem arithUSem "-"
  UMinus -> opUSem arithUSem "uminus"
  PlusMinus -> opUSem arithUSem "+-"
  PlusN n -> UniversalOpSem "+n" (NOperands 1) 1 $
    \case
      [v] -> mrgReturn [v + n]
      _ -> mrgThrowError AssertionViolation

data USem = USem

instance SemMap USem (OpCode SymInteger) B.ByteString VerificationConditions SymInteger where
  opSem _ = promoteToOpSem . getArithOpCodeUSem
  opSemMaybe _ = Just . promoteToOpSem . getArithOpCodeUSem

instance CSemMap USem (OpCode Integer) VerificationConditions Integer where
  opCSem _ c = downgradeToOpCSem (getArithOpCodeUSem (toSym c :: OpCode SymInteger) :: UniversalOpSem B.ByteString VerificationConditions SymInteger)
  opCSemMaybe _ c =
    Just $ downgradeToOpCSem (getArithOpCodeUSem (toSym c :: OpCode SymInteger) :: UniversalOpSem B.ByteString VerificationConditions SymInteger)
