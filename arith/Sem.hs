{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Sem where

import Component.SemMap
import Control.Monad.Except
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as M
import Data.List
import Grisette

arithSem :: SimpleOpSemMap B.ByteString VerificationConditions SymInteger
arithSem =
  SimpleOpSemMap $
    M.fromList
      [ ( "+",
          OpSem "+" CommutativeAssociativeListOperands $ mrgReturn . foldl1' (+)
        ),
        ( "*",
          OpSem "*" CommutativeAssociativeListOperands $ mrgReturn . foldl1' (*)
        ),
        ( "-",
          OpSem "-" (NOperands 2) $
            \case
              [a, b] -> mrgReturn $ a - b
              _ -> mrgThrowError AssertionViolation
        ),
        ( "uminus",
          OpSem "uminus" (NOperands 1) $
            \case
              [a] -> mrgReturn $ -a
        )
      ]

arithCSem :: SimpleOpCSemMap B.ByteString VerificationConditions Integer
arithCSem =
  SimpleOpCSemMap $
    M.fromList
      [ ( "+",
          OpCSem CommutativeAssociativeListOperands $ return . foldl1' (+)
        ),
        ( "*",
          OpCSem CommutativeAssociativeListOperands $ return . foldl1' (*)
        ),
        ( "-",
          OpCSem (NOperands 2) $
            \case
              [a, b] -> return $ a - b
              _ -> throwError AssertionViolation
        ),
        ( "uminus",
          OpCSem (NOperands 1) $
            \case
              [a] -> return $ -a
        )
      ]

arithUSem :: SimpleUniversalSemMap B.ByteString VerificationConditions SymInteger VerificationConditions Integer
arithUSem =
  SimpleUniversalSemMap $
    M.fromList
      [ ( "+",
          UniversalOpSem "+" CommutativeAssociativeListOperands $ mrgReturn . foldl1' (+)
        ),
        ( "*",
          UniversalOpSem "*" CommutativeAssociativeListOperands $ mrgReturn . foldl1' (*)
        ),
        ( "-",
          UniversalOpSem "-" (NOperands 2) $
            \case
              [a, b] -> mrgReturn $ a - b
              _ -> mrgThrowError AssertionViolation
        ),
        ( "uminus",
          UniversalOpSem "uminus" (NOperands 1) $
            \case
              [a] -> mrgReturn $ -a
        )
      ]
