{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Component.SemMap where

import Control.Monad.Except
import Data.ByteString qualified as B
import Data.HashMap.Strict qualified as M
import Data.Maybe
import Grisette

data NOperands
  = NOperands Int
  | CommutativeTwoOperands
  | ListOperands
  | CommutativeAssociativeListOperands

class X f

data OpSem g e a = OpSem
  { opGroup :: g,
    opOperands :: NOperands,
    opOutputNum :: Int,
    opFunc ::
      forall m.
      ( MonadError e m,
        MonadUnion m,
        MonadFresh m,
        Mergeable a
      ) =>
      [a] ->
      m [a]
  }

applyOp ::
  ( MonadError e m,
    MonadUnion m,
    MonadFresh m,
    Mergeable a
  ) =>
  OpSem g e a ->
  [a] ->
  m [a]
applyOp (OpSem _ _ _ f) = f

data OpCSem e a where
  OpCSem ::
    NOperands ->
    Int ->
    ( [a] ->
      Either e [a]
    ) ->
    OpCSem e a

applyCOp :: OpCSem e a -> [a] -> Either e [a]
applyCOp (OpCSem _ _ f) = f

data UniversalOpSem g e a = UniversalOpSem
  { uopGroup :: g,
    uopOperands :: NOperands,
    uopOutputNum :: Int,
    uopFunc ::
      forall m.
      ( MonadError e m,
        MonadUnion m,
        Mergeable a
      ) =>
      [a] ->
      m [a]
  }

promoteToOpSem :: UniversalOpSem g e a -> OpSem g e a
promoteToOpSem (UniversalOpSem g o on f) = OpSem g o on f

downgradeToOpCSem ::
  forall ce e c a g.
  (ToSym ce e, ToCon e ce, ToSym c a, ToCon a c, Mergeable e, Mergeable a) =>
  UniversalOpSem g e a ->
  OpCSem ce c
downgradeToOpCSem (UniversalOpSem _ o on f) =
  OpCSem
    o
    on
    ( \x -> case f (toSym x :: [a]) :: ExceptT e UnionM [a] of
        ExceptT (SingleU v) -> fromJust $ toCon v
        _ -> undefined
    )

class SemMap fm op g e a | fm -> op g e a where
  opSemMaybe :: fm -> op -> Maybe (OpSem g e a)
  opSem :: fm -> op -> OpSem g e a

class USemMap fm op g e a | fm -> op g e a where
  opUSemMaybe :: fm -> op -> Maybe (UniversalOpSem g e a)
  opUSem :: fm -> op -> UniversalOpSem g e a

class CSemMap fm op e a | fm -> op e a where
  opCSemMaybe :: fm -> op -> Maybe (OpCSem e a)
  opCSem :: fm -> op -> OpCSem e a

newtype SimpleOpSemMap op e a = SimpleOpSemMap
  { unSimpleOpSemMap :: M.HashMap op (OpSem op e a)
  }

newtype SimpleOpCSemMap op e a = SimpleOpCSemMap
  { unSimpleOpCSemMap :: M.HashMap op (OpCSem e a)
  }

data SimpleUniversalSemMap op e a ce c where
  SimpleUniversalSemMap ::
    ( ToSym ce e,
      ToCon e ce,
      ToSym c a,
      ToCon a c,
      Mergeable e,
      Mergeable c,
      Mergeable e,
      Mergeable a
    ) =>
    {unSimpleUniversalSemMap :: M.HashMap op (UniversalOpSem op e a)} ->
    SimpleUniversalSemMap op e a ce c

instance SemMap (SimpleOpSemMap B.ByteString e a) B.ByteString B.ByteString e a where
  opSemMaybe = flip M.lookup . unSimpleOpSemMap
  opSem = (M.!) . unSimpleOpSemMap

instance CSemMap (SimpleOpCSemMap B.ByteString e a) B.ByteString e a where
  opCSemMaybe = flip M.lookup . unSimpleOpCSemMap
  opCSem = (M.!) . unSimpleOpCSemMap

instance
  USemMap
    (SimpleUniversalSemMap B.ByteString e a ce c)
    B.ByteString
    B.ByteString
    e
    a
  where
  opUSemMaybe semMap opcode =
    M.lookup opcode (unSimpleUniversalSemMap semMap)
  opUSem semMap opcode =
    unSimpleUniversalSemMap semMap M.! opcode

instance
  SemMap
    (SimpleUniversalSemMap B.ByteString e a ce c)
    B.ByteString
    B.ByteString
    e
    a
  where
  opSemMaybe semMap opcode =
    promoteToOpSem <$> M.lookup opcode (unSimpleUniversalSemMap semMap)
  opSem semMap opcode =
    promoteToOpSem $ unSimpleUniversalSemMap semMap M.! opcode

instance CSemMap (SimpleUniversalSemMap B.ByteString e a ce c) B.ByteString ce c where
  opCSemMaybe (SimpleUniversalSemMap semMap) opcode =
    downgradeToOpCSem <$> M.lookup opcode semMap
  opCSem (SimpleUniversalSemMap semMap) opcode =
    downgradeToOpCSem $ semMap M.! opcode
