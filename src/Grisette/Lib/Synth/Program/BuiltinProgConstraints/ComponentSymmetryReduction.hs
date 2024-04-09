{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Synth.Program.BuiltinProgConstraints.ComponentSymmetryReduction
  ( ComponentSymmetryReduction (..),
    directDep,
    indirectDepNextConstraint,
    angelicDistanceMatrix,
    unrelated,
    canonicalOrderConstraint,
    distanceMatrixConstraint,
  )
where

import Grisette
  ( GenSymSimple (simpleFresh),
    LogicalOp (symImplies, symNot, (.&&), (.||)),
    MonadFresh,
    MonadUnion,
    SEq ((./=), (.==)),
    SOrd ((.<), (.>), (.>=)),
    Solvable (con),
    SymBool,
    symAll,
    symAny,
    symAssertWith,
  )
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Program.ComponentSketch.Program (Stmt (stmtDisabled))
import qualified Grisette.Lib.Synth.Program.ComponentSketch.Program as Component
import Grisette.Lib.Synth.Program.ProgConstraints
  ( ProgConstraints (constrainProg),
  )
import Grisette.Lib.Synth.VarId (SymbolicVarId)

directDep ::
  (SymbolicVarId symVarId) =>
  Component.Stmt op symVarId ->
  Component.Stmt op symVarId ->
  SymBool
directDep src dest =
  symAny
    (uncurry (.==))
    [(srcResId, destArgId) | srcResId <- srcResIds, destArgId <- destArgIds]
    .&& symNot (stmtDisabled src)
    .&& symNot (stmtDisabled dest)
  where
    srcResIds = Component.stmtResIds src
    destArgIds = Component.stmtArgIds dest

angelicDistanceMatrix ::
  (SymbolicVarId symVarId, GenSymSimple () symVarId, MonadFresh ctx) =>
  Int ->
  ctx [[symVarId]]
angelicDistanceMatrix num =
  traverse
    ( \row ->
        traverse
          (\col -> if row == col then return (-1) else simpleFresh ())
          [0 .. num - 1]
    )
    [0 .. num - 1]

inboundConstraint ::
  (SymbolicVarId symVarId, MonadContext ctx, MonadUnion ctx) =>
  [[symVarId]] ->
  ctx ()
inboundConstraint mat =
  symAssertWith "Next matrix not in bound" $
    symAll inBound $
      concat mat
  where
    inBound i =
      symAny (.== i) (fromIntegral <$> (-1 : [1 .. length mat - 1]))
        .&& (i .>= -1)
        .&& (i .< fromIntegral (length mat))

disabledNextConstraint ::
  (SymbolicVarId symVarId, MonadContext ctx, MonadUnion ctx) =>
  [Component.Stmt op symVarId] ->
  [[symVarId]] ->
  ctx ()
disabledNextConstraint stmts distanceMatrix = do
  symAssertWith "Bad disabled next" $
    symAll
      ( \i ->
          stmtDisabled (stmts !! i)
            `symImplies` symAll
              ( \j ->
                  (distanceMatrix !! i !! j .== -1)
                    .&& (distanceMatrix !! j !! i .== -1)
              )
              [0 .. n - 1]
      )
      [0 .. n - 1]
  where
    n = length stmts

directDepNextConstraint ::
  (SymbolicVarId symVarId, MonadContext ctx, MonadUnion ctx) =>
  [Component.Stmt op symVarId] ->
  [[symVarId]] ->
  ctx ()
directDepNextConstraint stmts distanceMatrix =
  symAssertWith "Bad dependency" $
    symAll
      (uncurry cond)
      [(i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1], i /= j]
  where
    n = length stmts
    cond i j
      | i == j = con True
      | otherwise =
          directDep (stmts !! i) (stmts !! j)
            .== (distanceMatrix !! i !! j .== 1)

indirectDepNextConstraint ::
  (SymbolicVarId symVarId, MonadContext ctx, MonadUnion ctx) =>
  [[symVarId]] ->
  ctx ()
indirectDepNextConstraint distanceMatrix =
  symAssertWith "Bad dependency" $
    symAll
      (uncurry cond)
      [(i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1], i /= j]
  where
    n = length distanceMatrix
    cond i j
      | i == j = con True
      | otherwise =
          (distanceMatrix !! i !! j ./= -1 .&& distanceMatrix !! i !! j ./= 1)
            `symImplies` symAny
              ( \k ->
                  ( (distanceMatrix !! i !! j .> distanceMatrix !! i !! k)
                      .&& (distanceMatrix !! i !! j .> distanceMatrix !! k !! j)
                  )
                    .&& (distanceMatrix !! i !! k ./= -1)
                    .&& (distanceMatrix !! k !! j ./= -1)
              )
              [k | k <- [0 .. n - 1], k /= i, k /= j]

noDepNextConstraint ::
  (SymbolicVarId symVarId, MonadContext ctx, MonadUnion ctx) =>
  [[symVarId]] ->
  ctx ()
noDepNextConstraint distanceMatrix =
  symAssertWith "Bad dependency" $
    symAll
      (uncurry cond)
      [(i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1], i /= j]
  where
    n = length distanceMatrix
    cond i j
      | i == j = con True
      | otherwise =
          (distanceMatrix !! i !! j .== -1)
            `symImplies` symAll
              ( \k ->
                  (distanceMatrix !! i !! k .== -1)
                    .|| (distanceMatrix !! k !! j .== -1)
              )
              [k | k <- [0 .. n - 1], k /= i, k /= j]

distanceMatrixConstraint ::
  (SymbolicVarId symVarId, MonadContext ctx, MonadUnion ctx) =>
  [Component.Stmt op symVarId] ->
  [[symVarId]] ->
  ctx ()
distanceMatrixConstraint stmts distanceMatrix = do
  inboundConstraint distanceMatrix
  disabledNextConstraint stmts distanceMatrix
  directDepNextConstraint stmts distanceMatrix
  indirectDepNextConstraint distanceMatrix
  noDepNextConstraint distanceMatrix

unrelated :: (SymbolicVarId symVarId) => [[symVarId]] -> Int -> Int -> SymBool
unrelated mat i j = (mat !! i !! j .== -1) .&& (mat !! j !! i .== -1)

canonicalOrderConstraint ::
  (SymbolicVarId symVarId, MonadContext ctx, MonadUnion ctx) =>
  [Component.Stmt op symVarId] ->
  [[symVarId]] ->
  ctx ()
canonicalOrderConstraint stmts mat =
  symAssertWith "Bad order" $
    symAll
      (uncurry cond)
      [(i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1], i < j]
  where
    n = length stmts
    cond i j
      | i == j = con True
      | otherwise =
          unrelated mat i j
            `symImplies` ( head (Component.stmtResIds $ stmts !! i)
                             .< head (Component.stmtResIds $ stmts !! j)
                         )

data ComponentSymmetryReduction = ComponentSymmetryReduction

instance
  ( MonadContext ctx,
    MonadFresh ctx,
    SymbolicVarId symVarId,
    GenSymSimple () symVarId,
    MonadUnion ctx
  ) =>
  ProgConstraints
    ComponentSymmetryReduction
    (Component.Prog op symVarId ty)
    ctx
  where
  constrainProg _ prog = do
    mat <- angelicDistanceMatrix (length $ Component.progStmtList prog)
    distanceMatrixConstraint (Component.progStmtList prog) mat
    canonicalOrderConstraint (Component.progStmtList prog) mat
