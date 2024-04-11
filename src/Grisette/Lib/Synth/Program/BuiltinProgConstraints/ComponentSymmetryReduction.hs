{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Lib.Synth.Program.BuiltinProgConstraints.ComponentSymmetryReduction
  ( ComponentSymmetryReduction (..),
    ComponentStatementUnreorderable (..),
    componentStatementUnreorderable',
    canonicalOrderConstraint,
    statementsDirectDep,
    statementsAdjacent,
  )
where

import Grisette
  ( LogicalOp (symImplies, symNot, (.&&), (.||)),
    Mergeable,
    MonadUnion,
    SEq ((.==)),
    SimpleMergeable,
    Solvable (con),
    SymBool,
    mrgIf,
    mrgReturn,
    mrgTraverse,
    mrgTraverse_,
    simpleMerge,
    symAnd,
    symAny,
    symAssertWith,
  )
import Grisette.Lib.Synth.Context (MonadContext)
import Grisette.Lib.Synth.Operator.OpTyping (OpTyping)
import Grisette.Lib.Synth.Program.BuiltinProgConstraints.Liveliness
  ( ComponentUse (ComponentUse),
    Def (defDisabled, defId, defResource),
    Liveliness (Liveliness),
    LivelinessConstraint,
    Resource (conflict),
    componentProgDefs,
    componentProgUses,
    componentStmtDefs,
    componentStmtInvalidatingDefs,
    componentStmtUses,
  )
import Grisette.Lib.Synth.Program.ComponentSketch.Program (Stmt (stmtDisabled))
import qualified Grisette.Lib.Synth.Program.ComponentSketch.Program as Component
import qualified Grisette.Lib.Synth.Program.Concrete as Concrete
import Grisette.Lib.Synth.Program.ProgConstraints
  ( OpSubProgConstraints (constrainOpSubProg),
    ProgConstraints (constrainProg),
  )
import Grisette.Lib.Synth.VarId (SymbolicVarId)

class
  (MonadContext ctx, MonadUnion ctx) =>
  ComponentStatementUnreorderable constrObj op ty ctx
    | op -> ty
  where
  -- | Determines whether two statements in a program can be reordered without
  -- altering the program's semantics.
  --
  -- The first argument is an object that represents a set of constraints
  -- preventing statement reordering. You can combine multiple constraint sets
  -- by using a tuple of objects.
  --
  -- In most cases, you can simply use the predefined constraint objects without
  -- implementing this function yourself. However, if you need to introduce your
  -- own constraints, keep the following in mind:
  --
  -- The function takes a program and two indices representing the positions of
  -- the statements in the program's statement list. These indices do not
  -- necessarily correspond to the statements' actual positions in the
  -- (flattened) data flow graph, which are indicated by the variable
  -- IDs.
  --
  -- The function may return any value:
  --
  -- * If the two indices are the same.
  -- * If either index is out of bounds.
  -- * If the statement at the second index is not the immediate successor of
  --   the statement at the first index in the flattened data flow graph. The
  --   second statement is considered the immediate successor if its first
  --   return ID is 1 greater than the last return ID of the first statement.
  --   Note that this does not impose any constraints on the indices themselves,
  --   and this do not assume that the two statements are dependent on each
  --   other.
  -- * If either of the two statements is disabled.
  -- * If the second statement directly depends on the first statement.
  --
  -- This means that, the implementation can safely assume that the two
  -- statements are valid (i.e., their indices are within bounds), distinct,
  -- adjacent, enabled, and not directly dependent on each other.
  --
  -- The function is allowed to overestimate the unreorderability condition. In
  -- other words, it can return 'True' even if the two statements can actually
  -- be reordered. While this won't compromise the correctness of the synthesis,
  -- it may impact its efficiency and the quality of the generated code.
  -- However, the function must not return 'False' if the two statements are
  -- genuinely unreorderable.
  componentStatementUnreorderable ::
    (SymbolicVarId symVarId) =>
    constrObj ->
    Component.Prog op symVarId ty ->
    Int ->
    Int ->
    ctx SymBool

-- |
-- Determines the reorderability of two statements based on the following
-- criteria, in order of precedence:
--
-- * Returns any value if the two indices are the same (this case will never
--   occur in practice).
-- * Returns any value if either index is out of bounds (this case will never
--   occur in practice).
-- * Returns 'False' (indicating reorderability) if either of the two statements
--   is disabled.
-- * Returns 'True' (indicating unreorderability) if the second statement
--   directly depends on the first statement.
-- * Returns 'True' (indicating unreorderability) if
--   'componentStatementUnreorderable' determines that the two statements are
--   unreorderable.
-- * May return any value if the statement at the second index is not the
--   immediate successor of the statement at the first index. We will never try
--   to reorder them.
componentStatementUnreorderable' ::
  ( ComponentStatementUnreorderable constrObj op ty ctx,
    SymbolicVarId symVarId
  ) =>
  constrObj ->
  Component.Prog op symVarId ty ->
  Int ->
  Int ->
  ctx SymBool
componentStatementUnreorderable' obj prog i j
  | i == j = mrgReturn $ con True
  | i >= length (Component.progStmtList prog) = mrgReturn $ con True
  | j >= length (Component.progStmtList prog) = mrgReturn $ con True
  | otherwise =
      let firstStmt = Component.progStmtList prog !! i
          secondStmt = Component.progStmtList prog !! j
       in do
            unreorderable <- componentStatementUnreorderable obj prog i j
            mrgIf
              (stmtDisabled firstStmt .|| stmtDisabled secondStmt)
              (return $ con False)
              ( return $
                  unreorderable .|| statementsDirectDep firstStmt secondStmt
              )

-- | Constrains the program to have a canonical order of statements.
--
-- This function uses the reorderability information provided by
-- `ComponentStatementUnreorderable` type class. It ensures a canonical order
-- by asserting that for all pairs of statements @stmt0@, @stmt1@, if @stmt1@ is
-- the immediate successor of @stmt0@ in the flattened data flow graph, then
-- either
--
-- * they are unreorderable, or
-- * @stmt0@ has the smaller index than @stmt1@.
canonicalOrderConstraint ::
  forall constrObj op symVarId ty ctx.
  ( SymbolicVarId symVarId,
    MonadContext ctx,
    MonadUnion ctx,
    ComponentStatementUnreorderable constrObj op ty ctx
  ) =>
  constrObj ->
  Component.Prog op symVarId ty ->
  ctx ()
canonicalOrderConstraint obj prog = do
  conds <-
    mrgTraverse
      (uncurry cond)
      [(i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1], i > j]
  symAssertWith "Bad order" $ symAnd conds
  where
    stmts = Component.progStmtList prog
    n = length stmts
    cond i j
      | i == j = mrgReturn $ con True
      | otherwise = do
          unreorderable <-
            componentStatementUnreorderable' obj prog i j :: ctx SymBool
          mrgReturn $
            statementsAdjacent (stmts !! i) (stmts !! j)
              `symImplies` unreorderable

newtype ComponentSymmetryReduction constrObj
  = ComponentSymmetryReduction constrObj

instance
  ( ProgConstraints constrObj (Concrete.Prog op conVarId ty) ctx,
    OpSubProgConstraints (ComponentSymmetryReduction constrObj) op ctx
  ) =>
  ProgConstraints
    (ComponentSymmetryReduction constrObj)
    (Concrete.Prog op conVarId ty)
    ctx
  where
  constrainProg obj prog =
    mrgTraverse_ (constrainOpSubProg obj) $
      Concrete.stmtOp <$> Concrete.progStmtList prog

-- | Note that the program constraints imposed by the objects will also be
-- enforced.
instance
  ( ProgConstraints constrObj (Component.Prog op symVarId ty) ctx,
    OpSubProgConstraints (ComponentSymmetryReduction constrObj) op ctx,
    ComponentStatementUnreorderable constrObj op ty ctx,
    SymbolicVarId symVarId
  ) =>
  ProgConstraints
    (ComponentSymmetryReduction constrObj)
    (Component.Prog op symVarId ty)
    ctx
  where
  constrainProg obj@(ComponentSymmetryReduction constrObj) prog = do
    constrainProg constrObj prog
    canonicalOrderConstraint constrObj prog
    mrgTraverse_ (constrainOpSubProg obj) $
      Component.stmtOp <$> Component.progStmtList prog

instance
  (MonadContext ctx, MonadUnion ctx, OpTyping op ty ctx) =>
  ComponentStatementUnreorderable () op ty ctx
  where
  componentStatementUnreorderable _ _ _ _ = mrgReturn $ con False

instance
  ( LivelinessConstraint livelinessObj SymBool op ty res ctx,
    SimpleMergeable res,
    Mergeable op,
    MonadUnion ctx
  ) =>
  ComponentStatementUnreorderable
    (Liveliness SymBool livelinessObj)
    op
    ty
    ctx
  where
  componentStatementUnreorderable
    (Liveliness obj)
    prog
    firstStmtPos
    secondStmtPos
      | firstStmtPos >= length stmtList
          || secondStmtPos >= length stmtList
          || firstStmtPos == secondStmtPos =
          mrgReturn $ con True
      | otherwise = do
          let first = stmtList !! firstStmtPos
              second = stmtList !! secondStmtPos
              otherStmts =
                fmap snd $
                  filter (\(i, _) -> i /= firstStmtPos && i /= secondStmtPos) $
                    zip [0 ..] stmtList
              removedProg = prog {Component.progStmtList = otherStmts}
          firstInvalidated <- componentStmtInvalidatingDefs obj first
          secondInvalidated <- componentStmtInvalidatingDefs obj second
          firstUses <- componentStmtUses obj first
          secondDefs <- componentStmtDefs obj second
          otherDefs <- componentProgDefs obj removedProg
          otherUses <- componentProgUses obj removedProg
          mrgReturn $
            symAny
              (firstUsedDefInvalidatedBySecond firstUses secondInvalidated)
              otherDefs
              .|| symAny
                (usedSecondDefInvalidatedByFirst secondDefs firstInvalidated)
                otherUses
      where
        stmtList = Component.progStmtList prog
        defInvalidated invalidatingDef def =
          simpleMerge $
            symAny
              ( \i ->
                  symNot (defDisabled def)
                    .&& symNot (defDisabled i)
                    .&& conflict (defResource i) (defResource def)
              )
              <$> invalidatingDef
        firstUsedDefInvalidatedBySecond' firstUses secondInvalidated otherDef =
          simpleMerge $ do
            uses <- firstUses
            let defIsUsed =
                  symAny
                    ( \(ComponentUse useId _ disabled) ->
                        symNot disabled .&& useId .== defId otherDef
                    )
                    uses
            return $
              defIsUsed .&& defInvalidated secondInvalidated otherDef
        firstUsedDefInvalidatedBySecond firstUses secondInvalidated otherDef =
          simpleMerge $
            symAny
              (firstUsedDefInvalidatedBySecond' firstUses secondInvalidated)
              <$> otherDef
        defUsed uses def =
          simpleMerge $
            symAny
              ( \(ComponentUse useId _ disabled) ->
                  symNot disabled .&& (useId .== defId def)
              )
              <$> uses
        usedSecondDefInvalidatedByFirst secondDefs firstInvalidated otherUse =
          simpleMerge $
            symAny
              ( \def ->
                  defUsed otherUse def
                    .&& defInvalidated firstInvalidated def
              )
              <$> secondDefs

instance
  ( ComponentStatementUnreorderable constrObj1 op ty ctx,
    ComponentStatementUnreorderable constrObj2 op ty ctx
  ) =>
  ComponentStatementUnreorderable
    (constrObj1, constrObj2)
    op
    ty
    ctx
  where
  componentStatementUnreorderable (obj1, obj2) prog i j = do
    unreorderable1 <- componentStatementUnreorderable obj1 prog i j
    unreorderable2 <- componentStatementUnreorderable obj2 prog i j
    mrgReturn $ unreorderable1 .|| unreorderable2

instance
  ( ComponentStatementUnreorderable constrObj1 op ty ctx,
    ComponentStatementUnreorderable constrObj2 op ty ctx,
    ComponentStatementUnreorderable constrObj3 op ty ctx
  ) =>
  ComponentStatementUnreorderable
    (constrObj1, constrObj2, constrObj3)
    op
    ty
    ctx
  where
  componentStatementUnreorderable (obj1, obj2, obj3) prog i j = do
    unreorderable1 <- componentStatementUnreorderable obj1 prog i j
    unreorderable2 <- componentStatementUnreorderable obj2 prog i j
    unreorderable3 <- componentStatementUnreorderable obj3 prog i j
    mrgReturn $ unreorderable1 .|| unreorderable2 .|| unreorderable3

statementsDirectDep ::
  (SymbolicVarId symVarId) =>
  Component.Stmt op symVarId ->
  Component.Stmt op symVarId ->
  SymBool
statementsDirectDep src dest =
  symAny
    (uncurry (.==))
    [ (srcResId, destArgId)
      | srcResId <- Component.stmtResIds src,
        destArgId <- Component.stmtArgIds dest
    ]
    .&& symNot (stmtDisabled src)
    .&& symNot (stmtDisabled dest)

statementsAdjacent ::
  (SymbolicVarId symVarId) =>
  Component.Stmt op symVarId ->
  Component.Stmt op symVarId ->
  SymBool
statementsAdjacent first second =
  last (Component.stmtResIds first) + 1 .== head (Component.stmtResIds second)
