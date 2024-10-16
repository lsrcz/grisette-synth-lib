{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Synth.Program.Concrete.Builder
  ( buildProg,
    node,
    node',
  )
where

import Control.Monad.State (MonadState (get, put), State, execState, gets)
import qualified Data.HashMap.Lazy as M
import Data.Hashable (Hashable)
import Data.List (sortOn)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette (Mergeable (rootStrategy), MergingStrategy (NoStrategy))
import qualified Grisette.Lib.Synth.Program.Concrete.Program as Concrete
import Grisette.Lib.Synth.VarId (ConcreteVarId)

data NodeRef op ty = NodeRef
  { nodeRef :: Node op ty,
    nodeRetId :: Int
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable)

data ProgArg ty = ProgArg
  { progArgName :: T.Text,
    progArgTy :: ty
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable)

data Node op ty
  = ArgNode (ProgArg ty)
  | InteriorNode
      { _op :: op,
        _num :: Int,
        _args :: [NodeRef op ty],
        _pseudoDeps :: [NodeRef op ty]
      }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable)

isInteriorNode :: Node op ty -> Bool
isInteriorNode InteriorNode {} = True
isInteriorNode _ = False

instance Mergeable (Node op ty) where
  rootStrategy = NoStrategy

data ProgRes op ty = ProgRes
  { progResNode :: NodeRef op ty,
    progResTy :: ty
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable)

data Prog op ty = Prog
  { progArgList :: [ProgArg ty],
    progResList :: [ProgRes op ty]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable)

instance Mergeable (Prog op ty) where
  rootStrategy = NoStrategy

toConcreteProg ::
  forall varId op ty.
  (Hashable op, Eq op, Hashable ty, Eq ty, ConcreteVarId varId) =>
  Prog op ty ->
  Concrete.Prog op varId ty
toConcreteProg (Prog argList resList) =
  Concrete.Prog
    ( fmap
        ( \a@(ProgArg name ty) ->
            Concrete.ProgArg
              { Concrete.progArgName = name,
                Concrete.progArgId = nodeRefToVarId $ NodeRef (ArgNode a) 0,
                Concrete.progArgType = ty
              }
        )
        argList
    )
    ( fmap
        ( \(node, varIds) ->
            case node of
              ArgNode {} -> error "Impossible"
              InteriorNode nodeOp _ refs _ ->
                Concrete.Stmt
                  { Concrete.stmtOp = nodeOp,
                    Concrete.stmtArgIds = nodeRefToVarId <$> refs,
                    Concrete.stmtResIds = varIds
                  }
        )
        allInteriorNodesList
    )
    ( fmap
        ( \(ProgRes node ty) ->
            Concrete.ProgRes
              { Concrete.progResId = nodeRefToVarId node,
                Concrete.progResType = ty
              }
        )
        resList
    )
  where
    accessArgs :: State (M.HashMap (Node op ty) [varId]) ()
    accessArgs = mapM_ (accessNode . ArgNode) argList
    accessNode :: Node op ty -> State (M.HashMap (Node op ty) [varId]) [varId]
    accessNode node = do
      map <- get
      case M.lookup node map of
        Just varId -> return varId
        Nothing ->
          case node of
            ArgNode {} -> do
              put $ M.insert node [fromIntegral $ M.size map] map
              return [fromIntegral $ M.size map]
            InteriorNode _ num _ _ -> do
              let varIds =
                    fromIntegral . (sum (length <$> M.elems map) +)
                      <$> [0 .. num - 1]
              put $ M.insert node varIds map
              return varIds
    walkNodes :: Node op ty -> State (M.HashMap (Node op ty) [varId]) [varId]
    walkNodes a@ArgNode {} = accessNode a
    walkNodes i@(InteriorNode _ _ nodeRefs pseudoDeps) = do
      currVarIds <- gets (M.lookup i)
      case currVarIds of
        Just varIds -> return varIds
        Nothing -> do
          mapM_ (walkNodes . nodeRef) nodeRefs
          mapM_ (walkNodes . nodeRef) pseudoDeps
          accessNode i
    allNodes :: M.HashMap (Node op ty) [varId]
    allNodes = flip execState M.empty $ do
      accessArgs
      mapM_ (walkNodes . nodeRef . progResNode) resList

    nodeToVarIds :: Node op ty -> [varId]
    nodeToVarIds = (allNodes M.!)

    nodeRefToVarId :: NodeRef op ty -> varId
    nodeRefToVarId (NodeRef ref retId) = nodeToVarIds ref !! retId

    allInteriorNodesList :: [(Node op ty, [varId])]
    allInteriorNodesList =
      filter (isInteriorNode . fst) $
        sortOn (head . snd) $
          M.toList allNodes

buildProg ::
  (Hashable op, Eq op, Hashable ty, Eq ty, ConcreteVarId varId) =>
  [(T.Text, ty)] ->
  ([NodeRef op ty] -> [(NodeRef op ty, ty)]) ->
  Concrete.Prog op varId ty
buildProg argPairs f =
  toConcreteProg $ Prog args (uncurry ProgRes <$> f argRefs)
  where
    args = uncurry ProgArg <$> argPairs
    argRefs = fmap (flip NodeRef 0 . ArgNode) args

node :: op -> Int -> [NodeRef op ty] -> [NodeRef op ty]
node op num refs = node' op num refs []

node' :: op -> Int -> [NodeRef op ty] -> [NodeRef op ty] -> [NodeRef op ty]
node' op num refs pseudoDeps =
  [NodeRef (InteriorNode op num refs pseudoDeps) i | i <- [0 .. num - 1]]
