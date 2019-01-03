{-# LANGUAGE FlexibleContexts #-}

module EdgeLatte where

import qualified Data.DList as D
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad.State

import AbsLatte
import CommonLatte
import QuadLatte
import GraphLatte

--type QBlock = (Label, [Quad])

--type BlockFun = (TopDef (), [QBlock])

type Edges = M.Map Label (M.Map Label Bool)

data Conns = Conns {preds :: Edges, succs :: Edges}
  deriving (Eq, Show)


addEdge :: Bool -> Label -> Label -> Conns -> Conns
addEdge fallthrough from to (Conns preds succs) = Conns preds' succs' where
  fromNeighbours = M.findWithDefault M.empty from succs
  toNeighbours = M.findWithDefault M.empty to preds

  fromNeighbours' = M.insert to fallthrough fromNeighbours
  toNeighbours' = M.insert from fallthrough toNeighbours

  succs' = M.insert from fromNeighbours' succs
  preds' = M.insert to toNeighbours' preds


addEdgeM :: MonadState Conns m => Bool -> Label -> Label -> m ()
addEdgeM fallthrough from to =
  modify $ addEdge fallthrough from to



findEdges :: [BlockFun] -> Conns
findEdges funs = execState (sequence_ $ findEdgesFun <$> funs) noConns where
  noConns = Conns M.empty M.empty


findEdgesFun :: BlockFun -> State Conns ()
findEdgesFun (_, labs) = foldM_ findEdgesLab Nothing labs


findEdgesLab :: Maybe Label -> QBlock -> State Conns (Maybe Label)
findEdgesLab (Just prev) qb@(curr, block) = do
  addEdgeM True prev curr
  findEdgesLab Nothing qb

findEdgesLab Nothing (curr, []) = return $ Just curr

findEdgesLab Nothing (curr, l) = do
  let lastComm = L.last l

  when (isJump lastComm) $ addEdgeM False curr $ getTarget lastComm

  return $ case lastComm of
    QVRet -> Nothing
    QRet _ -> Nothing
    QJmp _ -> Nothing
    otherwise -> Just curr



