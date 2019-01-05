{-# LANGUAGE FlexibleContexts #-}

module ToGraphLatte where

import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Reader

import AbsLatte
import CommonLatte
import QuadLatte
import GraphLatte
import EdgeLatte

--type QBlock = (Label, [Quad])

--type BlockFun = (TopDef (), [QBlock])

--type Edges = M.Map Label (M.Map Label Bool)

--data Conns = Conns {preds :: Edges, succs :: Edges}
--  deriving (Eq, Show)

type LabelMap = M.Map Label [Quad]

type FunGraph = (TopDef (), [Label], LabelMap)

type Graph = ([FunGraph], Conns)


toGraph :: [BlockFun] -> Conns -> Graph
toGraph funs conns = (funs', conns) where
  funs' = toGraphFun <$> funs


toGraphFun :: BlockFun -> FunGraph
toGraphFun (topDef, blocks) = (topDef, labels, labelMap) where
  labels = fst $ unzip blocks
  labelMap = foldl addBlock M.empty blocks

  addBlock :: M.Map Label [Quad] -> QBlock -> M.Map Label [Quad]
  addBlock acc (lab, quads) = M.insert lab quads acc


fromGraph :: Graph -> ([BlockFun], Conns)
fromGraph (funs, conns) = (funs', conns) where
  funs' = fromGraphFun <$> funs


fromGraphFun :: FunGraph -> BlockFun
fromGraphFun (topDef, labels, labelMap) = (topDef, blocks) where
  blocks = zip labels $ foldr pushBlock [] labels

  pushBlock lab t = case M.lookup lab labelMap of
    Nothing -> t
    Just block -> block:t




