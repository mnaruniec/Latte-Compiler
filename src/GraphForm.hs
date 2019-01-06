{-# LANGUAGE FlexibleContexts #-}

module GraphForm where

import qualified Data.Map.Strict as M

import AbsLatte
import CommonLatte
import QuadCode


type LabelMap = M.Map Label [Quad]

type FunGraph = (TopDef (), [Label], LabelMap)

type Graph = ([FunGraph], Conns)


toGraph :: [QuadFun] -> Conns -> Graph
toGraph funs conns = (funs', conns) where
  funs' = toGraphFun <$> funs


toGraphFun :: QuadFun -> FunGraph
toGraphFun (topDef, blocks) = (topDef, labels, labelMap) where
  labels = fst $ unzip blocks
  labelMap = foldl addBlock M.empty blocks

  addBlock :: M.Map Label [Quad] -> QuadBlock -> M.Map Label [Quad]
  addBlock acc (lab, quads) = M.insert lab quads acc


fromGraph :: Graph -> ([QuadFun], Conns)
fromGraph (funs, conns) = (funs', conns) where
  funs' = fromGraphFun <$> funs


fromGraphFun :: FunGraph -> QuadFun
fromGraphFun (topDef, labels, labelMap) = (topDef, blocks) where
  blocks = zip labels $ foldr pushBlock [] labels

  pushBlock lab t = case M.lookup lab labelMap of
    Nothing -> t
    Just block -> block:t




