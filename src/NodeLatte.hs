{-# LANGUAGE FlexibleContexts #-}

module NodeLatte where

import qualified Data.Map as M
import qualified Data.Set as S
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


type Visited = S.Set Label

type DfsMonad a = ReaderT Conns (State Visited) a


allLabels :: [BlockFun] -> [Label]

allLabels funs = concat $ allLabelsFun <$> funs where
  allLabelsFun (_, qBlocks) = fst $ unzip qBlocks


getNeighbours :: Label -> Conns -> [Label]
getNeighbours lab conns = neighbours where
  edges = succs conns
  neighbourMap = M.findWithDefault M.empty lab edges
  neighbours = fst $ unzip $ M.toList neighbourMap


unvisitedNeighbours :: Label -> DfsMonad [Label]
unvisitedNeighbours lab = do
  visited <- get
  neighbours <- asks (getNeighbours lab)
  let isUnvisited l = S.notMember l visited
  return $ filter isUnvisited neighbours

markVisited :: Label -> DfsMonad ()
markVisited lab =
  modify $ S.insert lab


-- TODO: unreachable functions, needless jumps
removeUnreachable :: Conns -> [BlockFun] -> [BlockFun]
removeUnreachable conns quadCode =
  (\(fn, code) -> (fn, removeUnreachableLabs reachLabs code)) <$> quadCode
  where
    reachLabs = reachableLabels conns quadCode


removeUnreachableLabs :: S.Set Label -> [QBlock] -> [QBlock]
removeUnreachableLabs reachLabs =
  filter (\(lab, _) -> S.member lab reachLabs)


reachableLabels :: Conns -> [BlockFun] -> S.Set Label
reachableLabels conns quadCode =
  execState (runReaderT (getReachable funLabs) conns) S.empty
  where
    allLabs = allLabels quadCode
    funLabs = filter isFunLab allLabs

    isFunLab (LFun _) = True
    isFunLab _ = False


getReachable :: [Label] -> DfsMonad ()
getReachable = sequence_ . (getReachableFun <$>) where
  getReachableFun lab = do
    markVisited lab
    getReachableLab lab


getReachableLab :: Label -> DfsMonad ()
getReachableLab lab = do
  ns <- unvisitedNeighbours lab
  sequence_ $ markVisited <$> ns
  sequence_ $ getReachableLab <$> ns


