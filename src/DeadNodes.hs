module DeadNodes where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Reader

import AbsLatte
import CommonLatte
import QuadCode
import Connections



type Visited = S.Set Label

type DfsMonad a = ReaderT Conns (State Visited) a


removeDeadNodes :: [QuadFun] -> Conns -> ([QuadFun], Conns)
removeDeadNodes quadCode conns = (quadCode', conns') where
  quadCode' =
    (\(fn, code) -> (fn, removeUnreachableLabs reachLabs code)) <$> quadCode

  conns' = findConns quadCode'

  reachLabs = reachableLabels quadCode conns


removeUnreachableLabs :: S.Set Label -> [QuadBlock] -> [QuadBlock]
removeUnreachableLabs reachLabs =
  filter (\(lab, _) -> S.member lab reachLabs)


reachableLabels :: [QuadFun] -> Conns -> S.Set Label
reachableLabels quadCode conns =
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


allLabels :: [QuadFun] -> [Label]
allLabels funs = concat $ allLabelsFun <$> funs where
  allLabelsFun (_, qBlocks) = fst $ unzip qBlocks


getNeighbours :: Label -> Conns -> [Label]
getNeighbours lab conns = neighbours where
  edges = succs conns
  neighbourSet = M.findWithDefault S.empty lab edges
  neighbours = S.toList neighbourSet


unvisitedNeighbours :: Label -> DfsMonad [Label]
unvisitedNeighbours lab = do
  visited <- get
  neighbours <- asks (getNeighbours lab)
  let isUnvisited l = S.notMember l visited
  return $ filter isUnvisited neighbours


markVisited :: Label -> DfsMonad ()
markVisited lab =
  modify $ S.insert lab

