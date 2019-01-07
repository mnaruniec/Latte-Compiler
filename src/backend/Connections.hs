{-# LANGUAGE FlexibleContexts #-}

module Connections where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State

import AbsLatte
import CommonLatte
import QuadCode



findConns :: [QuadFun] -> Conns
findConns funs = execState (sequence_ $ findConnsFun <$> funs) noConns where
  noConns = Conns M.empty M.empty


findConnsFun :: QuadFun -> State Conns ()
findConnsFun (_, labs) = sequence_ $ findConnsLab <$> labs


findConnsLab :: QuadBlock -> State Conns ()

findConnsLab (curr, []) = return ()

findConnsLab (curr, l) = do
  let lastComm = last l
  sequence_ $ addConnM curr <$> getTargets lastComm


addConn :: Label -> Label -> Conns -> Conns
addConn from to (Conns preds succs) = Conns preds' succs' where
  fromNeighbours = M.findWithDefault S.empty from succs
  toNeighbours = M.findWithDefault S.empty to preds

  fromNeighbours' = S.insert to fromNeighbours
  toNeighbours' = S.insert from toNeighbours

  succs' = M.insert from fromNeighbours' succs
  preds' = M.insert to toNeighbours' preds


addConnM :: MonadState Conns m => Label -> Label -> m ()
addConnM from to =
  modify $ addConn from to

