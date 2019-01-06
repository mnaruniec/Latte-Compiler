{-# LANGUAGE FlexibleContexts #-}

module Connections where

import qualified Data.DList as D
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad.State

import AbsLatte
import CommonLatte
import QuadCode



findConns :: [QuadFun] -> Conns
findConns funs = execState (sequence_ $ findConnsFun <$> funs) noConns where
  noConns = Conns M.empty M.empty


findConnsFun :: QuadFun -> State Conns ()
findConnsFun (_, labs) = foldM_ findConnsLab Nothing labs


findConnsLab :: Maybe Label -> QuadBlock -> State Conns (Maybe Label)
findConnsLab (Just prev) qb@(curr, block) = do
  addConnM True prev curr
  findConnsLab Nothing qb

findConnsLab Nothing (curr, []) = return $ Just curr

findConnsLab Nothing (curr, l) = do
  let lastComm = L.last l
  when (isJump lastComm) $ addConnM False curr $ getTarget lastComm
  return $ case lastComm of
    QVRet -> Nothing
    QRet _ -> Nothing
    QJmp _ -> Nothing
    otherwise -> Just curr


addConn :: Bool -> Label -> Label -> Conns -> Conns
addConn fallthrough from to (Conns preds succs) = Conns preds' succs' where
  fromNeighbours = M.findWithDefault M.empty from succs
  toNeighbours = M.findWithDefault M.empty to preds

  fromNeighbours' = M.insert to fallthrough fromNeighbours
  toNeighbours' = M.insert from fallthrough toNeighbours

  succs' = M.insert from fromNeighbours' succs
  preds' = M.insert to toNeighbours' preds


addConnM :: MonadState Conns m => Bool -> Label -> Label -> m ()
addConnM fallthrough from to =
  modify $ addConn fallthrough from to

