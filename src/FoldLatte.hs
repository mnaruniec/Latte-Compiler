{-# LANGUAGE FlexibleContexts #-}

module FoldLatte where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Reader

import AbsLatte
import CommonLatte
import QuadLatte
import GraphLatte
import EdgeLatte
import ToGraphLatte
import LiveLatte (LiveSet, LiveDesc(..), LiveMap, LiveMonad)

--type QBlock = (Label, [Quad])

--type BlockFun = (TopDef (), [QBlock])

--type Edges = M.Map Label (M.Map Label Bool)

--data Conns = Conns {preds :: Edges, succs :: Edges}
--  deriving (Eq, Show)

--type LabelMap = M.Map Label [Quad]

--type FunGraph = (TopDef (), [Label], LabelMap)

--type Graph = ([FunGraph], Conns)

--type LiveSet = S.Set Atom

--data LiveDesc = LiveDesc {end :: LiveSet, steps :: [LiveSet]}
  --deriving (Eq, Show)

--type LiveMap = M.Map Label LiveDesc

--type LiveMonad a = ReaderT (LabelMap, Conns) (State LiveMap) a
--

type Subs = M.Map Atom Atom

type FoldMonad a = State Subs a


makeSSA :: Graph ->  Graph
makeSSA (funs, conns) =
  (evalState stateMonad M.empty, conns)
  where
    stateMonad = collectSubs -- >>= applySubs

    collectSubs = sequence $ collectSubsFun <$> funs
    --globalSSA funs' = sequence $ readerMonad resolveUndefs <$> funs'


collectSubsFun :: FunGraph -> FoldMonad ()
collectSubsFun (topDef, labs, labMap) = do
  sequence_ $ collectSubsBlock <$> labMap

collectSubsBlock :: [Quad] -> FoldMonad ()
collectSubsBlock block = do
  sequence_ $ collectSubsQuad <$> block

collectSubsQuad :: Quad -> FoldMonad ()
collectSubsQuad (QAss a1 a2) = do
  modify $ M.insert a1 a2

collectSubsQuad (QNeg a1 CTrue)
  modify $ M.insert a1 CFalse

collectSubsQuad (QNeg a1 CFalse)
  modify $ M.insert a1 CTrue

collectSubsQuad (QOp a1 (CInt n1) op (CInt n2)) = do
  (f, fpe) = case op of
    QPlus -> ((+), False)
    QMinus -> ((-), False)
    QTimes -> ((*), False)
    QDiv -> ((/), True)
    QMod -> (mod, True)
  when ((not fpe) || n2 /= 0) $ do
    modify $ M.insert a1 $ CInt $ op n1 n2

collectSubsQuad (QOp a1 (CString s1) QCon (CString s2)) = do
  modify $ M.insert a1 $ CString $ s1 ++ s2




