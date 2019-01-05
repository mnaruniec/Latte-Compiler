{-# LANGUAGE FlexibleContexts #-}

module LiveLatte where

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

--type QBlock = (Label, [Quad])

--type BlockFun = (TopDef (), [QBlock])

--type Edges = M.Map Label (M.Map Label Bool)

--data Conns = Conns {preds :: Edges, succs :: Edges}
--  deriving (Eq, Show)

--type LabelMap = M.Map Label [Quad]

--type FunGraph = (TopDef (), [Label], LabelMap)

--type Graph = ([FunGraph], Conns)

type LiveSet = S.Set Atom

data LiveDesc = LiveDesc {end :: LiveSet, steps :: [LiveSet]}
  deriving (Eq, Show)

type LiveMap = M.Map Label LiveDesc

type LiveMonad a = ReaderT (LabelMap, Conns) (State LiveMap) a


getBlock :: Label -> LiveMonad [Quad]
getBlock lab = do
  (labelMap, _) <- ask
  return $ (M.!) labelMap lab


getPreds :: Label -> LiveMonad [Label]
getPreds lab = do
  allPreds <- asks $ preds . snd
  let predsMap = M.findWithDefault M.empty lab allPreds
  return $ M.keys predsMap


getLiveDesc :: Label -> LiveMonad LiveDesc
getLiveDesc lab = do
  liveMap <- get
  case M.lookup lab liveMap of
    Nothing -> return $ LiveDesc {end = S.empty, steps = [S.empty]}
    Just liveDesc -> return liveDesc

getLiveSteps :: Label -> LiveMonad [LiveSet]
getLiveSteps lab = do
  LiveDesc _ steps <- getLiveDesc lab
  return steps

getLiveEnd :: Label -> LiveMonad LiveSet
getLiveEnd lab = do
  LiveDesc end _ <- getLiveDesc lab
  return end

getLiveBeg :: Label -> LiveMonad LiveSet
getLiveBeg lab = do
  steps <- getLiveSteps lab
  return $ head steps


getLive :: Graph -> LiveMap
getLive (funs, conns) =
  execState (stateMonad) M.empty
  where
    stateMonad = sequence_ $ readerMonad <$> funs
    readerMonad f@(topDef, labs, labMap) =
      runReaderT (getLiveFun f) (labMap, conns)


getLiveFun :: FunGraph -> LiveMonad ()
getLiveFun (_, labs, _) =
  sequence_ $ getLiveLab Nothing <$> labs


getLiveLab :: Maybe Label -> Label -> LiveMonad ()
getLiveLab mPrev curr = do
  (prevBeg, force) <- case mPrev of
    Nothing -> return (S.empty, True)
    Just lab -> getLiveBeg lab >>= (\x -> return (x, False))

  oldEnd <- getLiveEnd curr
  let newEnd = oldEnd `S.union` prevBeg

  when (force || oldEnd /= newEnd) $ do
    oldBeg <- getLiveBeg curr
    newSteps <- getLiveBlock newEnd curr
    modify $ M.insert curr $ LiveDesc {end = newEnd, steps = newSteps}
    let newBeg = head newSteps

    when (newBeg /= oldBeg) $ do
      preds <- getPreds curr
      sequence_ $ getLiveLab (Just curr) <$> preds


getLiveBlock :: LiveSet -> Label -> LiveMonad [LiveSet]
getLiveBlock end lab =
  let
    getLiveBlock' q acc@(end:_) =
      let beg = getLiveQuad end q in beg:acc
  in do
    block <- getBlock lab
    return $ foldr getLiveBlock' [end] block


getLiveQuad :: LiveSet -> Quad -> LiveSet
getLiveQuad end q = beg' where
  (kill, use) = getKillUse q
  kill' = filter isVar kill
  use' = filter isVar use

  beg = foldr (S.delete) end kill'
  beg' = foldr (S.insert) beg use'


getKillUse :: Quad -> ([Atom], [Atom])
getKillUse (QAss a1 a2) = ([a1], [a2])

getKillUse (QJCond cond _) = ([], getUse cond)

getKillUse (QRet a) = ([], [a])

getKillUse (QIncr a) = ([a], [a])

getKillUse (QDecr a) = ([a], [a])

getKillUse (QNeg a1 a2) = ([a1], [a2])

getKillUse (QOp a1 a2 _ a3) = ([a1], [a2, a3])

getKillUse (QCall a1 _ as) = ([a1], as)

getKillUse (QVCall _ as) = ([], as)

getKillUse _ = ([], [])


getUse :: Cond -> [Atom]
getUse (Comp a1 _ a2) = [a1, a2]

getUse (ValTrue a) = [a]

getUse (ValFalse a) = [a]



isVar :: Atom -> Bool
isVar (Var _) = True
isVar _ = False




