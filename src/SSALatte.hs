{-# LANGUAGE FlexibleContexts #-}

module SSALatte where

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

dividorSSA :: Char
dividorSSA = '#'

type LastMap = M.Map Atom Integer

type LastSnap = M.Map Label LastMap

type SSAMonad a = ReaderT (LabelMap, Conns) (State (LiveMap, LastMap, LastSnap)) a


makeSSA :: Graph -> LiveMap -> Graph
makeSSA (funs, conns) liveMap =
  (evalState (stateMonad) (liveMap, M.empty, M.empty), conns)
  where
    stateMonad = localSSA >>= globalSSA

    localSSA = sequence $ readerMonad makeSSAFun <$> funs
    globalSSA funs' = sequence $ readerMonad resolveUndefs <$> funs'

    readerMonad callback f@(topDef, labs, labMap) =
      runReaderT (callback f) (labMap, conns)


getIdent :: Atom -> Integer -> Atom
getIdent (Var v) n = Var $ v ++ [dividorSSA] ++ show n

getLastNum :: Atom -> SSAMonad Integer
getLastNum v = do
  (_, lastMap, _) <- get
  let last = M.findWithDefault 0 v lastMap
  return last

getLastVar :: Atom -> SSAMonad Atom
getLastVar v@(Var _) = do
  last <- getLastNum v
  return $ getIdent v last

getLastVar v = return v


getNewVar :: Atom -> SSAMonad Atom
getNewVar v@(Var _) = do
  (liveMap, lastMap, lastSnap) <- get
  last <- getLastNum v
  let last' = last + 1
  put (liveMap, M.insert v last' lastMap, lastSnap)
  return $ getIdent v $ last'

getNewVar v = return v


saveSnap :: Label -> SSAMonad ()
saveSnap lab = do
  (liveMap, lastMap, lastSnap) <- get
  put (liveMap, lastMap, M.insert lab lastMap lastSnap)

getSnap :: Label -> SSAMonad LastMap
getSnap lab = do
  (_, _, lastSnap) <- get
  return $ (M.!) lastSnap lab

getSnapVar :: Label -> Atom -> SSAMonad Atom
getSnapVar lab a = do
  let old = dropNum a
  snap <- getSnap lab
  let mLast = M.lookup old snap
  case mLast of
    Nothing -> return old
    Just last -> return $ getIdent old last


getBlock :: Label -> SSAMonad [Quad]
getBlock lab = do
  (labelMap, _) <- ask
  return $ (M.!) labelMap lab


getPreds :: Label -> SSAMonad [Label]
getPreds lab = do
  allPreds <- asks $ preds . snd
  let predsMap = M.findWithDefault M.empty lab allPreds
  return $ M.keys predsMap


getLiveDesc :: Label -> SSAMonad LiveDesc
getLiveDesc lab = do
  (liveMap, _, _) <- get
  case M.lookup lab liveMap of
    Nothing -> return $ LiveDesc {end = S.empty, steps = [S.empty]}
    Just liveDesc -> return liveDesc

getLiveSteps :: Label -> SSAMonad [LiveSet]
getLiveSteps lab = do
  LiveDesc _ steps <- getLiveDesc lab
  return steps

getLiveEnd :: Label -> SSAMonad LiveSet
getLiveEnd lab = do
  LiveDesc end _ <- getLiveDesc lab
  return end

getLiveBeg :: Label -> SSAMonad LiveSet
getLiveBeg lab = do
  steps <- getLiveSteps lab
  return $ head steps


undefAss :: Atom -> SSAMonad Quad
undefAss a = do
  a' <- getNewVar a
  return $ QAss a' Undef

undefPhi :: [Label] -> Atom -> SSAMonad Quad
undefPhi labs a = do
  a' <- getNewVar a
  return $ QPhi a' $ (\lab -> (Undef, lab)) <$> labs


strDropNum :: String -> String
strDropNum [] = []
strDropNum (c:t) =
  if c == dividorSSA then [] else c:(strDropNum t)

dropNum :: Atom -> Atom
dropNum (Var v) = Var $ strDropNum v


resolveUndefs :: FunGraph -> SSAMonad FunGraph
resolveUndefs (topDef, labs, _) = do
  blocks <- sequence $ resolveUndefsLab <$> labs
  return (topDef, labs, M.fromList blocks)

resolveUndefsLab lab = do
  preds <- getPreds lab
  block <- getBlock lab
  block' <- sequence $ resolveUndefsQuad preds <$> block
  return (lab, block')

resolveUndefsQuad preds (QAss a Undef) = do
  let old = dropNum a
  case preds of
    [] -> return $ QAss a old
    [pred] -> do
      a1 <- getSnapVar pred old
      return $ QAss a a1

resolveUndefsQuad _ (QPhi a rs) = do
  let old = dropNum a
  let resolveRule (Undef, lab) = do
        a1 <- getSnapVar lab old
        return (a1, lab)
  rs' <- sequence $ resolveRule <$> rs
  return $ QPhi a rs'

resolveUndefsQuad _ q = return q


makeSSAFun :: FunGraph -> SSAMonad FunGraph
makeSSAFun (topDef, labs, _) = do
  blocks <- sequence $ makeSSALab <$> labs
  return (topDef, labs, M.fromList blocks)


makeSSALab :: Label -> SSAMonad QBlock
makeSSALab lab = do
  liveBeg <- getLiveBeg lab
  let liveBeg' = S.toList liveBeg

  preds <- getPreds lab
  let predsSize = length preds
  prolog <- if predsSize > 1
    then
      sequence $ undefPhi preds <$> liveBeg'
    else
      sequence $ undefAss <$> liveBeg'

  block <- getBlock lab
  block' <- sequence $ makeSSAQuad <$> block
  saveSnap lab
  return (lab, prolog ++ block')


makeSSAQuad :: Quad -> SSAMonad Quad
makeSSAQuad (QAss a1 a2) = do
  a2' <- getLastVar a2
  a1' <- getNewVar a1
  return $ QAss a1' a2'

makeSSAQuad (QRet a) = do
  a' <- getLastVar a
  return $ QRet a'

makeSSAQuad (QNeg a1 a2) = do
  a2' <- getLastVar a2
  a1' <- getNewVar a1
  return $ QNeg a1' a2'

makeSSAQuad (QOp a1 a2 op a3) = do
  a3' <- getLastVar a3
  a2' <- getLastVar a2
  a1' <- getNewVar a1
  return $ QOp a1' a2' op a3'

makeSSAQuad (QCall a lab as) = do
  as' <- sequence $ getLastVar <$> as
  a' <- getNewVar a
  return $ QCall a' lab as'

makeSSAQuad (QVCall lab as) = do
  as' <- sequence $ getLastVar <$> as
  return $ QVCall lab as'

makeSSAQuad (QJCond cond lab) = do
  cond' <- makeSSACond cond
  return $ QJCond cond' lab

makeSSAQuad q = return q

makeSSACond (Comp a1 rel a2) = do
  a2' <- getLastVar a2
  a1' <- getLastVar a1
  return $ Comp a1' rel a2'

makeSSACond (ValTrue a) = do
  a' <- getLastVar a
  return $ ValTrue a'

makeSSACond (ValFalse a) = do
  a' <- getLastVar a
  return $ ValFalse a'


--getLiveFun :: FunGraph -> LiveMonad ()
--getLiveFun (_, labs, _) =
--  sequence_ $ getLiveLab Nothing <$> labs
--
--
--getLiveLab :: Maybe Label -> Label -> LiveMonad ()
--getLiveLab mPrev curr = do
--  (prevBeg, force) <- case mPrev of
--    Nothing -> return (S.empty, True)
--    Just lab -> getLiveBeg lab >>= (\x -> return (x, False))
--
--  oldEnd <- getLiveEnd curr
--  let newEnd = oldEnd `S.union` prevBeg
--
--  when (force || oldEnd /= newEnd) $ do
--    oldBeg <- getLiveBeg curr
--    newSteps <- getLiveBlock newEnd curr
--    modify $ M.insert curr $ LiveDesc {end = newEnd, steps = newSteps}
--    let newBeg = head newSteps
--
--    when (newBeg /= oldBeg) $ do
--      preds <- getPreds curr
--      sequence_ $ getLiveLab (Just curr) <$> preds
--
--
--getLiveBlock :: LiveSet -> Label -> LiveMonad [LiveSet]
--getLiveBlock end lab =
--  let
--    getLiveBlock' q acc@(end:_) =
--      let beg = getLiveQuad end q in beg:acc
--  in do
--    block <- getBlock lab
--    return $ foldr getLiveBlock' [end] block
--
--
--getLiveQuad :: LiveSet -> Quad -> LiveSet
--getLiveQuad end q = beg' where
--  (kill, use) = getKillUse q
--  kill' = filter isVar kill
--  use' = filter isVar use
--
--  beg = foldr (S.delete) end kill'
--  beg' = foldr (S.insert) beg use'
--
--
--getKillUse :: Quad -> ([Atom], [Atom])
--getKillUse (QAss a1 a2) = ([a1], [a2])
--
--getKillUse (QJCond cond _) = ([], getUse cond)
--
--getKillUse (QRet a) = ([], [a])
--
--getKillUse (QNeg a1 a2) = ([a1], [a2])
--
--getKillUse (QOp a1 a2 _ a3) = ([a1], [a2, a3])
--
--getKillUse (QCall a1 _ as) = ([a1], as)
--
--getKillUse (QVCall _ as) = ([], as)
--
--getKillUse _ = ([], [])
--
--
--getUse :: Cond -> [Atom]
--getUse (Comp a1 _ a2) = [a1, a2]
--
--getUse (ValTrue a) = [a]
--
--getUse (ValFalse a) = [a]
--
--
--
--isVar :: Atom -> Bool
--isVar (Var _) = True
--isVar _ = False






