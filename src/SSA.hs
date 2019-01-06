module SSA where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Reader

import AbsLatte
import CommonLatte
import QuadCode
import GraphForm
import LiveVars hiding (getLiveDesc, getLiveSteps, getLiveEnd, getLiveBeg)



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


makeSSALab :: Label -> SSAMonad QuadBlock
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




