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


foldVars :: Graph ->  Graph
-- invalidates connections
foldVars (funs, conns) =
  (evalState stateMonad M.empty, conns)
  where
    stateMonad = collectSubs >> applySubs

    collectSubs = sequence_ $ collectSubsFun <$> funs
    applySubs = sequence $ applySubsFun <$> funs


applySubsFun :: FunGraph -> FoldMonad FunGraph
applySubsFun (topDef, labs, labMap) = do
  blockList <- sequence $ applySubsBlock <$> (M.toList labMap)
  return (topDef, labs, M.fromList blockList)



applySubsBlock :: QBlock -> FoldMonad QBlock
applySubsBlock (lab, []) = return (lab, [])

applySubsBlock (lab, h:t) = do
  (_, t') <- applySubsBlock (lab, t)
  mh' <- applySubsQuad h
  case mh' of
    Nothing -> return (lab, t')
    Just h' -> return (lab, h':t')


applySubsQuad :: Quad -> FoldMonad (Maybe Quad)
applySubsQuad (QAss a1 a2) = do
  has <- hasSub a1
  if has
    then return Nothing
    else do
      a2' <- getLastSub a2
      return $ Just $ QAss a1 a2'

applySubsQuad (QRet a) = do
  a' <- getLastSub a
  return $ Just $ QRet a'

applySubsQuad (QNeg a1 a2) = do
  has <- hasSub a1
  if has
    then return Nothing
    else do
      a2' <- getLastSub a2
      return $ Just $ QNeg a1 a2'

applySubsQuad (QOp a1 a2 op a3) = do
  has <- hasSub a1
  if has
    then return Nothing
    else do
      a2' <- getLastSub a2
      a3' <- getLastSub a3
      return $ Just $ QOp a1 a2' op a3'

applySubsQuad (QCall a lab as) = do
  as' <- sequence $ getLastSub <$> as
  return $ Just $ QCall a lab as'

applySubsQuad (QVCall lab as) = do
  as' <- sequence $ getLastSub <$> as
  return $ Just $ QVCall lab as'

applySubsQuad (QPhi a rs) = do
  has <- hasSub a
  if has
    then return Nothing
    else do
      rs' <- sequence $ (\(a, lab) -> do
          a' <- getLastSub a
          return (a', lab)) <$> rs
      return $ Just $ QPhi a rs'

applySubsQuad (QJCond cond lab) = do
  cond' <- applySubsCond cond
  if cond' == true
    then
      return $ Just $ QJmp lab
    else if cond' == false
      then
        return Nothing
      else
        return $ Just $ QJCond cond' lab

applySubsQuad q = return $ Just q


applySubsCond :: Cond -> FoldMonad Cond
applySubsCond cond'@(Comp a1 rel a2) = do
  a1' <- getLastSub a1
  a2' <- getLastSub a2
  let op = getComp rel
  let op' = getComp rel
  let cond' = Comp a1' rel a2'
  let cond'' = case cond' of
        --string comparison by address
        Comp (CString s1) (EQU ()) (CString s2) -> false
        Comp (CString s1) (NE ()) (CString s2) -> true
        Comp (CInt n1) _ (CInt n2) -> boolCond (op' n1 n2)
        Comp CTrue _ CTrue -> boolCond (op True True)
        Comp CTrue _ CFalse -> boolCond (op True False)
        Comp CFalse _ CTrue -> boolCond (op False True)
        Comp CFalse _ CFalse -> boolCond (op False False)
        Comp (Var s1) (EQU ()) (Var s2) ->
          if s1 == s2
            then true
            else cond'
        Comp (Var s1) (NE ()) (Var s2) ->
          if s1 == s2
            then false
            else cond'
        c@(Comp (Var _) _ (CTrue)) -> varBool c
        c@(Comp (Var _) _ (CFalse)) -> varBool c
        c@(Comp (CTrue) _ (Var _)) -> varBool c
        c@(Comp (CFalse) _ (Var _)) -> varBool c
  return cond''




applySubsCond (ValTrue a) = do
  a' <- getLastSub a
  return $ ValTrue a'

applySubsCond (ValFalse a) = do
  a' <- getLastSub a
  case a' of
    CTrue -> return false
    CFalse -> return true
    otherwise -> return $ ValFalse a'

varBool :: Cond -> Cond
varBool (Comp v rel CTrue) = varBool $ Comp CTrue rel v
varBool (Comp v rel CFalse) = varBool $ Comp CFalse rel v
varBool (Comp CTrue (EQU ()) v) = ValTrue v
varBool (Comp CTrue (NE ()) v) = ValFalse v
varBool (Comp CFalse (EQU ()) v) = ValFalse v
varBool (Comp CFalse (NE ()) v) = ValTrue v




boolCond :: Bool -> Cond
boolCond True = true
boolCond False = false

getComp :: Ord b => RelOp a -> (b -> b -> Bool)
getComp (EQU _) = (==)
getComp (NE _) = (/=)
getComp (LTH _) = (<)
getComp (LE _) = (<=)
getComp (GTH _) = (>)
getComp (GE _) = (>=)

true :: Cond
true = ValTrue CTrue

false :: Cond
false = ValTrue CFalse


hasSub :: Atom -> FoldMonad Bool
hasSub a = do
  subs <- get
  return $ M.lookup a subs /= Nothing


getLastSub :: Atom -> FoldMonad Atom
getLastSub a = do
  subs <- get
  case M.lookup a subs of
    Nothing -> return a
    Just a' -> getLastSub a'


collectSubsFun :: FunGraph -> FoldMonad ()
collectSubsFun (topDef, labs, labMap) = do
  sequence_ $ collectSubsBlock <$> labMap

collectSubsBlock :: [Quad] -> FoldMonad ()
collectSubsBlock block = do
  sequence_ $ collectSubsQuad <$> block

collectSubsQuad :: Quad -> FoldMonad ()
-- don't fold strings because of possible comparison by address
collectSubsQuad (QAss a1 (CString _)) = return ()

collectSubsQuad (QAss a1 a2) = do
  modify $ M.insert a1 a2

collectSubsQuad (QNeg a1 (CInt n)) = do
  modify $ M.insert a1 (CInt (-n))

collectSubsQuad (QOp a1 (CInt n1) op (CInt n2)) = do
  let (f, fpe) = case op of
        QPlus -> ((+), False)
        QMinus -> ((-), False)
        QTimes -> ((*), False)
        QDiv -> (div, True)
        QMod -> (mod, True)
  when ((not fpe) || n2 /= 0) $ do
    modify $ M.insert a1 $ CInt $ f n1 n2

collectSubsQuad (QOp a1 (CString s1) QCon (CString s2)) = do
  modify $ M.insert a1 $ CString $ s1 ++ s2

collectSubsQuad (QPhi a rs) = do
  let start = fst $ head rs
  let same = all (\(a, _) -> a == start) rs
  when same $ do
    modify $ M.insert a start

collectSubsQuad _ = return ()

