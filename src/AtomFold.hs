{-# LANGUAGE FlexibleContexts #-}

module AtomFold where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Reader

import AbsLatte
import CommonLatte
import QuadCode
import Connections
import GraphForm



type Subs = M.Map Atom Atom

type FoldMonad a = State Subs a


foldAtoms :: Graph -> (Graph, Bool)
foldAtoms (funs, conns) = ((funs', conns'), changed)
  where
    (funs', changed) = evalState stateMonad M.empty
    linear = fst $ fromGraph (funs', conns)
    conns' = if changed then findConns linear else conns

    stateMonad = collectSubs >> applySubs
    collectSubs = sequence_ $ collectSubsFun <$> funs
    applySubs = do
      result <- sequence $ applySubsFun <$> funs
      let (funs', changedList) = unzip result
      return (funs', any id changedList)


applySubsFun :: FunGraph -> FoldMonad (FunGraph, Bool)
applySubsFun (topDef, labs, labMap) = do
  result <- sequence $ applySubsBlock <$> (M.toList labMap)
  let (blockList, changedList) = unzip result
  return ((topDef, labs, M.fromList blockList), any id changedList)


applySubsBlock :: QuadBlock -> FoldMonad (QuadBlock, Bool)
applySubsBlock (lab, []) = return ((lab, []), False)

applySubsBlock (lab, h:t) = do
  ((_, t'), chan1) <- applySubsBlock (lab, t)
  (mh', chan2) <- applySubsQuad h
  case mh' of
    Nothing -> return ((lab, t'), True)
    Just h' -> return ((lab, h':t'), chan1 || chan2)


applySubsQuad :: Quad -> FoldMonad (Maybe Quad, Bool)
applySubsQuad (QAss a1 a2) = do
  has <- hasSub a1
  if has
    then return (Nothing, True)
    else do
      a2' <- getLastSub a2
      return (Just $ QAss a1 a2', a2 /= a2')

applySubsQuad (QRet a) = do
  a' <- getLastSub a
  return (Just $ QRet a', a /= a')

applySubsQuad (QNeg a1 a2) = do
  has <- hasSub a1
  if has
    then return (Nothing, True)
    else do
      a2' <- getLastSub a2
      return (Just $ QNeg a1 a2', a2 /= a2')

applySubsQuad (QOp a1 a2 op a3) = do
  has <- hasSub a1
  if has
    then return (Nothing, True)
    else do
      a2' <- getLastSub a2
      a3' <- getLastSub a3
      return (Just $ QOp a1 a2' op a3', a2 /= a2' || a3 /= a3')

applySubsQuad (QCall a lab as) = do
  as' <- sequence $ getLastSub <$> as
  return (Just $ QCall a lab as', as /= as')

applySubsQuad (QVCall lab as) = do
  as' <- sequence $ getLastSub <$> as
  return (Just $ QVCall lab as', as /= as')

applySubsQuad (QPhi a rs) = do
  has <- hasSub a
  if has
    then return (Nothing, True)
    else do
      rs' <- sequence $ (\(a, lab) -> do
          a' <- getLastSub a
          return (a', lab)) <$> rs
      return (Just $ QPhi a rs', rs /= rs')

applySubsQuad (QJCond cond lab) = do
  cond' <- applySubsCond cond
  if cond' == true
    then
      return (Just $ QJmp lab, True)
    else if cond' == false
      then
        return (Nothing, True)
      else
        return (Just $ QJCond cond' lab, cond /= cond')

applySubsQuad q = return (Just q, False)


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

