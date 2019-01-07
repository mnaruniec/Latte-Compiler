module PhiUpdate where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import AbsLatte
import CommonLatte
import QuadCode
import GraphForm





updatePhi :: Graph -> Graph
updatePhi (funs, conns) = (funs', conns) where
  funs' = updatePhiFun reachable <$> funs
  reachable = S.fromList $ concatMap getLabelsFun funs
  getLabelsFun (_, _, labMap) = M.keys labMap


updatePhiFun :: S.Set Label -> FunGraph -> FunGraph
updatePhiFun reach (topDef, labs, labMap) = (topDef, labs, labMap') where
  labMap' = M.fromList $ updatePhiLab reach <$> M.toList labMap


updatePhiLab :: S.Set Label -> QuadBlock -> QuadBlock
updatePhiLab reach (lab, block) = (lab, block') where
  block' = updatePhiQuad reach <$> block


updatePhiQuad :: S.Set Label -> Quad -> Quad
updatePhiQuad reach (QPhi a rs) = result where
  result = case rs' of
    [(a', lab)] -> QAss a a'
    otherwise -> QPhi a rs'
  rs' = foldr updateRule [] rs
  updateRule r@(_, lab) acc = if S.member lab reach then r:acc else acc

updatePhiQuad _ q = q

