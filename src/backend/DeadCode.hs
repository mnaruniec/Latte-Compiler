module DeadCode where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.Reader

import AbsLatte
import CommonLatte
import QuadCode
import GraphForm
import LiveVars



removeDeadCode :: Graph -> LiveMap -> (Graph, Bool)
removeDeadCode (funs, conns) liveMap = ((funs', conns), changed) where
  funs' = deadCodeFun liveMap <$> funs
  changed = funs /= funs'


deadCodeFun :: LiveMap -> FunGraph -> FunGraph
deadCodeFun liveMap (topDef, labs, labMap) = (topDef, labs, labMap') where
  labMap' = M.fromList $ deadCodeLab liveMap <$> M.toList labMap


deadCodeLab :: LiveMap -> QuadBlock -> QuadBlock
deadCodeLab liveMap (lab, block) = (lab, block') where
  liveDesc = (M.!) liveMap lab
  liveSteps = tail $ steps liveDesc
  block' = concatMap (uncurry deadCodeQuad) $ zip block liveSteps


deadCodeQuad :: Quad -> LiveSet -> [Quad]
deadCodeQuad q@(QAss a _) = do
  ifLive a q

deadCodeQuad q@(QNeg a _) = do
  ifLive a q

deadCodeQuad q@(QOp a _ _ _) = do
  ifLive a q

deadCodeQuad q@(QCall a lab as) = do
  live <- isLive a
  if live
    then return [q]
    else return [QVCall lab as]

deadCodeQuad q = return [q]


ifLive :: Atom -> Quad -> LiveSet -> [Quad]
ifLive a q = do
  live <- isLive a
  if live
    then return [q]
    else return []


isLive :: Atom -> LiveSet -> Bool
isLive a = do
  asks $ S.member a



