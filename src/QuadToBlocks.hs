module QuadToBlocks where

import qualified Data.DList as D
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

import AbsLatte
import CommonLatte
import QuadCode



type BlocksMonad a = WriterT (D.DList QuadBlock)
  (ReaderT (S.Set Label) (State Context)) a



quadToBlocks :: Context -> [(TopDef (), [LQuad])] -> [QuadFun]
quadToBlocks ctx quadCode =
  fst $ runState (sequence $ fnToBlocks <$> quadCode) ctx


getTargetLabels :: TopDef () -> [LQuad] -> S.Set Label
getTargetLabels (FnDef _ _ (Ident id) _ _) lQuads = foldr f start lQuads
  where
    start = S.singleton $ LFun id

    f lQuad acc =
      foldr S.insert acc (getLTargets lQuad)


isTarget :: Label -> BlocksMonad Bool
isTarget l = asks $ S.member l


fnToBlocks :: (TopDef (), [LQuad]) -> State Context (QuadFun)
fnToBlocks (fnDef, quadCode) = do
  let targets = getTargetLabels fnDef quadCode
  ((), dl) <- runReaderT (runWriterT $ lQuadToBlocks Nothing D.empty quadCode) targets
  return (fnDef, D.toList dl)


lQuadToBlocks :: Maybe Label -> D.DList Quad -> [LQuad] -> BlocksMonad ()
-- We start a new block, but need a label
lQuadToBlocks Nothing curr ((NoL q):tail) = do
  lab <- getLab
  lQuadToBlocks Nothing curr ((Lab lab q):tail)

-- We start a new block and have a label
lQuadToBlocks Nothing curr ((Lab lab q):tail) = do
  lQuadToBlocks (Just lab) curr ((NoL q):tail)

-- We continue old block, but might finish if we are jump target
lQuadToBlocks (Just l1) curr ((Lab l2 q):tail) = do
  l2Target <- isTarget l2
  if l2Target
    then do
      tell $ D.singleton (l1, D.toList curr)
      lQuadToBlocks (Just l2) D.empty $ (NoL q):tail
    else do
      let curr' = D.snoc curr q -- append one
      lQuadToBlocks (Just l1) curr' tail

-- We extend old block, finishing if we are a jump
lQuadToBlocks (Just lab) curr ((NoL q):tail) = do
  let curr' = D.snoc curr q
  if isJump q
    then do
      tell $ D.singleton (lab, D.toList curr')
      lQuadToBlocks Nothing D.empty tail
    else do
      lQuadToBlocks (Just lab) curr' tail

lQuadToBlocks (Just lab) curr [] = do
  tell $ D.singleton (lab, D.toList curr)

lQuadToBlocks Nothing _ [] = return ()

