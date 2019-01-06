module BackEnd where

import AbsLatte
import Data.DList
import qualified Data.Map.M as M
import Control.Monad.Reader
import Control.Monad.Writer

import CommonLatte
import QuadLatte
import GraphLatte
import NopLatte
import EdgeLatte
import NodeLatte
import ToGraphLatte
import LiveLatte
import SSALatte
import FoldLatte




printBlockFun :: BlockFun -> IO ()
printBlockFun (FnDef _ _ (Ident id) _ _, blocks) = do
  putStrLn $ "Function " ++ id ++ ":\n"
  sequence_ $ printQBlock <$> blocks
  putStrLn ""

printQBlock :: QBlock -> IO ()
printQBlock (lab, quads) = do
  putStrLn $ show lab ++ ":"
  sequence_ $ putStrLn . show <$> quads
  putStrLn ""

printQCode :: [BlockFun] -> IO ()
printQCode = sequence_ . (printBlockFun <$>)


frontEnd :: Program Location -> IO ()
frontEnd p = do
  let (_, typeCheck) = runWriter $ checkTypes p
  let errors = toList typeCheck
  if errors /= []
    then do
      putStrLn $ show errors
    else do
      let newTree = uniqueVars p
      putStrLn $ printTree newTree
      let (ctx, quadCode) = genQuad newTree
      sequence_ $ printList . snd <$> quadCode
      let blockFuns = quadToBlocks ctx quadCode
      putStrLn "\n\n"
      printQCode blockFuns
      let blockFuns' = removeNops blockFuns
      putStrLn "\n\nAfter nop elimination:\n"
      printQCode blockFuns'

      let connections = findEdges blockFuns'
      putStrLn $ show connections
      putStrLn "\n\n"
      let reachable = reachableLabels connections blockFuns'
      putStrLn $ show reachable

      let blockFuns'' = removeUnreachable connections blockFuns'
      putStrLn "\n\n"
      printQCode blockFuns''
      let connections' = findEdges blockFuns''
      putStrLn "\n"
      putStrLn $ show connections'

      let graph = toGraph blockFuns'' connections'
      putStrLn "\n\n"
      putStrLn $ show graph

      let liveMap = getLive graph
      putStrLn "\n\n"
      putStrLn $ show liveMap

      let ssaGraph = makeSSA graph liveMap

      putStrLn "\n\n"
      printQCode $ fst $ fromGraph ssaGraph

      let optGraph = foldVars ssaGraph

      printQCode $ fst $ fromGraph optGraph






