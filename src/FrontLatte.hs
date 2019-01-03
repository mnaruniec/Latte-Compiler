module FrontLatte where

import AbsLatte
import Data.DList
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Writer

import CommonLatte
import TypeLatte
import PrintLatte
import VarsLatte
import QuadLatte
import GraphLatte
import NopLatte
import EdgeLatte
import NodeLatte


printList :: Show a => [a] -> IO ()
printList l = do
  sequence_ $ putStrLn . show <$> l

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


