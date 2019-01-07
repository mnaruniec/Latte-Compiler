module BackEnd where

import AbsLatte

import CommonLatte


import QuadCode
import QuadGenerate
import QuadToBlocks
import NopRemoval
import Connections
import DeadNodes
import GraphForm
import LiveVars
import SSA
import AtomFold
import Optimize
import LLVM



backEnd :: Program () -> IO ()
backEnd p = do
      let (ctx, quadCode) = genQuad p
      sequence_ $ printList . snd <$> quadCode
      let blockFuns = quadToBlocks ctx quadCode
      putStrLn "\n\n"
      printQCode blockFuns
      let blockFuns' = removeNops blockFuns
      putStrLn "\n\nAfter nop elimination:\n"
      printQCode blockFuns'
      let connections = findConns blockFuns'
      putStrLn $ show connections
      putStrLn "\n\n"
--      let reachable = reachableLabels blockFuns' connections
--      putStrLn $ show reachable

      let (blockFuns'', connections') = removeDeadNodes blockFuns' connections
      putStrLn "\n\n"
      printQCode blockFuns''
      putStrLn $ show connections'

      let graph = toGraph blockFuns'' connections'
      putStrLn "\n\n"
      putStrLn $ show graph

      let liveMap = getLiveVars graph
      putStrLn "\n\n"
      putStrLn $ show liveMap

      let ssaGraph = makeSSA graph liveMap
      putStrLn "\n\n"
      printQCode $ fst $ fromGraph ssaGraph

      optGraph <- optimize ssaGraph
      putStrLn "\n\n"
      printQCode $ fst $ fromGraph optGraph

      let linear = fst $ fromGraph optGraph
      let llvmCode = getLLVM linear

      printStringList llvmCode

