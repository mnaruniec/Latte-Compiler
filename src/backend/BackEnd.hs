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



backEnd :: Program () -> IO String
backEnd p = do
      let (ctx, quadCode) = genQuad p
      let blockFuns = quadToBlocks ctx quadCode
      let blockFuns' = removeNops blockFuns
      let connections = findConns blockFuns'
      let (blockFuns'', connections') = removeDeadNodes blockFuns' connections
      let graph = toGraph blockFuns'' connections'
      let liveMap = getLiveVars graph
      let ssaGraph = makeSSA graph liveMap
      optGraph <- optimize ssaGraph
      let linear = fst $ fromGraph optGraph
      let llvmCode = getLLVM linear
      return $ unlines llvmCode

