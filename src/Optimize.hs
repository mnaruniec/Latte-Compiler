module Optimize where

import AbsLatte

import CommonLatte


import QuadCode
import DeadNodes
import GraphForm
import LiveVars
import SSA
import AtomFold



optimize :: Graph -> IO Graph
optimize graph = do
      let (graph', changed) = foldAtoms graph
      putStrLn $ "after fold: \n"
      printQCode $ fst $ fromGraph graph'
      let (linear, conns) = fromGraph graph'
      let (linear', conns') = removeDeadNodes linear conns
      putStrLn $ "after dead nodes: \n"
      printQCode linear'
      let graph'' = toGraph linear' conns'
      let liveMap = getLiveVars graph''
      putStrLn $ "live vars: \n" ++ show liveMap
      --TODO deadCode
      --TODO phi update
      if changed
        then optimize graph''
        else return graph''




