module Optimize where

import AbsLatte

import CommonLatte


import QuadCode
import DeadNodes
import GraphForm
import LiveVars
import SSA
import AtomFold
import PhiUpdate
import DeadCode



optimize :: Graph -> IO Graph
optimize graph = do
      let (graph1, changed1) = foldAtoms graph
--      putStrLn $ "\n\nafter fold: \n"
--      printQCode $ fst $ fromGraph graph1
      let (linear, conns) = fromGraph graph1
      let (linear', conns') = removeDeadNodes linear conns
--      putStrLn $ "\n\nafter dead nodes: \n"
--      printQCode linear'
      let graph2 = toGraph linear' conns'
      let graph3 = updatePhi graph2
--      putStrLn $ "\n\nafter phi update: \n"
--      printQCode $ fst $ fromGraph graph3
      let liveMap = getLiveVars graph3
--      putStrLn $ "\n\nlive vars: \n" ++ show liveMap
      let (graph4, changed2) = removeDeadCode graph3 liveMap
--      putStrLn $ "\n\nafter dead code: \n"
--      printQCode $ fst $ fromGraph graph4

      if changed1 || changed2
        then optimize graph4
        else return graph4




