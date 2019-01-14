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
      let (linear, conns) = fromGraph graph1
      let (linear', conns') = removeDeadNodes linear conns
      let graph2 = toGraph linear' conns'
      let graph3 = updatePhi graph2
      let liveMap = getLiveVars graph3
      let (graph4, changed2) = removeDeadCode graph3 liveMap
      if changed1 || changed2
        then optimize graph4
        else return graph4

