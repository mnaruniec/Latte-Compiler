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
      putStrLn $ show $ genQuad newTree

