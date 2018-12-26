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




frontEnd :: Program Location -> IO ()
frontEnd p = do
  let (_, typeCheck) = runWriter $ checkTypes p
  let errors = toList typeCheck
  if errors /= []
    then do
      putStrLn $ show errors
    else
      putStrLn $ printTree $ uniqueVars p


