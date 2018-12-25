module FrontLatte where

import AbsLatte
import Data.DList
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Writer

import TypeLatte


frontEnd :: Program Location -> IO ()
frontEnd p = do
  let (_, typeCheck) = runWriter $ checkTypes p
  putStrLn $ show $ toList typeCheck


