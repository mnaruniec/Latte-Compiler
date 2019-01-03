module NopLatte where

import qualified Data.DList as D
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

import AbsLatte
import CommonLatte
import QuadLatte
import GraphLatte


removeNops :: [BlockFun] -> [BlockFun]
removeNops = (removeNopsFun <$>) where
  removeNopsFun (fun, labs) = (fun, removeNopsLab <$> labs)

removeNopsLab :: QBlock -> QBlock
removeNopsLab (lab, (h:t)) = (lab, t'') where
  t'' = case h of
    QVRet -> [h]
    QRet _ -> [h]
    QNOp -> t'
    otherwise -> h:t'

  (_, t') = removeNopsLab (lab, t)

removeNopsLab (lab, []) = (lab, [])


