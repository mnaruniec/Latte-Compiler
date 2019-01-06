module NopRemoval where

import AbsLatte
import CommonLatte
import QuadCode



-- removes nops and statements after return
removeNops :: [QuadFun] -> [QuadFun]
removeNops = (removeNopsFun <$>) where
  removeNopsFun (fun, labs) = (fun, removeNopsLab <$> labs)

removeNopsLab :: QuadBlock -> QuadBlock
removeNopsLab (lab, (h:t)) = (lab, t'') where
  t'' = case h of
    QVRet -> [h]
    QRet _ -> [h]
    QNOp -> t'
    otherwise -> h:t'

  (_, t') = removeNopsLab (lab, t)

removeNopsLab (lab, []) = (lab, [])


