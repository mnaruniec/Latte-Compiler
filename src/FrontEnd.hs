module FrontEnd where

import AbsLatte
import CommonLatte
import TypeCheck
import TreeTrans



frontEnd :: Program Location -> (Program (), [String])
frontEnd p = (p', errors) where
  errors = checkTypes p
  p' = if errors /= []
    then strip p
    else transTree p

