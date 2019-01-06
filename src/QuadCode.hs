{-# LANGUAGE FlexibleContexts #-}

module QuadCode where

import Text.Read (readMaybe)
import qualified Data.Map.Strict as M
import Control.Monad.State

import AbsLatte
import CommonLatte


{- Basic blocks and functions in linear form -}

type QuadBlock = (Label, [Quad])

type QuadFun = (TopDef (), [QuadBlock])


{- Graph connections -}

type Edges = M.Map Label (M.Map Label Bool)

data Conns = Conns {preds :: Edges, succs :: Edges}
  deriving (Eq, Show)


{- Context for quadruple generation -}

data Context = Context {nextVar :: Integer, nextLab :: Integer}


{- Quadruple code -}

data Atom =
    CInt Integer
  | CString String
  | CTrue
  | CFalse
  | Var String
  | Undef
  deriving (Eq, Ord)

instance Show Atom where
  show (CInt n) = show n
  show (CString str) = show str
  show (CTrue) = "true"
  show (CFalse) = "false"
  show (Var str) = str
  show (Undef) = "UNDEF"


data Cond =
    Comp Atom (RelOp ()) Atom
  | ValTrue Atom
  | ValFalse Atom
  deriving (Eq, Ord)

instance Show Cond where
  show (Comp a1 rel a2) = show a1 ++ " " ++ show rel ++ " " ++ show a2
  show (ValTrue a) = show a
  show (ValFalse a) = "!(" ++ show a ++ ")"


data Label =
    LNum Integer
  | LFun String
  deriving (Eq, Ord)

instance Show Label where
  show (LNum n) = "L" ++ show n
  show (LFun str@(h:t)) =
    let rm = readMaybe t :: Maybe Integer in
      if h == 'L' && rm /= Nothing
        then "func_" ++ str
        else str


data Op = QPlus | QMinus | QTimes | QDiv | QMod | QCon
  deriving (Eq, Ord)

instance Show Op where
  show (QPlus) = "+"
  show (QMinus) = "-"
  show (QTimes) = "*"
  show (QDiv) = "/"
  show (QMod) = "%"
  show (QCon) = "++"


data Quad =
    QAss Atom Atom
  | QJmp Label
  | QJCond Cond Label
  | QVRet
  | QRet Atom
  | QNOp
  | QNeg Atom Atom
  | QOp Atom Atom Op Atom
  | QCall Atom Label [Atom]
  | QVCall Label [Atom]
  | QPhi Atom [(Atom, Label)]
  deriving (Eq, Ord)

instance Show Quad where
  show (QAss a1 a2) = show a1 ++ " := " ++ show a2
  show (QJmp l) = "goto " ++ show l
  show (QJCond c l) = "if (" ++ show c ++ ") goto " ++ show l
  show (QVRet) = "return"
  show (QRet a) = "return " ++ show a
  show (QNOp) = "nop"
  show (QNeg a1 a2) = show a1 ++ " := -(" ++ show a2 ++ ")"
  show (QOp a1 a2 op a3) =
    show a1 ++ " := " ++ show a2 ++ " " ++ show op ++ " " ++ show a3
  show (QCall a l as) = show a ++ " := " ++ show l ++ "(" ++ show as ++ ")"
  show (QVCall l as) = show l ++ "(" ++ show as ++ ")"
  show (QPhi a rs) = show a ++ " := phi " ++ show rs


data LQuad =
    NoL Quad
  | Lab Label Quad
  deriving (Eq, Ord)

instance Show LQuad where
  show (NoL q) = show q
  show (Lab l q) = show l ++ ": " ++ show q


{- Helper functions -}

neg :: RelOp a -> RelOp a
neg (LTH a) = (GE a)
neg (GE a) = (LTH a)
neg (GTH a) = (LE a)
neg (LE a) = (GTH a)
neg (EQU a) = (NE a)
neg (NE a) = (EQU a)


stripLab :: LQuad -> Quad
stripLab (Lab _ q) = q
stripLab (NoL q) = q


isJump :: Quad -> Bool
isJump (QJmp _) = True
isJump (QJCond _ _) = True
isJump _ = False


isLJump :: LQuad -> Bool
isLJump lq = isJump $ stripLab lq


getTarget :: Quad -> Label
getTarget (QJmp l) = l
getTarget (QJCond _ l) = l


getLTarget :: LQuad -> Label
getLTarget lq = getTarget $ stripLab lq


getLab :: MonadState Context m => m Label
getLab = do
  Context nV nL <- get
  put $ Context nV (nL + 1)
  return $ LNum nL


getVarNum :: MonadState Context m => m Integer
getVarNum = do
  Context nV nL <- get
  put $ Context (nV + 1) nL
  return nV


getVar :: MonadState Context m => m Atom
getVar = do
  n <- getVarNum
  return $ Var $ "t" ++ show n

