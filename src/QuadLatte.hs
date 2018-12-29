module QuadLatte where

import Data.DList
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad.State
import Control.Monad.Writer

import AbsLatte
import CommonLatte

data Atom =
    CInt Integer
  | CString String
  | CBool Bool -- might not need
  | Var String
  deriving (Eq, Ord, Show)

data Cond =
    Comp Atom (RelOp ()) Atom
  | VarTrue Atom
  | VarFalse Atom
  deriving (Eq, Ord, Show)

data Op = QPlus | QMinus | QTimes | QDiv | QMod | QCon
  deriving (Eq, Ord, Show)

data Quad =
    QAss Atom Atom
  | QJmp Integer
  | QJCond Comp Integer
  | QParam Int Atom
  | QVRet
  | QRet Atom
  | QNOp
  | QIncr Atom
  | QDecr Atom
  | QNeg Atom Atom
  | QOp Atom Atom Op Atom
  deriving (Eq, Ord, Show)

data LQuad =
    NoL Quad
  | Lab Integer Quad
  deriving (Eq, Ord, Show)


data Context = Context {nextVar :: Integer, nextLab :: Integer}

type QuadMonad a = StateT Context (Writer (DList LQuad)) a


neg :: RelOp a -> RelOp a
neg (LTH a) = (GE a)
neg (GE a) = (LTH a)
neg (GTH a) = (LE a)
neg (LE a) = (GTH a)
neg (EQU a) = (NE a)
neg (NE a) = (EQU a)


maxInt :: Integer
maxInt = 2 ^ 31 - 1
minInt :: Integer
minInt = -(2 ^ 31)



emit :: LQuad -> QuadMonad ()
emit = tell . singleton

emitLab :: Integer -> Quad -> QuadMonad ()
emitLab l q = emit $ Lab l q

emitMLab :: Maybe Integer -> Quad -> QuadMonad ()
emitMLab ml q =
  case ml of
    Nothing -> emit $ NoL q
    Just l -> emitLab l q

getLab :: QuadMonad Integer
getLab = do
  Context nV nL <- get
  put $ Context nV (nL + 1)
  return nL

getVarNum :: QuadMonad Integer
getVarNum = do
  Context nV nL <- get
  put $ Context (nV + 1) nL
  return nV

getVar :: QuadMonad Atom
getVar = do
  n <- getVarNum
  return $ Var $ "t" ++ show n


genQuad :: Program () -> [(TopDef (), [LQuad])]
genQuad (Program _ topDefs) =
  zip topDefs $ quadFn <$> topDefs where
    --TODO label at beginning, retV at end, unique labels
    quadFn (FnDef _ _ _ _ block) = toList $
      execWriter (runStateT (quadBlock Nothing block) startContext)

    startContext = Context 0 0


quadBlock :: Maybe Integer -> Block () -> QuadMonad ()
quadBlock mLab (Block () (stmt:t)) = do
  quadStmt mLab stmt
  quadBlock Nothing $ Block () t

quadBlock _ (Block () []) = return ()


quadCondJump :: Maybe Integer -> Integer -> Integer -> Integer -> Expr () -> QuadMonad ()
quadCondJump mLab lThen lElse lNext comp@(Comp a1 rel a2)
  | lThen == lNext = do
      emitMLab mLab $ JCond (Comp a1 (neg rel) a2) lElse
  | lElse == lNext = do
      emitMLab mLab $ JCond comp lThen
  | otherwise = do
      emitMLab mLab $ JCond comp lThen
      emit $ NoL $ Jmp lElse

quadCondJump mLab lThen lElse lNext cond@(VarTrue (Var id))
  | lThen == lNext = do
      emitMLab mLab $ JCond (VarFalse $ Var id) lElse
  | lElse == lNext = do
      emitMLab mLab $ JCond cond lThen
  | otherwise = do
      emitMLab mLab $ JCond cond lThen
      emit $ NoL $ Jmp lElse





quadCond :: Maybe Integer -> Integer -> Integer -> Integer -> Expr () -> QuadMonad ()
quadCond mLab lThen lElse lNext (Neg () cond) =
  quadCond mLab lElse lThen lNext cond


quadCond mLab lThen _ lNext (ELitTrue ()) =
  emitMLab mLab $ if (lThen == lNext) then QNOp else QJmp lNext

quadCond mLab lThen lElse lNext (ELitFalse ()) =
  quadCond mLab lThen lElse lNext (Neg () $ ELitTrue ())

quadCond mLab lThen lElse lNext (ERel () e1 rel e2) = do
  (mLab', a1) <- quadExpr mLab e1
  (mLab'', a2) <- quadExpr mLab' e2
  quadCondJump mLab'' lThen lElse lNext $ Comp a1 rel a2

quadCond mLab lThen lElse lNext (EAnd () c1 c2) = do
  lMid <- getLab
  quadCond mLab lMid lElse lMid c1
--TODO check if mLab can be not assigned
  quadCond (Just lMid) lThen lElse lNext c2

quadCond mLab lThen lElse lNext (EOr () c1 c2) = do
  lMid <- getLab
  quadCond mLab lThen lMid lMid c1
--TODO check if mLab can be not assigned
  quadCond (Just lMid) lThen lElse lNext c2

--TODO might move to any expr pattern
quadCond mLab lThen lElse lNext (EVar () (Ident id)) =
  quadCondJump mLab lThen lElse lNext $ VarTrue $ Var id

quadCond mLab lThen lElse lNext expr =
  (mLab', a1) <- quadExpr mLab expr
  quadCondJump mLab' lThen lElse lNext $ VarTrue a1
--TODO check if there can be non Var atoms


quadStmt :: Maybe Integer -> Stmt () -> QuadMonad ()
quadStmt mLab (While () cond stmt) = do
  condLab <- getLab
  stmtLab <- getLab
  afterLab <- getLab
  emitMLab mLab $ QJmp condLab
  quadStmt (Just stmtLab) stmt
  quadCond (Just condLab) stmtLab afterLab afterLab cond


quadStmt mLab (Empty ()) =
  emitMLab mLab QNOp

quadStmt mLab (BStmt () block) = quadBlock mLab block

quadStmt mLab (Incr () (Ident id)) =
  emitMLab mLab $ QIncr $ Var id

quadStmt mLab (Decr () (Ident id)) =
  emitMLab mLab $ QDecr $ Var id

quadStmt mLab (VRet ()) =
  emitMLab mLab $ QVRet


quadStmt mLab stmt = return ()


quadExpr :: Maybe Integer -> Expr () -> QuadMonad (Maybe Integer, Atom)
quadExpr mLab (EVar () (Ident id)) = return (mLab, Var id)

quadExpr mLab (ELitInt () n) = return (mLab, CInt n)

quadExpr mLab (EString () str) = return (mLab, CString str)

quadExpr mLab (Neg () expr) = do
  (mLab', a) <- quadExpr mLab expr
  v <- getVar
  emitMLab mLab' $ QNeg v a
  return (Nothing, v)

quadExpr mLab (EMul () e1 op e2) = do
  (mLab', a1) <- quadExpr mLab e1
  (mLab'', a2) <- quadExpr mLab' e2
  let op' = case op of
    Times () -> QTimes
    Div () -> QDiv
    Mod () -> QMod
  v <- getVar
  emitMLab mLab'' $ QOp v a1 op' a2
  return (Nothing, v)

quadExpr mLab (EAdd () e1 op e2) = do
  (mLab', a1) <- quadExpr mLab e1
  (mLab'', a2) <- quadExpr mLab' e2
  let op' = case op of
    Plus () -> QPlus
    Minus () -> QMinus
  v <- getVar
  emitMLab mLab'' $ QOp v a1 op' a2
  return (Nothing, v)

quadExpr mLab (ECon () e1 e2) = do
  (mLab', a1) <- quadExpr mLab e1
  (mLab'', a2) <- quadExpr mLab' e2
  v <- getVar
  emitMLab mLab'' $ QOp v a1 QCon a2
  return (Nothing, v)

--quadExpr mLab (EMul () e1



