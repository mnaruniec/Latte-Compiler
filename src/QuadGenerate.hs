{-# LANGUAGE FlexibleContexts #-}

module QuadGenerate where

import qualified Data.DList as D
import Control.Monad.State
import Control.Monad.Writer

import AbsLatte
import CommonLatte
import QuadCode



type QuadMonad a = StateT Context (Writer (D.DList LQuad)) a


defVal :: Type a -> Expr a
defVal (Int a) = ELitInt a 0
defVal (Str a) = EString a ""
defVal (Bool a) = ELitFalse a


emit :: LQuad -> QuadMonad ()
emit = tell . D.singleton

emitLab :: Label -> Quad -> QuadMonad ()
emitLab l q = emit $ Lab l q

emitMLab :: Maybe Label -> Quad -> QuadMonad ()
emitMLab ml q =
  case ml of
    Nothing -> emit $ NoL q
    Just l -> emitLab l q

genQuad :: Program () -> (Context, [(TopDef (), [LQuad])])
genQuad (Program _ topDefs) = (ctx, zip topDefs quadCodes) where
  (ctx, quadCodes) = foldr quadFn (startContext, []) topDefs
  quadFn (FnDef _ t (Ident id) _ block) (ctx, tail) =
    (ctx', (D.toList $ dl'):tail)
    where
      (((), ctx'), dl) = runWriter (runStateT (quadBlock (Just $ LFun id) block) ctx)
      dl' = if (t == Void ()) then dl `mappend` (D.fromList [NoL QVRet]) else dl

  startContext = Context 0 0


quadBlock :: Maybe Label -> Block () -> QuadMonad ()
quadBlock mLab (Block () (stmt:t)) = do
  quadStmt mLab stmt
  quadBlock Nothing $ Block () t

quadBlock _ (Block () []) = return ()


quadCondJump :: Maybe Label -> Label -> Label -> Label -> Cond -> QuadMonad ()
quadCondJump mLab lThen lElse lNext comp@(Comp a1 rel a2)
  | lThen == lNext = do
      emitMLab mLab $ QJCond (Comp a1 (neg rel) a2) lElse
  | lElse == lNext = do
      emitMLab mLab $ QJCond comp lThen
  | otherwise = do
      emitMLab mLab $ QJCond comp lThen
      l <- getLab
      emitLab l $ QJmp lElse

quadCondJump mLab lThen lElse lNext cond@(ValTrue a)
  | lThen == lNext = do
      emitMLab mLab $ QJCond (ValFalse $ a) lElse
  | lElse == lNext = do
      emitMLab mLab $ QJCond cond lThen
  | otherwise = do
      emitMLab mLab $ QJCond cond lThen
      l <- getLab
      emitLab l $ QJmp lElse

quadCond :: Maybe Label -> Label -> Label -> Label -> Expr () -> QuadMonad ()
quadCond mLab lThen lElse lNext (Not () cond) =
  quadCond mLab lElse lThen lNext cond


quadCond mLab lThen _ lNext (ELitTrue ()) =
  emitMLab mLab $ if (lThen == lNext) then QNOp else QJmp lThen

quadCond mLab lThen lElse lNext (ELitFalse ()) =
  quadCond mLab lThen lElse lNext (Not () $ ELitTrue ())

quadCond mLab lThen lElse lNext (ERel () e1 rel e2) = do
  (mLab', a1) <- quadExpr mLab e1
  (mLab'', a2) <- quadExpr mLab' e2
  quadCondJump mLab'' lThen lElse lNext $ Comp a1 rel a2

quadCond mLab lThen lElse lNext (EAnd () c1 c2) = do
  lMid <- getLab
  quadCond mLab lMid lElse lMid c1
  quadCond (Just lMid) lThen lElse lNext c2

quadCond mLab lThen lElse lNext (EOr () c1 c2) = do
  lMid <- getLab
  quadCond mLab lThen lMid lMid c1
  quadCond (Just lMid) lThen lElse lNext c2

quadCond mLab lThen lElse lNext expr = do
  (mLab', a1) <- quadExpr mLab expr
  quadCondJump mLab' lThen lElse lNext $ ValTrue a1


quadStmt :: Maybe Label -> Stmt () -> QuadMonad ()
quadStmt mLab (Decl () t (item:tail)) = do
  let (id, expr) = case item of
        NoInit () id -> (id, defVal t)
        Init () id expr -> (id, expr)

  quadStmt mLab (Ass () id expr)
  quadStmt Nothing (Decl () t tail)

quadStmt _ (Decl () _ []) = return ()

quadStmt mLab (Ass () (Ident id) expr) = do
  (mLab', a) <- quadExpr mLab expr
  emitMLab mLab' $ QAss (Var id) a

quadStmt mLab (While () cond stmt) = do
  condLab <- getLab
  stmtLab <- getLab
  afterLab <- getLab
  emitMLab mLab $ QJmp condLab
  quadStmt (Just stmtLab) stmt
  quadCond (Just condLab) stmtLab afterLab afterLab cond
  emitLab afterLab QNOp

quadStmt mLab (Empty ()) =
  emitMLab mLab QNOp

quadStmt mLab (BStmt () block) = quadBlock mLab block

quadStmt mLab (Incr () (Ident id)) = do
  let v = Var id
  emitMLab mLab $ QOp v v QPlus $ CInt 1

quadStmt mLab (Decr () (Ident id)) = do
  let v = Var id
  emitMLab mLab $ QOp v v QMinus $ CInt 1

quadStmt mLab (VRet ()) =
  emitMLab mLab $ QVRet

quadStmt mLab (Ret () expr) = do
  (mLab', a) <- quadExpr mLab expr
  emitMLab mLab' $ QRet a

quadStmt mLab (CondElse () cond s1 s2) = do
  trueLab <- getLab
  falseLab <- getLab
  afterLab <- getLab
  quadCond mLab trueLab falseLab trueLab cond
  quadStmt (Just trueLab) s1
  emit $ NoL $ QJmp afterLab
  quadStmt (Just falseLab) s2
  emitLab afterLab QNOp

quadStmt mLab (Cond () cond stmt) = do
  trueLab <- getLab
  falseLab <- getLab
  quadCond mLab trueLab falseLab trueLab cond
  quadStmt (Just trueLab) stmt
  emitLab falseLab QNOp

quadStmt mLab (SExp () expr) = do
  (mLab', a) <- quadExpr mLab expr
  emitMLab mLab' QNOp


quadExpr :: Maybe Label -> Expr () -> QuadMonad (Maybe Label, Atom)
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

quadExpr mLab (EApp () (Ident id) es) = do
  (mLab', args) <- quadArg mLab es
  v <- getVar
  emitMLab mLab' $ QCall v (LFun id) args
  return (Nothing, v)
  where
    quadArg mLab (expr:t) = do
      (mLab', as) <- quadArg mLab t
      (mLab'', a) <- quadExpr mLab' expr
      return (mLab'', a:as)

    quadArg mLab [] = return (mLab, [])

quadExpr mLab (ELitTrue ()) = return (mLab, CTrue)

quadExpr mLab (ELitFalse ()) = return (mLab, CFalse)

quadExpr mLab (Not () (ELitTrue ())) =
  quadExpr mLab $ ELitFalse ()

quadExpr mLab (Not () (ELitFalse ())) =
  quadExpr mLab $ ELitTrue ()

quadExpr mLab (Not () (Not () expr)) =
  quadExpr mLab expr

quadExpr mLab cond = do
  lTrue <- getLab
  lFalse <- getLab
  lAfter <- getLab
  v <- getVar
  quadCond mLab lTrue lFalse lTrue cond
  emitLab lTrue $ QAss v $ CTrue
  emitLab lFalse $ QAss v $ CFalse
  emitLab lAfter QNOp
  return (Nothing, v)

