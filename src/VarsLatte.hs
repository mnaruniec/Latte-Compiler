module VarsLatte where

import AbsLatte
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

import CommonLatte


type PEnv = M.Map Ident (Type ())

type VEnv = M.Map Ident (Int, Type ())

data Env = Env {pEnv :: PEnv, vEnv :: VEnv}

type Next = M.Map Ident Int

type VarsMonad a = ReaderT Env (State Next) a


returnE :: a -> VarsMonad (Env, a)
returnE x = do
  env <- ask
  return (env, x)


newId :: Ident -> Int -> Ident
newId (Ident id) n = Ident $ id ++ "$" ++ show n


getNewId :: Ident -> VarsMonad Ident
getNewId id = do
  (id', t) <- getInfo id
  return id'


getInfo :: Ident -> VarsMonad (Ident, Type ())
getInfo id = do
  Just (n, t) <- asks $ (!? id) . vEnv
  return $ (newId id n, t)


getNext :: Ident -> VarsMonad Int
getNext id = do
  mNext <- gets (!? id)
  case mNext of
    Nothing -> do
      modify $ M.insert id 2
      return 1
    Just next -> do
      modify $ M.insert id $ next + 1
      return next


uniqueVars :: Program Location -> Program ()
uniqueVars (Program _ topDefs) =
  Program () $ uniqueFnDef pEnv <$> topDefs where
    pEnv = M.fromList $ fnDefToPEnv <$> topDefs'
    fnDefToPEnv (FnDef _ t id _ _) = (id, strip t)
    topDefs' = builtInDefs ++ topDefs


uniqueFnDef :: PEnv -> TopDef Location -> TopDef ()
uniqueFnDef pEnv (FnDef _ t id args block) = result where
  result = FnDef () (strip t) id args' block'

  vEnv = M.fromList $ argToVEnv <$> args
  env = Env pEnv vEnv
  next = M.fromList $ argToNext <$> args
  args' = argToArg' <$> args

  argToVEnv (Arg _ t id) = (id, (0, strip t))
  argToNext (Arg _ _ id) = (id, 1)
  argToArg' (Arg _ t id) = Arg () (strip t) $ newId id 0

  block' = evalState (runReaderT (uniqueBlock block) env) next


uniqueBlock :: Block Location -> VarsMonad (Block ())
uniqueBlock (Block _ stmts) = do
    stmts' <- uniqueBlock' stmts
    return $ Block () stmts'
  where
    uniqueBlock' [] = return []
    uniqueBlock' (stmt:t) = do
      (env', stmt') <- uniqueStmt stmt
      t' <- local (\_ -> env') $ uniqueBlock' t
      return $ stmt':t'


uniqueStmt :: Stmt Location -> VarsMonad (Env, Stmt ())
uniqueStmt (BStmt _ block) = uniqueBlock block >>= returnE . (BStmt ())

uniqueStmt (Ass _ id expr) = do
  (_, expr') <- uniqueExpr expr
  id' <- getNewId id
  returnE $ Ass () id' expr'

uniqueStmt (Incr _ id) = do
  id' <- getNewId id
  returnE $ Incr () id'

uniqueStmt (Decr _ id) = do
  id' <- getNewId id
  returnE $ Decr () id'

uniqueStmt (Ret _ expr) = do
  (_, expr') <- uniqueExpr expr
  returnE $ Ret () expr'

uniqueStmt (Cond _ expr stmt) = do
  (_, expr') <- uniqueExpr expr
  (_, stmt') <- uniqueStmt stmt
  returnE $ Cond () expr' stmt'

uniqueStmt (CondElse _ expr stmt1 stmt2) = do
  (_, expr') <- uniqueExpr expr
  (_, stmt1') <- uniqueStmt stmt1
  (_, stmt2') <- uniqueStmt stmt2
  returnE $ CondElse () expr' stmt1' stmt2'

uniqueStmt (While _ expr stmt) = do
  (_, expr') <- uniqueExpr expr
  (_, stmt') <- uniqueStmt stmt
  returnE $ While () expr' stmt'

uniqueStmt (SExp _ expr) = do
  (_, expr') <- uniqueExpr expr
  returnE $ SExp () expr'


uniqueStmt (Decl l t (item:tail)) = do
  Env pEnv vEnv <- ask
  (item', id, n) <- case item of
    NoInit _ id -> do
      n <- getNext id
      return (NoInit () $ newId id n, id, n)
    Init _ id expr -> do
      (_, expr') <- uniqueExpr expr
      n <- getNext id
      return (Init () (newId id n) expr', id, n)

  let vEnv' = M.insert id (n, strip t) vEnv
  let env' = Env pEnv vEnv'

  (env'', Decl () t' tail') <- local (\_ -> env') $ uniqueStmt $ Decl l t tail
  return (env'', Decl () t' $ item':tail')

uniqueStmt (Decl _ t []) = returnE $ Decl () (strip t) []


uniqueStmt stmt = returnE $ strip stmt

uniqueExpr :: Expr Location -> VarsMonad (Type(), Expr ())

uniqueExpr (EVar _ id) = do
  (id', t) <- getInfo id
  return $ (t, EVar () $ id')

uniqueExpr (Neg _ expr) = do
  (_, expr') <- uniqueExpr expr
  return $ (Int (), Neg () expr')

uniqueExpr (Not _ expr) = do
  (_, expr') <- uniqueExpr expr
  return $ (Bool (), Not () expr')

uniqueExpr (EMul _ e1 op e2) = do
  (_, e1') <- uniqueExpr e1
  (_, e2') <- uniqueExpr e2
  return $ (Int (), EMul () e1' (strip op) e2')


uniqueExpr (EAdd _ e1 op e2) = do
  (t, e1') <- uniqueExpr e1
  (_, e2') <- uniqueExpr e2
  return $ (t, if (t == Str ())
    then ECon () e1' e2'
    else EAdd () e1' (strip op) e2')

uniqueExpr (ERel _ e1 op e2) = do
  (_, e1') <- uniqueExpr e1
  (_, e2') <- uniqueExpr e2
  return $ (Bool (), ERel () e1' (strip op) e2')


uniqueExpr (EAnd _ e1 e2) = do
  (_, e1') <- uniqueExpr e1
  (_, e2') <- uniqueExpr e2
  return $ (Bool (), EAnd () e1' e2')


uniqueExpr (EOr _ e1 e2) = do
  (_, e1') <- uniqueExpr e1
  (_, e2') <- uniqueExpr e2
  return $ (Bool (), EOr () e1' e2')

uniqueExpr (EApp _ id es) = do
  es' <- sequence $ uniqueExpr <$> es
  Just t <- asks $ (!? id) . pEnv
  let (_, es'') = L.unzip es'
  return $ (t, EApp () id es'')

uniqueExpr e@(EString _ _) = return $ (Str (), strip e)

uniqueExpr e@(ELitInt _ _) = return $ (Int (), strip e)

uniqueExpr e = return $ (Bool (), strip e)






