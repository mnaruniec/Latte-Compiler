module TreeTrans where

import AbsLatte
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.Reader
import Control.Monad.State

import CommonLatte


type PEnv = M.Map Ident (Type ())
type VEnv = M.Map Ident (Int, Type ())
data Env = Env {pEnv :: PEnv, vEnv :: VEnv}

type Next = M.Map Ident Int

type VarsMonad a = ReaderT Env (State Next) a


dividorShadow :: Char
dividorShadow = '$'


newId :: Ident -> Int -> Ident
newId (Ident id) n = Ident $ id ++ [dividorShadow] ++ show n


removeQuotes :: String -> String
removeQuotes = init . tail


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


transTree :: Program Location -> Program ()
transTree (Program _ topDefs) =
  Program () $ transFnDef pEnv <$> topDefs where
    pEnv = M.fromList $ fnDefToPEnv <$> topDefs'
    fnDefToPEnv (FnDef _ t id _ _) = (id, strip t)
    topDefs' = builtInDefs ++ topDefs


transFnDef :: PEnv -> TopDef Location -> TopDef ()
transFnDef pEnv (FnDef _ t id args block) = result where
  result = FnDef () (strip t) id args' block'

  vEnv = M.fromList $ argToVEnv <$> args
  env = Env pEnv vEnv
  next = M.fromList $ argToNext <$> args
  args' = argToArg' <$> args

  argToVEnv (Arg _ t id) = (id, (0, strip t))
  argToNext (Arg _ _ id) = (id, 1)
  argToArg' (Arg _ t id) = Arg () (strip t) $ newId id 0

  block' = evalState (runReaderT (transBlock block) env) next


transBlock :: Block Location -> VarsMonad (Block ())
transBlock (Block _ stmts) = do
    stmts' <- transBlock' stmts
    return $ Block () stmts'
  where
    transBlock' [] = return []
    transBlock' (stmt:t) = do
      (env', stmt') <- transStmt stmt
      t' <- local (\_ -> env') $ transBlock' t
      return $ stmt':t'


transStmt :: Stmt Location -> VarsMonad (Env, Stmt ())
transStmt (BStmt _ block) = transBlock block >>= returnE . (BStmt ())

transStmt (Ass _ id expr) = do
  (_, expr') <- transExpr expr
  id' <- getNewId id
  returnE $ Ass () id' expr'

transStmt (Incr _ id) = do
  id' <- getNewId id
  returnE $ Incr () id'

transStmt (Decr _ id) = do
  id' <- getNewId id
  returnE $ Decr () id'

transStmt (Ret _ expr) = do
  (_, expr') <- transExpr expr
  returnE $ Ret () expr'

transStmt (Cond _ expr stmt) = do
  (_, expr') <- transExpr expr
  (_, stmt') <- transStmt stmt
  returnE $ Cond () expr' stmt'

transStmt (CondElse _ expr stmt1 stmt2) = do
  (_, expr') <- transExpr expr
  (_, stmt1') <- transStmt stmt1
  (_, stmt2') <- transStmt stmt2
  returnE $ CondElse () expr' stmt1' stmt2'

transStmt (While _ expr stmt) = do
  (_, expr') <- transExpr expr
  (_, stmt') <- transStmt stmt
  returnE $ While () expr' stmt'

transStmt (SExp _ expr) = do
  (_, expr') <- transExpr expr
  returnE $ SExp () expr'

transStmt (Decl l t (item:tail)) = do
  Env pEnv vEnv <- ask
  (item', id, n) <- case item of
    NoInit _ id -> do
      n <- getNext id
      return (NoInit () $ newId id n, id, n)
    Init _ id expr -> do
      (_, expr') <- transExpr expr
      n <- getNext id
      return (Init () (newId id n) expr', id, n)

  let vEnv' = M.insert id (n, strip t) vEnv
  let env' = Env pEnv vEnv'

  (env'', Decl () t' tail') <- local (\_ -> env') $ transStmt $ Decl l t tail
  return (env'', Decl () t' $ item':tail')

transStmt (Decl _ t []) = returnE $ Decl () (strip t) []

transStmt stmt = returnE $ strip stmt


transExpr :: Expr Location -> VarsMonad (Type(), Expr ())

transExpr (EVar _ id) = do
  (id', t) <- getInfo id
  return $ (t, EVar () $ id')

transExpr (Neg _ expr) = do
  (_, expr') <- transExpr expr
  return $ (Int (), Neg () expr')

transExpr (Not _ expr) = do
  (_, expr') <- transExpr expr
  return $ (Bool (), Not () expr')

transExpr (EMul _ e1 op e2) = do
  (_, e1') <- transExpr e1
  (_, e2') <- transExpr e2
  return $ (Int (), EMul () e1' (strip op) e2')

transExpr (EAdd _ e1 op e2) = do
  (t, e1') <- transExpr e1
  (_, e2') <- transExpr e2
  return $ (t, if (t == Str ())
    then ECon () e1' e2'
    else EAdd () e1' (strip op) e2')

transExpr (ERel _ e1 op e2) = do
  (_, e1') <- transExpr e1
  (_, e2') <- transExpr e2
  return $ (Bool (), ERel () e1' (strip op) e2')

transExpr (EAnd _ e1 e2) = do
  (_, e1') <- transExpr e1
  (_, e2') <- transExpr e2
  return $ (Bool (), EAnd () e1' e2')

transExpr (EOr _ e1 e2) = do
  (_, e1') <- transExpr e1
  (_, e2') <- transExpr e2
  return $ (Bool (), EOr () e1' e2')

transExpr (EApp _ id es) = do
  es' <- sequence $ transExpr <$> es
  Just t <- asks $ (!? id) . pEnv
  let (_, es'') = unzip es'
  return $ (t, EApp () id es'')

transExpr e@(EString _ str) = return $ (Str (), EString () $ removeQuotes str)

transExpr e@(ELitInt _ _) = return $ (Int (), strip e)

transExpr e = return $ (Bool (), strip e)

