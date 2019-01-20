{-# LANGUAGE FlexibleContexts #-}
module TypeCheck where

import AbsLatte
import qualified Data.DList as D
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Control.Monad.Reader
import Control.Monad.Writer

import CommonLatte


type ErrorList = D.DList String

type PEnv = M.Map Ident (Type ())
type VEnv = M.Map Ident (Type (), Bool)
data Env = Env {pEnv :: PEnv, vEnv :: VEnv, curr :: (Ident, Type ())}

type TypeMonad a = ReaderT Env (Writer ErrorList) a


tellL :: MonadWriter (D.DList [a]) m => [a] -> m ()
tellL = tell . D.singleton


tellLoc :: MonadWriter ErrorList m => Location -> String -> m ()
tellLoc Nothing l = tellL l
tellLoc (Just (line, col)) l = tellL $ "Line " ++ show line ++ ":" ++ show col ++ ": " ++ l


checkDuplicates :: Ord b => (a -> b) -> (a -> Writer ErrorList ()) -> [a] -> Writer ErrorList ()
checkDuplicates key printer l = sequence_ $ foldl f (S.empty, return ()) l where
  f (set, monad) el = (set', monad') where
    key' = key el
    monad' = if (S.member key' set)
      then monad >> printer el
      else monad
    set' = S.insert key' set


checkTypes :: Program Location -> [String]
checkTypes p = D.toList errList where
  (_, errList) = runWriter $ checkProgram p


checkProgram :: Program Location -> Writer ErrorList ()
checkProgram (Program _ topDefs) = do
  sequence_ $ checkSignature <$> topDefs

  let topDefs' = builtInDefs ++ topDefs

  checkDuplicates (\(FnDef _ _ id _ _) -> id)
    (\(FnDef loc _ (Ident id) _ _)
      -> tellLoc loc $ "Found duplicate definition of function " ++ id ++ "!")
    topDefs'

  let pEnv = M.fromList $ fnDefToType <$> topDefs'

  when (pEnv !? (Ident "main") == Nothing) $ tellL "There is no main function!"

  sequence_ $ checkFnBody pEnv <$> topDefs

  where
    checkSignature :: TopDef Location -> Writer ErrorList ()
    checkSignature (FnDef loc t (Ident id) args _) =
      let
        checkParam (Arg loc' t (Ident id')) = do
          checkTypeCorrect t
          when (strip t == Void ()) $
            tellLoc loc' $ "Function " ++ id ++ " has void argument " ++ id' ++ "!"

      in do
        checkTypeCorrect t
        sequence_ $ checkParam <$> args
        checkDuplicates (\(Arg _ _ id') -> id')
          (\(Arg loc' _ (Ident id'))
            -> tellLoc loc' $ "Found duplicate argument name " ++ id' ++ " in function " ++ id ++ "!")
          args
        when (id == "main" && (strip t /= Int () || args /= [])) $
          tellLoc loc "The main function should be of type () -> int!"
        when (not $ checkIdent id) $
          tellLoc loc $ "Incorrect function name: " ++ id ++ "!"

    fnDefToType :: TopDef Location -> (Ident, Type ())
    fnDefToType (FnDef _ t id args _) = (id, Fun () (strip t) argTypes) where
      argTypes = (\(Arg _ t id) -> strip t) <$> args

    checkFnBody :: PEnv -> TopDef Location -> Writer ErrorList ()
    checkFnBody pEnv (FnDef loc t (Ident id) args block) = do
      let vEnv =
            let
              f (Arg _ t id') = (id', (strip t, False))
            in
              M.fromList $ f <$> args
      let env = Env {pEnv = pEnv, vEnv = vEnv, curr = (Ident id, strip t)}

      returning <- runReaderT (checkBlock block) env
      when ((not returning) && strip t /= Void ()) $ tellLoc loc $
        "Function " ++ id ++ " might not return!\n" ++
          "Please add return at its end, even if it is redundant."


checkBlock :: Block Location -> TypeMonad Bool
checkBlock (Block _ stmts) = do
  Env pEnv vEnv curr <- ask
  let vEnv' = M.map (\(t, _) -> (t, False)) vEnv
  let env = Env pEnv vEnv' curr

  local (\_ -> env) $ checkBlock' False stmts
  where
    checkBlock' returning' (stmt:t) = do
      (env'', returning'') <- checkStmt stmt
      local (\_ -> env'') $ checkBlock' (returning' || returning'') t

    checkBlock' returning' [] = return returning'


checkStmt :: Stmt Location -> TypeMonad (Env, Bool)
checkStmt (Empty _) = returnE False

checkStmt (VRet loc) = do
  (Ident id, t) <- asks curr
  when (t /= Void ()) $ tellLoc loc $
    "Returning without value from function " ++ id ++
      " of return type " ++ show t ++ "!"
  returnE True

checkStmt (Ret loc expr) = do
  (Ident id, t) <- asks curr
  t' <- checkExpr expr
  when (t /= t') $ tellLoc loc $
    "Returning value of type " ++ show t' ++
      " from function " ++ id ++ " of return type " ++ show t ++ "!"
  returnE True

checkStmt (BStmt _ block) = checkBlock block >>= returnE

checkStmt (SExp _ expr) = do
  checkExpr expr
  returnE False

checkStmt (CondElse _ expr stmt1 stmt2) = do
  assertType (Bool ()) expr
  (_, ret1) <- checkStmt stmt1
  (_, ret2) <- checkStmt stmt2
  returnE $
    case expr of
      ELitTrue _ -> ret1
      ELitFalse _ -> ret2
      otherwise -> ret1 && ret2

checkStmt (Cond _ expr stmt) = do
  assertType (Bool ()) expr
  (_, ret) <- checkStmt stmt
  returnE $
    case expr of
      ELitTrue _ -> ret
      otherwise -> False

checkStmt (While _ expr stmt) = do
  assertType (Bool ()) expr
  checkStmt stmt
  returnE $
    case expr of
      ELitTrue _ -> True -- consider infinite loop as always correct
      otherwise -> False

checkStmt (Decl _ _ []) = returnE False

checkStmt (Decl l t (item:tail)) = do
  checkTypeCorrect t
  let t' = strip t
  when (t' == Void ()) $ tellLoc l $ "Cannot create variables of type void!"

  Env pEnv vEnv curr <- ask

  (loc, id) <- case item of
    NoInit loc (Ident id) -> return (loc, id)
    Init loc (Ident id) expr -> do
      assertType t' expr
      return (loc, id)

  when (not $ checkIdent id) $ tellLoc loc $ "Incorrect variable name: " ++ id ++ "!"

  case vEnv !? (Ident id) of
    Just (_, True) -> tellLoc loc $ "Found duplicate variable " ++ id ++ " declaration in a single block!"
    otherwise -> return ()

  let vEnv' = M.insert (Ident id) (t', True) vEnv
  let env' = Env pEnv vEnv' curr

  local (\_ -> env') $ checkStmt (Decl l t tail)

checkStmt (Ass loc (Ident id) expr) = do
  vEnv <- asks vEnv
  case vEnv !? Ident id of
    Nothing -> do
      tellLoc loc $ "Variable " ++ id ++ " not declared!"
      checkExpr expr
      return ()
    Just (t, _) -> do
      assertType t expr
  returnE False


checkStmt (Incr loc (Ident id)) = do
  vEnv <- asks vEnv
  case vEnv !? Ident id of
    Nothing -> do
      tellLoc loc $ "Variable " ++ id ++ " not declared!"
    Just (t, _) -> do
       when (t /= Int ()) $ tellLoc loc $
        "Cannot increment variable " ++ id ++ " of type " ++ show t ++ "!"
  returnE False


checkStmt (Decr loc (Ident id)) = do
  vEnv <- asks vEnv
  case vEnv !? Ident id of
    Nothing -> do
      tellLoc loc $ "Variable " ++ id ++ " not declared!"
    Just (t, _) -> do
       when (t /= Int ()) $ tellLoc loc $
        "Cannot decrement variable " ++ id ++ " of type " ++ show t ++ "!"
  returnE False


checkStmt (AssArr loc id index e2) = do
  assertType (Int ()) index
  t <- assertArrayVar loc id
  assertType t e2
  returnE False


checkStmt (ForEach loc t (Ident elId) (Ident arrId) stmt) = do
  t' <- assertArrayVar loc (Ident arrId)
  when (t' /= strip t) $ tellLoc loc $
    "Iterator " ++ elId ++ " of type " ++ show (strip t)
      ++ " for array " ++ arrId ++ " of type " ++ show t' ++ "!"
  Env pEnv vEnv curr <- ask
  let vEnv' = M.insert (Ident elId) (strip t, True) vEnv
  local (\_ -> Env pEnv vEnv' curr) $ checkStmt stmt
  returnE False


checkTypeCorrect :: MonadWriter ErrorList m => Type Location -> m ()
checkTypeCorrect (Arr loc (Void _)) = do
  tellLoc loc $ "Void arrays are not permitted!"

checkTypeCorrect (Arr loc (Arr _ _)) = do
  tellLoc loc $ "Multi-dimentional arrays are not permitted!"

checkTypeCorrect _ = return ()


assertPrimitive :: MonadWriter ErrorList m => Type Location -> m ()
assertPrimitive (Arr loc _) = do
  tellLoc loc $ "Expected primitive type!"

assertPrimitive _ = return ()


assertArrayVar :: Location -> Ident -> TypeMonad (Type ())
assertArrayVar loc (Ident id) = do
  vEnv <- asks vEnv
  case vEnv !? (Ident id) of
    Nothing -> do
      tellLoc loc $
        "Array " ++ id ++ " not defined!\n"
          ++ "Assuming int[] for further checking."
      return $ Int ()
    Just ((Arr () t), _) -> return t
    Just _ -> do
      tellLoc loc $
        "Variable " ++ id ++ " is not an array!\n"
          ++ "Treated like int[] for further checking."
      return $ Int ()


assertAny :: [Type ()] -> Expr Location -> TypeMonad (Type ())
assertAny ts expr = do
  t <- checkExpr expr
  when (not $ elem t ts) $ tellLoc (getLoc expr) $
    "Expected expression of one of types: " ++ show ts ++ ", but received expression of type " ++ show t ++ "!"
  return t

assertType :: Type () -> Expr Location -> TypeMonad ()
assertType t expr = do
  t' <- checkExpr expr
  when (t /= t') $ tellLoc (getLoc expr) $
    "Expected expression of type " ++ show t ++ " but received expression of type " ++ show t' ++ "!"


assertNotVoid :: Expr Location -> TypeMonad (Type ())
assertNotVoid expr = do
  t <- checkExpr expr
  when (t == Void ()) $ tellLoc (getLoc expr) $
    "Expression cannot have type void!"
  return t

assertEqu :: MonadWriter ErrorList m => Location -> Type () -> Type () -> m ()
assertEqu loc t1 t2 =
  when (t1 /= t2) $ tellLoc loc $
    "Both expressions should have the same type, but left is of type "
          ++ show t1 ++ " and right of type " ++ show t2 ++ "!"


checkExpr :: Expr Location -> TypeMonad (Type ())
checkExpr (ELitTrue _) = return $ Bool ()

checkExpr (ELitFalse _) = return $ Bool ()

checkExpr (ELitInt loc val) = do
  when (val < minInt || val > maxInt) $ tellLoc loc $
    "Integer constant should be between " ++ show minInt ++
      " and " ++ show maxInt ++ "!"
  return $ Int ()

checkExpr (EString _ _) = return $ Str ()


checkExpr (EVar loc (Ident id)) = do
  vEnv <- asks vEnv
  case vEnv !? (Ident id) of
    Nothing -> do
      tellLoc loc $
        "Variable " ++ id ++ " not defined!\n"
          ++ "Assuming int for further checking."
      return $ Int ()
    Just (t, _) -> return t

checkExpr (EApp loc (Ident id) args) = do
  let lArgs = length args
  argTypes <- sequence $ checkExpr <$> args
  pEnv <- asks pEnv
  case pEnv !? Ident id of
    Nothing -> do
      tellLoc loc $
        "Function " ++ id ++ " not defined!\n"
          ++ "Assuming return value int for further checking."
      return $ Int ()
    Just (Fun () t params) -> do
      let lParams = length params
      when (lParams /= lArgs) $ tellLoc loc $
        "Function " ++ id ++ " takes " ++ show lParams ++
          " arguments, but is called with " ++ show lArgs ++ "!"
      sequence_ $ checkArg <$> (zip4 [1..] args argTypes params)
      return t
  where
    checkArg (idx, expr, argType, paramType) =
      when (argType /= paramType) $ tellLoc (getLoc expr) $
        "Argument number " ++ show idx ++ " of function " ++ id ++
          " should be of type " ++ show paramType ++ ", but is " ++ show argType ++ "!"


checkExpr (Neg _ e) = do
  assertType (Int ()) e
  return $ Int ()


checkExpr (Not _ e) = do
  assertType (Bool ()) e
  return $ Bool ()

checkExpr (EMul _ e1 _ e2) = do
  assertType (Int ()) e1
  assertType (Int ()) e2
  return $ Int ()


checkExpr (EAdd loc e1 op e2)
  | op' /= Plus () = do
      assertType (Int ()) e1
      assertType (Int ()) e2
      return $ Int ()
  | otherwise = do
      t1 <- assertAny [Int (), Str ()] e1
      t2 <- assertAny [Int (), Str ()] e2
      assertEqu loc t1 t2
      return $ t1
  where
    op' = strip op

checkExpr (ERel loc e1 rel e2)
  | rel' == EQU () || rel' == NE () = do
      t1 <- assertNotVoid e1
      t2 <- assertNotVoid e2
      assertEqu loc t1 t2
      return $ Bool ()
  | otherwise = do
      assertType (Int ()) e1
      assertType (Int ()) e2
      return $ Bool ()
  where
    rel' = strip rel


checkExpr (EOr _ e1 e2) = do
  assertType (Bool ()) e1
  assertType (Bool ()) e2
  return $ Bool ()


checkExpr (EAnd _ e1 e2) = do
  assertType (Bool ()) e1
  assertType (Bool ()) e2
  return $ Bool ()


checkExpr (EArrNew _ t expr) = do
  assertPrimitive t
  assertType (Int ()) expr
  return $ Arr () $ strip t


checkExpr (EArrAcc loc id expr) = do
  assertType (Int ()) expr
  assertArrayVar loc id


checkExpr (EArrLen loc id) = do
  assertArrayVar loc id
  return $ Int ()

