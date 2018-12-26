module TypeLatte where

import AbsLatte
import Data.DList
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad.Reader
import Control.Monad.Writer

type Location = Maybe (Int, Int)
type ErrorList = DList String

type PEnv = M.Map Ident (Type ())
type VEnv = M.Map Ident (Type (), Bool)
data Env = Env {pEnv :: PEnv, vEnv :: VEnv, curr :: (Ident, Type ())}

type TypeMonad a = ReaderT Env (Writer ErrorList) a

maxInt :: Integer
maxInt = 2 ^ 31 - 1
minInt :: Integer
minInt = -(2 ^ 31)


(!?) = flip M.lookup

tellL :: [a] -> Writer (DList [a]) ()
tellL = tell . singleton

tellLoc :: Location -> String -> Writer ErrorList ()
tellLoc Nothing l = tellL l
tellLoc (Just (line, col)) l = tellL $ "Line " ++ show line ++ ":" ++ show col ++ ": " ++ l

strip :: Functor f => f a -> f ()
strip = ((\_ -> ()) <$>)


checkDuplicates :: Ord b => (a -> b) -> (a -> Writer ErrorList ()) -> [a] -> Writer ErrorList ()
checkDuplicates key printer l = sequence_ $ foldl f (S.empty, return ()) l where
  f (set, monad) el = (set', monad') where
    key' = key el
    monad' = if (S.member key' set)
      then monad >> printer el
      else monad
    set' = S.insert key' set

builtInDefs = [prIDef, prStrDef, errDef, reIDef, reStrDef] where
  loc = Just (-1, -1)
  prIDef = FnDef loc (Void loc) (Ident "printInt")
    [Arg loc (Int loc) (Ident "n")] (Block loc [Empty loc])
  prStrDef = FnDef loc (Void loc) (Ident "printString")
    [Arg loc (Str loc) (Ident "str")] (Block loc [Empty loc])
  errDef = FnDef loc (Void loc) (Ident "error")
    [] (Block loc [Empty loc])
  reIDef = FnDef loc (Int loc) (Ident "readInt")
    [] (Block loc [Empty loc])
  reStrDef = FnDef loc (Str loc) (Ident "readString")
    [] (Block loc [Empty loc])


checkTypes :: Program (Location) -> Writer ErrorList ()
checkTypes (Program _ topDefs) = do
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
        f' (Arg loc' t (Ident id')) =
          when (strip t == Void ()) $
            tellLoc loc' $ "Function " ++ id ++ " has void argument " ++ id' ++ "!"
      in do
        sequence_ $ f' <$> args
        checkDuplicates (\(Arg _ _ id') -> id')
          (\(Arg loc' _ (Ident id'))
            -> tellLoc loc' $ "Found duplicate argument name " ++ id' ++ " in function " ++ id ++ "!")
          args
        when (id == "main" && (strip t /= Int () || args /= [])) $
          tellLoc loc "The main function should be of type () -> int!"

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
      when (not returning) $ tellLoc loc $ unlines
        ["Function " ++ id ++ " might not return!",
         "Please add return at its end, even if it is redundant."]


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


returnE :: a -> TypeMonad (Env, a)
returnE x = do
  env <- ask
  return (env, x)


checkStmt :: Stmt Location -> TypeMonad (Env, Bool)
checkStmt (Empty _) = returnE False

checkStmt (VRet loc) = do
  (Ident id, t) <- asks curr
  when (t /= Void ()) $ lift $ tellLoc loc $
    "Returning without value from function " ++ id ++
      " of return type " ++ show t ++ "!"
  returnE True

checkStmt (Ret loc expr) = do
  (Ident id, t) <- asks curr
  t' <- checkExpr expr
  when (t /= t') $ lift $ tellLoc loc $
    "Returning value of type " ++ show t' ++
      " from function " ++ id ++ " of return type " ++ show t ++ "!"
  returnE True

checkStmt _ = returnE False


getLoc :: Expr a -> a
getLoc (EVar l _) = l
getLoc (ELitInt l _) = l
getLoc (ELitTrue l) = l
getLoc (ELitFalse l) = l
getLoc (EApp l _ _) = l
getLoc (EString l _) = l
getLoc (Neg l _) = l
getLoc (Not l _) = l
getLoc (EMul l _ _ _) = l
getLoc (EAdd l _ _ _) = l
getLoc (ERel l _ _ _) = l
getLoc (EAnd l _ _) = l
getLoc (EOr l _ _) = l

assertAny :: [Type ()] -> Expr Location -> TypeMonad (Type ())
assertAny ts expr = do
  t <- checkExpr expr
  when (not $ L.elem t ts) $ lift $ tellLoc (getLoc expr) $
    "Expected expression of one of types: " ++ show ts ++ ", but received expression of type " ++ show t ++ "!"
  return t

assertType :: Type () -> Expr Location -> TypeMonad ()
assertType t expr = do
  t' <- checkExpr expr
  when (t /= t') $ lift $ tellLoc (getLoc expr) $
    "Expected expression of type " ++ show t ++ " but received expression of type " ++ show t' ++ "!"


assertNotVoid :: Expr Location -> TypeMonad (Type ())
assertNotVoid expr = do
  t <- checkExpr expr
  when (t == Void ()) $ lift $ tellLoc (getLoc expr) $
    "Expression cannot have type void!"
  return t

assertEqu :: Location -> Type () -> Type () -> TypeMonad ()
assertEqu loc t1 t2 =
  when (t1 /= t2) $ lift $ tellLoc loc $
    "Both expressions should have the same type, but left is of type "
          ++ show t1 ++ " and right of type " ++ show t2 ++ "!"


checkExpr :: Expr Location -> TypeMonad (Type ())
checkExpr (ELitTrue _) = return $ Bool ()

checkExpr (ELitFalse _) = return $ Bool ()

checkExpr (ELitInt loc val) = do
  when (val < minInt || val > maxInt) $ lift $ tellLoc loc $
    "Integer constant should be between " ++ show minInt ++
      " and " ++ show maxInt ++ "!"
  return $ Int ()

checkExpr (EString _ _) = return $ Str ()


checkExpr (EVar loc (Ident id)) = do
  vEnv <- asks vEnv
  case vEnv !? (Ident id) of
    Nothing -> do
      lift $ tellLoc loc $
        "Variable " ++ id ++ " not defined!\n"
          ++ "Assuming int for further checking."
      return $ Int ()
    Just (t, _) -> return t

checkExpr (EApp loc (Ident id) args) = do
  let lArgs = L.length args
  argTypes <- sequence $ checkExpr <$> args
  pEnv <- asks pEnv
  case pEnv !? Ident id of
    Nothing -> do
      lift $ tellLoc loc $
        "Function " ++ id ++ " not defined!\n"
          ++ "Assuming return value int for further checking."
      return $ Int ()
    Just (Fun () t params) -> do
      let lParams = L.length params
      when (lParams /= lArgs) $ lift $ tellLoc loc $
        "Function " ++ id ++ " takes " ++ show lParams ++
          " arguments, but is called with " ++ show lArgs ++ "!"
      sequence_ $ checkArg <$> (L.zip4 [1..] args argTypes params)
      return t
  where
    checkArg (idx, expr, argType, paramType) =
      when (argType /= paramType) $ lift $ tellLoc (getLoc expr) $
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
      return $ Int ()
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







