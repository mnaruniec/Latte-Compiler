module TypeLatte where

import AbsLatte
import Data.DList
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Reader
import Control.Monad.Writer

type Location = Maybe (Int, Int)
type ErrorList = DList String

type PEnv = M.Map Ident (Type ())
type VEnv = M.Map Ident (Type (), Bool)
data Env = Env {pEnv :: PEnv, vEnv :: VEnv}

type TypeMonad a = ReaderT Env (Writer ErrorList) a


(!?) = flip M.lookup

tellL :: [a] -> Writer (DList [a]) ()
tellL = tell . singleton

tellLoc :: Location -> String -> Writer ErrorList ()
tellLoc Nothing l = tellL l
tellLoc (Just (line, col)) l = tellL $ "Line " ++ show line ++ ":" ++ show col ++ ": " ++ l

stripType :: Type a -> Type ()
stripType (Int _) = Int ()
stripType (Str _) = Str ()
stripType (Bool _) = Bool ()
stripType (Void _) = Void ()
stripType (Fun _ t ts) = Fun () (stripType t) (stripType <$> ts)

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
          when (stripType t == Void ()) $
            tellLoc loc' $ "Function " ++ id ++ " has void argument " ++ id' ++ "!"
      in do
        sequence_ $ f' <$> args
        checkDuplicates (\(Arg _ _ id') -> id')
          (\(Arg loc' _ (Ident id'))
            -> tellLoc loc' $ "Found duplicate argument name " ++ id' ++ " in function " ++ id ++ "!")
          args
        when (id == "main" && (stripType t /= Int () || args /= [])) $
          tellLoc loc "The main function should be of type () -> int!"

    fnDefToType :: TopDef Location -> (Ident, Type ())
    fnDefToType (FnDef _ t id args _) = (id, Fun () (stripType t) argTypes) where
      argTypes = (\(Arg _ t id) -> stripType t) <$> args

    checkFnBody :: PEnv -> TopDef Location -> Writer ErrorList ()
    checkFnBody pEnv (FnDef loc t (Ident id) args block) = do
      let vEnv =
            let
              f (Arg _ t id') = (id', (stripType t, False))
            in
              M.fromList $ f <$> args
      let env = Env {pEnv = pEnv, vEnv = vEnv}

      returning <- runReaderT (checkBlock block) env
      when (not returning) $ tellLoc loc $ unlines
        ["Function " ++ id ++ " might not return!",
         "Please add return at its end, even if it is redundant."]


checkBlock :: Block Location -> TypeMonad Bool
checkBlock (Block _ stmts) = do
  Env pEnv vEnv <- ask
  let vEnv' = M.map (\(t, _) -> (t, False)) vEnv
  let env = Env pEnv vEnv'

  returning <- local (\_ -> env) $ checkBlock' False stmts
  return returning

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

-- TODO dodac info w ktorej funkcji jestem









