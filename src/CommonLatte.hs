{-# LANGUAGE FlexibleContexts #-}

module CommonLatte where


import qualified Data.Map.Strict as M
import Control.Monad.Reader

import AbsLatte


type Location = Maybe (Int, Int)


maxInt :: Integer
maxInt = 2 ^ 31 - 1
minInt :: Integer
minInt = -(2 ^ 31)


dividorShadow :: Char
dividorShadow = '_'


dividorSSA :: Char
dividorSSA = '_'


returnE :: MonadReader env m => a -> m (env, a)
returnE x = do
  env <- ask
  return (env, x)


(!?) :: Ord k => M.Map k a -> k -> Maybe a
(!?) = flip M.lookup


strip :: Functor f => f a -> f ()
strip = ((\_ -> ()) <$>)


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


builtInDefs :: [TopDef Location]
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


printList :: Show a => [a] -> IO ()
printList l = do
  sequence_ $ putStrLn . show <$> l

