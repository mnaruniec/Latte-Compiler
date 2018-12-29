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

data Comp =
    Comp Atom (RelOp ()) Atom
  deriving (Eq, Ord, Show)

data Quad =
    Ass Atom Atom
  | Jmp Integer
  | JCond Comp Integer
  | Param Int Atom
  | RetV
  | Ret Atom
  | NOp
  deriving (Eq, Ord, Show)

data LQuad =
    NoL Quad
  | Lab Integer Quad
  deriving (Eq, Ord, Show)


data Context = Context {nextVar :: Integer, nextLab :: Integer}

type QuadMonad a = StateT Context (Writer (DList LQuad)) a


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


genQuad :: Program () -> [(TopDef (), [LQuad])]
genQuad (Program _ topDefs) =
  zip topDefs $ quadFn <$> topDefs where
    --TODO label at beginning, retV at end
    quadFn (FnDef _ _ _ _ block) = toList $
      execWriter (runStateT (quadBlock block) startContext)

    startContext = Context 0 0


quadBlock :: Block () -> QuadMonad ()
quadBlock (Block () stmts) = do
  sequence_ $ quadStmt <$> stmts

quadStmt :: Maybe Integer -> Stmt () -> QuadMonad ()
quadStmt mLab (While () expr stmt) = do
  condLab <- getLab
  stmtLab <- getLab
  emitMLab mLab $ Jmp condLab
  quadStmt (Just stmtLab) stmt
  --TODO



quadStmt mLab stmt = return ()



