module LLVM where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.Reader hiding (void)
import Control.Monad.State hiding (void)

import AbsLatte
import CommonLatte
import QuadCode

type LLVM = [String]

type FunMap = M.Map Label String

type TypeMap = M.Map Atom String

newtype Global = Global Integer
  deriving (Eq)

instance Show Global where
  show (Global n) = prefixGlobal ++ show n

type StringMap = M.Map String Global

type Inherits = M.Map Atom [Atom]

data LLVMState = St {strings :: StringMap, nextStr :: Integer,
  types :: TypeMap, nextLocal :: Integer, inherits :: Inherits }
  deriving (Eq, Show)

type LLVMMonad a = ReaderT FunMap (State LLVMState) a



getLLVM :: [QuadFun] -> LLVM
getLLVM funs = output where
  output = stringDefs ++ ["", ""] ++ imports ++ ["", ""] ++ funs'''

  stringDefs = concat $ getStringDef <$> M.toList strings

  imports = concat $ getFunHeader declare <$> runtimeDefs

  funs''' = concat funs''

  (funs'', St strings _ _ _ _) =
    runState (runReaderT buildMonad startEnv) startState

  buildMonad = sequence $ llvmFun <$> funs

  startState =
    St {strings = M.empty, nextStr = 0, types = M.empty, nextLocal = 0, inherits = M.empty}

  startEnv = M.fromList $ collectFunType <$> funs'
  collectFunType (FnDef _ t (Ident id) _ _) = (LFun id, strType t)
  funs' = runtimeDefs ++ (fst $ unzip funs)

  runtimeDefs = strip <$> privDefs ++ builtInDefs


getStringDef :: (String, Global) -> LLVM
getStringDef (s, global) = [withNull] where
  noNull = show global ++ " = private constant [" ++ show (length s + 1) ++ " x i8] c" ++ show s

  withNull = (init noNull) ++ "\\00\""


llvmFun :: QuadFun -> LLVMMonad LLVM
llvmFun (topDef@(FnDef _ _ _ args _), qBlocks) = do
  let header = getFunHeader define topDef
  sequence_ $ putArgType <$> args
  sequence_ $ collectTypesBlock <$> qBlocks
  resolveTypes
  qBlocks' <- sequence $ llvmBlock <$> qBlocks
  return $ header ++ concat qBlocks' ++ ["}", ""]


resolveTypes :: LLVMMonad ()
resolveTypes = do
  inh <- gets inherits
  sequence_ $ resolveTypesTree S.empty <$> M.keys inh

resolveTypesTree :: S.Set Atom -> Atom -> LLVMMonad (S.Set Atom, Maybe String)
resolveTypesTree visited a = do
  let visited' = S.insert a visited
  if S.member a visited
    then return (visited, Nothing)
    else do
      has <- hasType a
      if has
        then do
          t <- getType a
          return (visited', Just t)
        else do

          inhs <- gets $ (flip (M.!) a) . inherits
          has <- sequence $ hasType <$> inhs
          let trueList = filter snd $ zip inhs has
          res@(visited'', Just t) <- if trueList == []
            then
              let
                f (vis, Nothing) a' = resolveTypesTree vis a'
                f acc _ = return acc
              in do
                foldM f (visited', Nothing) inhs
            else do
              let a' = fst $ head trueList
              t <- getType a'
              return (visited', Just t)
          putType a t
          sequence_ $ (flip putType) t <$> inhs
          return res



collectTypesBlock :: QuadBlock -> LLVMMonad ()
collectTypesBlock (_, block) = do
  sequence_ $ collectTypesQuad <$> block


collectTypesQuad :: Quad -> LLVMMonad ()

collectTypesQuad (QJCond (ValTrue v) _ _) =
  putType v bool

collectTypesQuad (QJCond (ValFalse v) _ _) =
  putType v bool


collectTypesQuad (QNeg a1 a2) = do
  putType a1 int
  putType a2 int


collectTypesQuad (QOp a1 a2 QCon a3) = do
  putType a1 str
  putType a2 str
  putType a3 str


collectTypesQuad (QOp a1 a2 op a3) = do
  putType a1 int
  putType a2 int
  putType a3 int


collectTypesQuad (QAss a1 c@(CString _)) = do
  putType a1 str


collectTypesQuad (QCall a1 lab as) = do
  t <- asks $ flip (M.!) lab
  putType a1 t


collectTypesQuad (QPhi a1 rs) = do
  putInherits a1 $ fst $ unzip rs


collectTypesQuad _ = return ()




putArgType :: Arg () -> LLVMMonad ()
putArgType (Arg _ t (Ident id)) = putType (Var id) $ strType t


llvmBlock :: QuadBlock -> LLVMMonad LLVM
llvmBlock (label, block) = do
  block' <- sequence $ llvmQuad <$> block
  let label' = case label of
        LFun _ -> []
        otherwise -> [show label ++ ":"]
  return $ label' ++ concat block'


llvmQuad :: Quad -> LLVMMonad LLVM
llvmQuad (QJmp lab) = do
  return ["br label %" ++ show lab]

llvmQuad (QJCond (ValTrue v) l1 l2) = do
  return ["br i1 " ++ show v ++ ", label %" ++ show l1 ++ ", label %" ++ show l2]

llvmQuad (QJCond (ValFalse v) l1 l2) = do
  llvmQuad (QJCond (ValTrue v) l2 l1)

llvmQuad (QJCond (Comp a1 rel a2) l1 l2) = do
  v <- getFreshVar
  t <- getType a1
  let relS = case rel of
        EQU () -> "eq "
        NE () -> "ne "
        LTH () -> "slt "
        LE () -> "sle "
        GTH () -> "sgt "
        GE () -> "sge "
  let comp = [show v ++ " = icmp " ++ relS ++ t ++ " " ++ show a1 ++ ", " ++ show a2]
  jump <- llvmQuad $ QJCond (ValTrue v) l1 l2
  return $ comp ++ jump

llvmQuad (QVRet) = return ["ret void"]

llvmQuad (QRet a) = do
  t <- getType a
  return ["ret " ++ t ++ " " ++ show a]

llvmQuad (QNeg a1 a2) = llvmQuad (QOp a1 (CInt 0) QMinus a2)

llvmQuad (QOp a1 a2 QCon a3) = do
  llvmQuad $ QCall a1 privConcat [a2, a3]

llvmQuad (QOp a1 a2 op a3) = do
  let opS = case op of
        QPlus -> "add "
        QMinus -> "sub "
        QTimes -> "mul "
        QDiv -> "sdiv "
        QMod -> "srem "
  return [show a1 ++ " = " ++ opS ++ int ++ " " ++ show a2 ++ ", " ++ show a3]


llvmQuad (QAss a1 c@(CString _)) = do
  llvmQuad $ QCall a1 privCopy [c]

llvmQuad (QCall a1 lab as) = do
  t <- getType a1
  args <- sequence $ loadString <$> as
  let (as', loads) = unzip args
  let loads' = reverse $ concat loads
  asTypes <- sequence $ getType <$> as'
  let typedAs = zip asTypes as'

  let addArg (t, a) acc = ", " ++ t ++ " " ++ show a ++ acc
  let argStr = foldr addArg ")" typedAs

  let argStr' = "(" ++ if length argStr > 1
        then tail $ tail argStr
        else argStr

  return $ loads' ++ [show a1 ++ " = call " ++ t ++ " " ++ show lab ++ argStr']

llvmQuad (QVCall lab as) = do
  t <- asks $ flip (M.!) lab
  args <- sequence $ loadString <$> as
  let (as', loads) = unzip args
  let loads' = reverse $ concat loads
  asTypes <- sequence $ getType <$> as'
  let typedAs = zip asTypes as'

  let addArg (t, a) acc = ", " ++ t ++ " " ++ show a ++ acc
  let argStr = foldr addArg ")" typedAs

  let argStr' = "(" ++ if length argStr > 1
        then tail $ tail argStr
        else argStr

  return $ loads' ++ ["call " ++ t ++ " " ++ show lab ++ argStr']

llvmQuad (QPhi a1 rs) = do
  t <- getType a1

  let addRule (a, lab) acc = ", [" ++ show a ++ ", "
        ++ (if isFunLab lab then "%0" else "%" ++ show lab) ++ "]" ++ acc
  let rs' = foldr addRule "" rs

  return [show a1 ++ " = phi " ++ t ++ (tail rs')]

llvmQuad QNOp = return []

llvmQuad _ = return []


getFunHeader :: Bool -> TopDef () -> LLVM
getFunHeader define (FnDef _ t (Ident id) args _) = [header] where
  header = beg ++ strType t ++ " @" ++ id ++ args'' ++ end

  args' = foldr handleArg ")" args
  args'' = "(" ++ if length args' > 1
    then tail $ tail args'
    else args'

  handleArg (Arg _ t (Ident id)) acc = ", " ++ strType t ++ " %" ++ id ++ acc

  (beg, end) = if define
    then ("define ", " {")
    else ("declare ", "")



isFunLab :: Label -> Bool
isFunLab (LFun _) = True
isFunLab _ = False


constType :: Atom -> String
constType (CInt _) = int
constType (CString _) = str
constType CTrue = bool
constType CFalse = bool
--constType _ = int --DEBUG


hasType :: Atom -> LLVMMonad Bool
hasType a@(Var _) = do
  typeMap <- gets types
  return $ M.member a typeMap

hasType _ = return True


getType :: Atom -> LLVMMonad String
getType a@(Var _) = do
  typeMap <- gets types
  return $ (M.!) typeMap a

getType a = return $ constType a


putType :: Atom -> String -> LLVMMonad ()
putType a@(Var _) t = do
  St str nStr typeMap nLoc inh <- get
  let typeMap' = M.insert a t typeMap
  put $ St str nStr typeMap' nLoc inh

putType _ _ = return ()


putInherits :: Atom -> [Atom] -> LLVMMonad ()
putInherits a as = do
  St str nStr typeMap nLoc inh <- get
  let inh' = M.insert a as inh
  put $ St str nStr typeMap nLoc inh'


getFreshVar :: LLVMMonad Atom
getFreshVar = do
  St str nStr typeMap nLoc inh <- get
  let nLoc' = nLoc + 1
  put $ St str nStr typeMap nLoc' inh
  return $ Var $ prefixLLVMTemp ++ show nLoc

getFreshString :: LLVMMonad Global
getFreshString = do
  St str nStr typeMap nLoc inh <- get
  let nStr' = nStr + 1
  put $ St str nStr' typeMap nLoc inh
  return $ Global nStr


loadString :: Atom -> LLVMMonad (Atom, LLVM)
loadString (CString s) = do
  stringMap <- gets strings
  glob <- case M.lookup s stringMap of
    Nothing -> do
      glob <- getFreshString
      let stringMap' = M.insert s glob stringMap
      modify $ (\(St _ nStr tM nL inh) -> St stringMap' nStr tM nL inh)
      return glob
    Just glob -> return glob

  v <- getFreshVar
  putType v str

  return (v, [show v ++ " = bitcast [" ++ show ((length s) + 1)
    ++ " x i8]* " ++ show glob ++ " to i8*"])

loadString a = return (a, [])


define = True
declare = False

int = "i32"
str = "i8*"
bool = "i1"
void = "void"

privConcat :: Label
privConcat = LFun $ prefixPrivFun ++ "concat"

privCopy :: Label
privCopy = LFun $ prefixPrivFun ++ "copy"


strType :: Type () -> String
strType (Int _) = int
strType (Str _) = str
strType (Bool _) = bool
strType (Void _) = void


