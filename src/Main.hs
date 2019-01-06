module Main where


import System.IO ( stdin, stderr, hGetContents, hPutStrLn  )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import LexLatte
import ParLatte
import SkelLatte
import PrintLatte
import AbsLatte

import CommonLatte
import FrontEnd
import BackEnd

import ErrM



type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun (Program Location) -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

printError :: IO ()
printError = do
  hPutStrLn stderr "ERROR\n"

printOk :: IO ()
printOk = do
  hPutStrLn stderr "OK\n"


run :: Verbosity -> ParseFun (Program Location) -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do printError
                          putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do let (tree', errors) = frontEnd tree
                          when (errors /= []) $ do
                            printError
                            printList errors
                            exitFailure
                          backEnd tree'
                          exitSuccess




showTree :: Int -> Program Location -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2 pProgram
    "-s":fs -> mapM_ (runFile 0 pProgram) fs
    fs -> mapM_ (runFile 2 pProgram) fs





