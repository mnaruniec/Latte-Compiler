module Main where


import System.IO ( stderr, hGetContents, hPutStrLn, withFile, IOMode (WriteMode) )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess, ExitCode(ExitSuccess) )
import System.FilePath ( FilePath, dropExtension )
import System.Process ( readProcessWithExitCode )
import Control.Monad ( when )

import LexLatte
import ParLatte
import AbsLatte

import CommonLatte
import FrontEnd
import BackEnd

import ErrM



type ParseFun a = [Token] -> Err a

myLLexer = myLexer


lib :: FilePath
lib = RUNTIME_PATH -- provided by preprocessor


runCommand :: String -> [String] -> IO ()
runCommand cmd args = do
  (exitCode, out, err) <- readProcessWithExitCode cmd args []
  when (exitCode /= ExitSuccess)
    $ printError >> putStrLn out >> putStrLn err >> exitFailure


runFile :: ParseFun (Program Location) -> FilePath -> IO ()
runFile p f = do
  content <- readFile f
  let noExt = dropExtension f
  let ll = noExt ++ ".ll"
  let bc = noExt ++ ".bc"
  run ll bc p content

printError :: IO ()
printError = do
  hPutStrLn stderr "ERROR\n"

printOk :: IO ()
printOk = do
  hPutStrLn stderr "OK\n"


run :: FilePath -> FilePath -> ParseFun (Program Location) -> String -> IO ()
run ll bc p s = let ts = myLLexer s in case p ts of
           Bad s    -> do printError
                          putStrLn "\nParse              Failed...\n"
                          putStr "Tokens:"
                          putStr $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do let (tree', errors) = frontEnd tree
                          when (errors /= []) $ do
                            printError
                            printList errors
                            exitFailure
                          llvm <- backEnd tree'
                          withFile ll WriteMode $ (\h -> hPutStrLn h llvm)
                          runCommand "llvm-as" ["-o", bc, ll]
                          runCommand "llvm-link" ["-o", bc, bc, lib]
                          printOk
                          exitSuccess

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= run "./Stdin.ll" "./Stdin.bc" pProgram
    f:_ -> runFile pProgram f

