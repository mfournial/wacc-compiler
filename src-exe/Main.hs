{-| WACC Compiler 

Compiler for WACC language as defined in year 2017 Imperial year 2 specification

Group 26 -- Waskell
Module      : main
Maintainer  : mmf115@ic.ac.uk
Portability : POSIX

This module runs the compiler and decides which actions to take depending on
user input

-}

{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-overlapping-patterns #-}

import Control.Monad (when)
import qualified Data.ByteString as B
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.Environment (getArgs, getProgName)
import System.IO (stdin, hGetContents)

import Bnfc.AbsWacc
import Bnfc.ErrM
import Bnfc.LexWacc
import Bnfc.ParWacc
import Bnfc.PrintWacc
import Bnfc.SkelWacc

type ParseFun a = [Token] -> Err a

myLLexer :: String -> [Token]
myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p str = 
    let ts =  myLLexer str
    in case p ts of
        Bad s   -> do  
            putStrLn "\nParse              Failed...\n"
            putStrV v "Tokens:"
            putStrV v $ show ts
            putStrLn s
            exitWith $ ExitFailure 100
        Ok  tree -> do
            putStrLn "Parse Successfull!"
            showTree v tree
            exitWith ExitSuccess


showTree :: (Show a, Print a) => Int -> a -> IO ()
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
    , "  -s (files)      Silent mode. Parse content otherwise files silently."
    ]
  exitWith $ ExitFailure 1

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> hGetContents stdin >>= run 2 pExp
    "-s":fs -> mapM_ (runFile 0 pExp) fs
    fs -> mapM_ (runFile 2 pExp) fs
