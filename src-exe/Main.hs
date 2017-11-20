{-| 
= WACC Compiler 

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
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.Environment (getArgs, getProgName)
import System.FilePath.Posix (replaceExtension, takeFileName)
import System.IO (stdin, hGetContents)

import Alex.Waskell
import Code.Generator
import Data.Waskell.ADT
import Data.Waskell.Error
import Data.Waskell.Scope
import Happy.Waskell

-- | Definition for a parse function: takes a list of Tokens and produces an
-- errorList possibly containing the ADT or the errors produced during parsing
type ParseFunction a = [Token] -> ErrorList a

-- | The Expected erbosity of the output
type Verbosity = Int

-- | runFile will call run on the content of the passed in file
runFile :: Verbosity -- ^ The desired level of verbosity
        -> ParseFunction WaccTree -- ^ The desired parse function to use
        -> FilePath -- ^ The file path 
        -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p (replaceExtension (takeFileName f) ".s")

-- | run will run the parser on the inputed string and output the result if 
-- the verbosity requires it. Run will always ouput errors
run :: Verbosity -- ^ the desired verbosity
    -> ParseFunction WaccTree -- ^ the parse function to use
    -> FilePath -- ^ the name of the ouput file
    -> String -- ^ the input to compiler
    -> IO ()
run v parser outPath input = dieOnError v (myLexer input >>= parser >>= genSymbols) >>= (produceASM outPath)

-- | usage displays usage of the program CLI interface
usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -v (files)      Verbose mode. Parse content otherwise files silently."
    ]
  exitWith $ ExitFailure 1

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> hGetContents stdin >>= run 2 pExp "a.s"
    "-v":fs    -> mapM_ (runFile 2 pExp) fs
    fs         -> mapM_ (runFile 0 pExp) fs
