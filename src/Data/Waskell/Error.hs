{-|

Error Monad

Group 26 -- Waskell
Module      : Error
Maintainer  : kc1616@ic.ac.uk
Portability : POSIX

This module is the basis for handling errors in the group 26 WACC compiler. We consider that if we still have a value, we may continue computation.
Bind is implemented in such a way that if die is called, rather than the throwError function (which requires a placeholder value) then computation will stop.
Otherwise we continue with the placeholder value.

-}

module Data.Waskell.Error (
  Level(..),
  Stage(..),
  ErrorData(..),
  ErrorList,
  throwError,
  die,
  displayResult,
  dieOnError,
  checkForFatals,
  unsafeErrorListToMaybe,
  errorListToMaybe,
  syntaxErrorCode,
  semanticErrorCode
  ) where


import Control.Monad (liftM)
import Data.List
import System.Console.ANSI
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)


data Level = InfoLevel | WarningLevel | FatalLevel
  deriving (Ord, Eq)
data Stage = AnalStage | TypeStage | LexorStage | ParserStage | UnknownStage
  deriving (Ord, Eq)

instance Show Level where
  show InfoLevel = "INFO"
  show WarningLevel = "WARN"
  show FatalLevel = "ERROR"

instance Show Stage where
  show AnalStage = "Analysis"
  show LexorStage = "Lexor"
  show ParserStage = "Parser"
  show TypeStage = "Typecheck"
  show UnknownStage = "Internal"

data ErrorData = ErrorData {
  level :: Level,
  stage :: Stage,
  position :: (Int, Int),
  message :: String,
  exitCode :: Int
} deriving (Eq)

instance Ord ErrorData where
  (<=) a b = if (level a == level b)
               then if (stage a == stage b)
                 then (message a <= message b)
               else (stage a < stage b)
             else (level a < level b)

instance (Show a) => Show (ErrorList a) where
  show (ErrorList (Just a) es) = show a ++ "\n" ++ concatMap (\ed -> message ed) es
  show (ErrorList Nothing _) = "Fatal Error"

data ErrorList a = ErrorList (Maybe a) [ErrorData]
  deriving (Eq, Ord)

instance Applicative ErrorList where
  pure a = ErrorList (Just a) []
  (<*>) (ErrorList (Just f) efunc) (ErrorList (Just a) es) = ErrorList (Just (f a)) (efunc ++ es)
  (<*>) (ErrorList _ efunc) (ErrorList _ es) = ErrorList Nothing (efunc ++ es)

instance Monad ErrorList where
  return = pure
  fail s = ErrorList Nothing [(ErrorData FatalLevel UnknownStage (0,0) ("Internal Compiler Error: " ++ s) 255)]
  ErrorList (Just a) es >>= f = case f a of
    ErrorList (Just b) es' -> ErrorList (Just b) (es ++ es')
    ErrorList Nothing es'  -> ErrorList Nothing (es ++ es')
  ErrorList Nothing es >>= _ = ErrorList Nothing es

instance Functor ErrorList where
  fmap = liftM

syntaxErrorCode :: Int
syntaxErrorCode = 100

semanticErrorCode :: Int
semanticErrorCode = 200

checkForFatals :: ErrorList a -> ErrorList a
checkForFatals e@(ErrorList _ es) = if any (\ed -> level ed == FatalLevel) es
                                      then ErrorList Nothing es
                                      else e

isFatal :: ErrorData -> Bool
isFatal ed = level ed == FatalLevel

throwError :: a -> ErrorData -> ErrorList a
throwError a e = ErrorList (Just a) [e]

die :: Stage -> (Int, Int) -> String -> Int -> ErrorList a
die s p m i = ErrorList Nothing [(ErrorData FatalLevel s p m i)]

printError :: ErrorData -> IO ()
printError ed = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn (showErrorHeader ed)
  setSGR [Reset]
  putStrLn (showErrorMessage ed)

-- | Shows the meta informations about the error
showErrorHeader :: ErrorData -> String
showErrorHeader ed = "*** " ++ show (level ed) ++ ": in stage " ++ show (stage ed) ++
                     " at position " ++ show (position ed) ++ " with error:\n"

-- | Shows the error message in the error data
showErrorMessage :: ErrorData -> String
showErrorMessage ed = message ed 

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

-- | Will extract a maybe value from the errorlist. N.B This may not actually be the value of the computation performed, and could be filled with values designed to perform error recovery.
-- For example, in typechecking an expression we may return IntType when actually there was a TypeError.
unsafeErrorListToMaybe :: ErrorList a -> Maybe a
unsafeErrorListToMaybe (ErrorList v _) = v

-- | Checks for fatal errors, if there are none we will return Just the value computed, if there is a fatal error however, we return nothing.
-- This is equivalent to unsafeErrorListToMaybe . checkForFatals
errorListToMaybe :: ErrorList a -> Maybe a 
errorListToMaybe = unsafeErrorListToMaybe . checkForFatals

displayResult :: Show a => ErrorList a -> IO ()
displayResult (ErrorList b []) = putStrLn (show b)
displayResult (ErrorList _ eds) = mapM_ printError (sort eds)

dieOnError :: Show a => Int -> ErrorList a -> IO (a)
dieOnError v (ErrorList (Just a) []) = if v > 1 then putStrLn "SUCCESS CHECKING VALIDITY" >> return a else return a
dieOnError v (ErrorList _ eds) = mapM printError (sort eds) >> maybe (if v > 1 then putStrLn "SUCCESS" >> exitWith ExitSuccess else exitWith ExitSuccess) (\e -> exitWith (ExitFailure (exitCode e))) (safeHead (filter isFatal eds))
