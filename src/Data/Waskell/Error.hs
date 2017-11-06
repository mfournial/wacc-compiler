module Data.Waskell.Error where

import Control.Monad (liftM)
import Data.List
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
  show (ErrorList (Just a) es) = show a ++ "\n" ++ concatMap showError es
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
printError = putStrLn . showError

showError :: ErrorData -> String
showError ed = show (level ed) ++ ": in stage " ++ show (stage ed) ++ " at position " ++ show (position ed) ++ " with error:\n" ++ message ed 

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : xs) = Just x

displayResult :: Show a => ErrorList a -> IO ()
displayResult (ErrorList b []) = putStrLn (show b)
displayResult (ErrorList _ eds) = mapM_ printError (sort eds)

displayErrorsAndExit :: ErrorList a -> IO ()
displayErrorsAndExit (ErrorList _ []) = putStrLn "SUCCESS" >> exitWith ExitSuccess
displayErrorsAndExit (ErrorList _ eds) = mapM printError (sort eds) >> maybe (exitWith ExitSuccess) (\e -> exitWith (ExitFailure (exitCode e))) (safeHead (filter isFatal eds))
