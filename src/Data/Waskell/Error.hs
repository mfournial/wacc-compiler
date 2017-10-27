module Data.Waskell.Error (
  Level,
  Stage,
  ErrorData,
  ErrorList,
  throwError,
  throwFatal
) where

import Control.Monad (liftM)


data Level = InfoLevel | WarningLevel | FatalLevel
  deriving (Ord, Eq)
data Stage = AnalStage | TypeStage | LexorStage | ParserStage | UnknownStage
  deriving (Ord, Eq)

instance Show Level where
  show InfoLevel = "INFO"
  show WarningLevel = "WARN"
  show FatalLevel = "ERROR"

instance Show Stage where
  show l = "Stage: " ++ show' l
    where
      show' AnalStage = "Analysis"
      show' LexorStage = "Lexor"
      show' ParserStage = "Parser"
      show' TypeStage = "Typecheck"
      show' UnknownStage = "Internal"

data ErrorData = ErrorData {
  level :: Level,
  stage :: Stage,
  position :: (Int, Int),
  message :: String
} deriving (Eq)

instance Ord ErrorData where
  (<=) a b = if (level a == level b) 
               then if (stage a == stage b) 
                 then (message a <= message b)
               else (stage a < stage b)
             else (level a < level b)

instance (Show a) => Show (ErrorList a) where
  show (ErrorList (Just a) _) = show a
  show (ErrorList Nothing _) = "Fatal Error"

data ErrorList a = ErrorList (Maybe a) [ErrorData] 
  deriving (Eq, Ord)

instance Applicative ErrorList where
  pure a = ErrorList (Just a) [] 
  (<*>) (ErrorList (Just f) efunc) (ErrorList (Just a) es) = ErrorList (Just (f a)) (efunc ++ es)
  (<*>) (ErrorList _ efunc) (ErrorList _ es) = ErrorList Nothing (efunc ++ es)

instance Monad ErrorList where
  return = pure  
  fail s = ErrorList Nothing [(ErrorData FatalLevel UnknownStage (0,0) ("Internal Compiler Error: " ++ s))]
  ErrorList (Just a) es >>= f = case f a of
    ErrorList (Just b) es' -> ErrorList (Just b) (es ++ es')
    ErrorList Nothing es'  -> ErrorList Nothing (es ++ es')
  ErrorList Nothing es >>= _ = ErrorList Nothing es
    

instance Functor ErrorList where
  fmap = liftM

throwError :: a -> ErrorData -> ErrorList a
throwError a e = ErrorList (Just a) [e]

throwFatal :: Stage -> (Int, Int) -> String -> ErrorList a
throwFatal s p m = ErrorList Nothing [(ErrorData FatalLevel s p m)]
