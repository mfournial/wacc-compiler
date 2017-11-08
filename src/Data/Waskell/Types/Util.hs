{-|

WaccType Type Utilities

Group 26 -- Waskell
Module      : ADT
Maintainer  : kc1616@ic.ac.uk
Portability : POSIX

This module specifies utility functions for handling types.
-}

module Data.Waskell.Types.Util where

import Data.Waskell.ADT
import Data.Waskell.Error

-- | liftType lifts a BaseType to be of Haskell Type Type (From our ADT)  
liftType :: BaseType -> Type
liftType = Pairable . BaseType

-- | wplus lifts two BaseTypes and then pairs them
wplus :: BaseType -> BaseType -> (Type, Type)
wplus a b = (liftType a, liftType b)

-- | throwTypeError throws an error with Type Error: prefixing the error.
throwTypeError :: a -> Position -> String -> ErrorList a
throwTypeError t p str = throwError t (ErrorData FatalLevel TypeStage p ("Type Error: " ++ str) semanticErrorCode)
