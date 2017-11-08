module Data.Waskell.Types.Util where

import Data.Waskell.ADT
  
liftType :: BaseType -> Type
liftType = Pairable . BaseType

wplus :: BaseType -> BaseType -> (Type, Type)
wplus a b = (liftType a, liftType b)
