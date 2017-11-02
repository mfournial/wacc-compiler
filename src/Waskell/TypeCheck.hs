module Waskell.TypeCheck where

import Data.Waskell.ADT
import Data.Waskell.Error
  
getFuncType :: Function -> ErrorList Type
getFuncType = undefined

getRhsType :: AssignRhs -> [NewScope] -> ErrorList Type
getRhsType = undefined

