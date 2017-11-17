module Code.Generator.Expression (expression) where

import Code.Instructions()
import Code.Generator.State
import Code.Generator.RetLoc
import Data.Waskell.ADT

expression :: Expression -> ARM (Instructions, RetLoc)
expression e = undefined
