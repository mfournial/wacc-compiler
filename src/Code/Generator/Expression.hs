module Code.Generator.Expression (expression, expressionReg) where

import Code.Instructions(Reg)
import Code.Generator.State
import Code.Generator.RetLoc
import Data.Waskell.ADT

import Data.Sequence((><))

expression :: Expression -> ARM (Instructions, RetLoc)
expression e = undefined

expressionReg :: Expression -> Reg -> ARM Instructions
expressionReg e r = do
  (is, rl) <- expression e
  return (is >< storeToRegister r rl)
