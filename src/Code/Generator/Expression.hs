{-# LANGUAGE TupleSections #-}

module Code.Generator.Expression (expression, expressionReg) where

import Code.Instructions
import Code.Generator.State
import Code.Generator.RetLoc
import Data.Waskell.ADT

import Data.Sequence((><), empty)
import Data.Char(ord)

expression :: Expression -> ARM (Instructions, RetLoc)
expression (IntExp i)       = intToReg i       R0
expression (BoolExp True)   = intToReg 1       R0
expression (BoolExp False)  = intToReg 0       R0
expression (CharExpr c)     = intToReg (ord c) R0
expression PairExpr         = intToReg 0       R0

expression (StringExpr str) = fmap (empty,) $ newStringLiteral str


expression _ = error "Expression pattern not matched"

intToReg :: Int -> Reg -> ARM (Instructions, RetLoc)
intToReg i r = return (pure (LDR AL W r (Const i)), Register r)

expressionReg :: Expression -> Reg -> ARM Instructions
expressionReg e r = do
  (is, rl) <- expression e
  return (is >< storeToRegister r rl)
