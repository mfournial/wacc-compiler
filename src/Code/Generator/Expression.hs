{-# LANGUAGE TupleSections #-}

module Code.Generator.Expression (expression, expressionReg) where

import Code.Instructions
import Code.Generator.State
import Code.Generator.StateInstructions
import Code.Generator.RetLoc
import Code.Generator.RetLoc.Internal
import Data.Waskell.ADT

import Data.Sequence((><), (<|), (|>), empty, singleton)
import Data.Char(ord)

expression :: Expression -> ARM (Instructions, RetLoc)
expression (IntExp i)         = intToReg i       R0
expression (BoolExp True)     = intToReg 1       R0
expression (BoolExp False)    = intToReg 0       R0
expression (CharExpr c)       = intToReg (ord c) R0
expression PairExpr           = intToReg 0       R0

expression (IdentExpr (s, _)) = fmap (empty,) $ getVar s

expression (UExpr (uexp, _) (e, _)) = do
  (sub, loc)          <- expression e
  (savIns, [saveLoc]) <- push [R0]
  strRegIns           <- storeToRegister R0 loc
  (retIns, [retLoc])  <- push [R0]
  restoreReg          <- storeToRegister R0 saveLoc
  return $ ((sub >< (savIns <| (strRegIns >< ((evalUExp uexp |> retIns) >< restoreReg)))), retLoc)

expression (StringExpr str) = fmap ((empty,) . PRL) $ newStringLiteral str
expression _ = error "Expression pattern not matched"

evalUExp :: UnaryOperator -> Instructions
evalUExp UMinus = singleton (RSB AL F R0 R0 (ImmOpInt 0))
evalUExp _ = error "UExp pattern not matched"

intToReg :: Int -> Reg -> ARM (Instructions, RetLoc)
intToReg i r = return (pure (LDR AL W r (Const i)), PRL (Register r))

expressionReg :: Expression -> Reg -> ARM Instructions
expressionReg e r = do
  (is, rl) <- expression e
  iStore <- storeToRegister r rl
  return (is >< iStore)
