{-# LANGUAGE TupleSections #-}

module Code.Generator.Expression (expression, expressionReg) where

import Code.Instructions
import Code.Generator.State
import Code.Generator.StateInstructions
import Code.Generator.ARM
import Data.Waskell.ADT

import Data.Sequence((><), (<|), (|>), empty, singleton)
import Data.Maybe

import Control.Monad

expression :: Expression -> ARM (Instructions, RetLoc)
expression (BracketExp (e, _)) = expression e

expression (IntExp i)         = intToReg i       R0
expression (BoolExp True)     = intToReg 1       R0
expression (BoolExp False)    = intToReg 0       R0
expression (CharExpr c)       = return (singleton (MOV AL F R0 (ImmOpCh c)), PRL (Register R0))
expression PairExpr           = intToReg 0       R0

expression (IdentExpr (s, _)) = fmap (empty,) $ getVar s

expression (UExpr (uexp, _) (e, _)) = do
  (sub, loc)          <- expression e
  strRegIns           <- storeToRegister R0 loc
  return $ (sub >< strRegIns >< evalUExp uexp, (PRL (Register R0)))

expression (BExp (e, _) (bop, _) (e', _)) = do
  (saveReg, _)  <- push [R1]
  (left, lloc)        <- expression e
  strLeft             <- storeToRegister R1 lloc
  (right, rloc)       <- expression e'
  strRight            <- storeToRegister R0 rloc
  resReg              <- pop [R1]
  return $ ((saveReg <| ((left >< strLeft >< right >< strRight >< evalBExp bop) |> resReg)), (PRL (Register R0)))

expression (ArrayExpr (ArrayElem i indexps, _)) = do
  (HeapAddr arraddr) <- fmap fromJust $ (getFromHeap . getVal) i
  let arrLoc = (HeapAddr (arraddr + 1)) -- Note the first word is the length of the array
  pushed <- mapM ((pusher =<<) . expression . getVal) indexps 
  ins    <- foldM arrayExp' (storeToRegisterPure R0 arrLoc) $ reverse pushed
  return (ins, (PRL (Register R0)))
  where
    pusher :: (Instructions, RetLoc) -> ARM (Instructions, RetLoc)
    pusher (ins, loc) = do
      str <- storeToRegister R0 loc
      (pushins, pushlocs) <- push [R0]
      return ((ins >< str) |> pushins, head pushlocs)
    arrayExp' :: Instructions -> (Instructions, RetLoc) -> ARM Instructions
    arrayExp' is (is', loc) = do
      str <- storeToRegister R0 loc
      return (is >< is' >< str >< storeToRegisterPure R0 (RegLoc R0)) 

expression (StringExpr str) = fmap ((empty,) . PRL) $ newStringLiteral str

evalUExp :: UnaryOperator -> Instructions
evalUExp UMinus  = singleton (RSB AL F R0 R0 (ImmOpInt 0))
evalUExp UBang   = singleton (MVN AL F R0 (ShiftReg R0 NSH))
evalUExp ULength = storeToRegisterPure R0 (RegLoc R0)
-- UOrd and UChr actually do nothing! They exist only for type safety.
evalUExp UOrd    = empty
evalUExp UChr    = empty

evalBExp :: BinaryOperator -> Instructions
evalBExp BTimes   = singleton (MUL AL F R0 R0 R1)
--Todo check div by zero
evalBExp BDivide    = singleton (BL AL "__aeabi_idiv") 
evalBExp BModulus   = singleton (BL AL "__aeabi_idivmod")
evalBExp BPlus      = singleton (ADD AL F R0 R0 (ShiftReg R1 NSH))
evalBExp BMinus     = singleton (SUB AL F R0 R0 (ShiftReg R1 NSH))
evalBExp BAnd       = singleton (AND AL F R0 R0 (ShiftReg R1 NSH))
evalBExp BOr        = singleton (ORR AL F R0 R0 (ShiftReg R1 NSH))

evalBExp BMore      = evalBBoolExp GTh LE
evalBExp BLess      = evalBBoolExp LTh GE
evalBExp BMoreEqual = evalBBoolExp GE  LTh
evalBExp BLessEqual = evalBBoolExp LE  GTh
evalBExp BEqual     = evalBBoolExp Eq  Neq
evalBExp BNotEqual  = evalBBoolExp Neq Eq

evalBBoolExp :: Condition -> Condition -> Instructions
evalBBoolExp a b  = singleton (CMP AL R0 (ShiftReg R1 NSH))
                  |> MOV a F R0 (ImmOpInt 1) 
                  |> MOV b F R0 (ImmOpInt 0)


intToReg :: Int -> Reg -> ARM (Instructions, RetLoc)
intToReg i r = return (pure (MOV AL F r (ImmOpInt i)), PRL (Register r))

expressionReg :: Expression -> Reg -> ARM Instructions
expressionReg e r = do
  (is, rl) <- expression e
  iStore <- storeToRegister r rl
  return (is >< iStore)
