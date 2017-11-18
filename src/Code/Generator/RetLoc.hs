module Code.Generator.RetLoc(RetLoc(..), storeToRegister) where

import Code.Instructions

import Data.Sequence

data RetLoc = HeapAddr Int | StackOffset Int | StringLit String | RegLoc Reg | Register Reg

storeToRegister :: Reg -> RetLoc -> Instructions
storeToRegister r (HeapAddr i)    = storeToRegister' r (Const i) >< storeToRegister r (RegLoc r)
storeToRegister r (StackOffset i) = storeToRegister' r (OffReg StackPointer (addrToOffset i) False)   
storeToRegister r (StringLit str) = storeToRegister' r (Label str)
storeToRegister r' (RegLoc r) = storeToRegister' r' (OffReg r (addrToOffset 0) False)
storeToRegister r' (Register r)
  | r' == r = empty
  | otherwise = pure (MOV AL F r' (ShiftReg r NSH))

storeToRegister' :: Reg -> Address -> Instructions
storeToRegister' r a = pure (LDR AL W r a)

--Note this is the incorrect behaviour TODO 
addrToOffset :: Int -> Offset
addrToOffset i = Int i
