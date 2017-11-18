module Code.Generator.RetLoc(RetLoc(..), storeToRegister) where

import Code.Instructions

import Data.Sequence

data RetLoc = HeapAddr Int | StackOffset Int | StringLit String

storeToRegister :: Reg -> RetLoc -> Instructions
storeToRegister r (HeapAddr i)    = storeToRegister' r (Const i) >< storeToRegister' r (OffReg r (addrToOffset 0) False) 
storeToRegister r (StackOffset i) = storeToRegister' r (OffReg StackPointer (addrToOffset i) False)   
storeToRegister r (StringLit str) = storeToRegister' r (Label str)

storeToRegister' :: Reg -> Address -> Instructions
storeToRegister' r a = pure (LDR AL W r a)

--Note this is the incorrect behaviour TODO 
addrToOffset :: Int -> Offset
addrToOffset i = Int i
