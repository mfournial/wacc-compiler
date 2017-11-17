module Code.Generator.RetLoc(RetLoc(..), storeToRegister) where

import Code.Instructions

import Data.Sequence

data RetLoc = HeapAddr Int | StackOffset Int | StringLit String

storeToRegister :: RetLoc -> Reg -> Instructions
storeToRegister (HeapAddr i) r    = storeToRegister' r (Const i) >< storeToRegister' r (OffReg r (addrToOffset 0) False) 
storeToRegister (StackOffset i) r = storeToRegister' r (OffReg StackPointer (addrToOffset i) False)   
storeToRegister (StringLit str) r = storeToRegister' r (Label str)

storeToRegister' :: Reg -> Address -> Instructions
storeToRegister' r a = pure (LDR AL W r a)

--Note this is the incorrect behaviour TODO 
addrToOffset :: Int -> Offset
addrToOffset i = Int i
