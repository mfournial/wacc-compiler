module Code.Generator.RetLoc(
  RetLoc,
  PureRetLoc(..),
  storeToRegister,
  storeToRegisterPure
) where

import Code.Generator.RetLoc.Internal

import Code.Instructions
import Code.Generator.State

import Data.Sequence

storeToRegister :: Reg -> RetLoc -> ARM Instructions
storeToRegister r (StackPtr i) = do
  off <- getOffsetFromStackPtr i
  return $ storeToRegister' r (OffReg StackPointer (offsetToARMOffset off) False)   

storeToRegister r (PRL k) = return $ storeToRegisterPure r k

storeToRegisterPure :: Reg -> PureRetLoc -> Instructions
storeToRegisterPure r (HeapAddr i) = storeToRegister' r (Const i) >< storeToRegisterPure r (RegLoc r)

storeToRegisterPure r (StringLit str) = storeToRegister' r (Label str)
storeToRegisterPure r' (RegLocOffset r o) = storeToRegister' r' (OffReg r (offsetToARMOffset o) False)
storeToRegisterPure r' (RegLoc r) = storeToRegister' r' (OffReg r (offsetToARMOffset 0) False)
storeToRegisterPure r (ImmInt x) = singleton (MOV AL F r (ImmOpInt x))
storeToRegisterPure r (ImmChar c) = singleton (MOV AL F r (ImmOpCh c))

storeToRegisterPure r' (Register r)
  | r' == r = empty
  | otherwise = singleton (MOV AL F r' (ShiftReg r NSH))

storeToRegister' :: Reg -> Address -> Instructions
storeToRegister' r a = singleton (LDR AL W r a)

--Note this is the incorrect behaviour TODO 
offsetToARMOffset :: Int -> Offset
offsetToARMOffset i = Int i
