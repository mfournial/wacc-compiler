module Code.Generator.ARM where

import Control.Monad.State.Lazy(State)
import Data.Sequence
import qualified Data.HashMap.Strict as M

import Code.Instructions

type ARM = State Junk

data Junk = Junk {
  strLits :: Data,
  stack :: VarTable,
  heap :: VarTable,
  sp :: Int,
  ref :: Int,
  runtime :: Seq RCID
} 

type VarTable = [M.HashMap String Int]
type Data = Seq String

data RuntimeComponent = RC RCID Instructions

instance Eq RuntimeComponent where
  (==) (RC rid _) (RC rid' _) = rid == rid'

data RCID = 
      PrintStr
    | PrintInt
    | PrintChar
    | PrintBool
    | ReadInt
    | ReadChar
    | ThrowRuntimeErr
    | ThrowOverflowErr
    | FreePair
    | PrintRef
    | ArrayCheck
    | Checkdbz
  deriving (Eq)

data RetLoc = PRL PureRetLoc | StackPtr Int 
  deriving (Show)

data PureRetLoc = 
      StringLit String
    | RegLoc Reg
    | RegLocOffset Reg Int
    | Register Reg
    | ImmChar Char
    | ImmInt Int
  deriving (Show)

type RegMod = Reg -> Address -> Instructions


storeToRegisterPure :: Reg -> PureRetLoc -> Instructions
storeToRegisterPure r (ImmInt i)    = singleton (MOV AL F r (ImmOpInt i))
storeToRegisterPure r (ImmChar c)   = singleton (MOV AL F r (ImmOpCh c))
storeToRegisterPure r (Register r') = storeRegs r r'
storeToRegisterPure r k             = modifyRegisterPure storeToRegister' r k

updateWithRegisterPure :: Reg -> PureRetLoc -> Instructions
updateWithRegisterPure r (ImmInt i)    = error "Attempting to update immediate value"
updateWithRegisterPure r (ImmChar c)   = error "Attempting to update immediate value"
updateWithRegisterPure r (Register r') = storeRegs r' r
updateWithRegisterPure r k             = modifyRegisterPure updateWithRegister' r k

modifyRegisterPure :: RegMod -> Reg -> PureRetLoc -> Instructions
modifyRegisterPure f r (StringLit str) = f r (Label str)
modifyRegisterPure f r' (RegLocOffset r o) = f r' (OffReg r (offsetToARMOffset o) False)
modifyRegisterPure f r' (RegLoc r) = f r' (OffReg r (offsetToARMOffset 0) False)
modifyRegisterPure _ _ _ = error "modify cannot work on immediate values"

storeRegs :: Reg -> Reg -> Instructions
storeRegs r r'
  | r == r' = empty
  | otherwise = singleton (MOV AL F r (ShiftReg r' NSH))

storeToRegister' :: Reg -> Address -> Instructions
storeToRegister' r a = singleton (LDR AL W r a)

updateWithRegister' :: Reg -> Address -> Instructions
updateWithRegister' r a = singleton (STR AL W r a)
  
--Note this is the incorrect behaviour TODO 
offsetToARMOffset :: Int -> Offset
offsetToARMOffset i = Int i

