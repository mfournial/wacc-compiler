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

data RCID = PrintStr
          | PrintInt
          | PrintChar
          | PrintBool
          | ReadInt
          | ReadChar
          | ThrowRuntimeErr
          | FreePair
          | PrintRef
          | ArrayCheck
          | Checkdbz
          deriving (Eq)

data RetLoc = PRL PureRetLoc | StackPtr Int
  deriving (Show)

data PureRetLoc = HeapAddr Int
                | StringLit String
                | RegLoc Reg
                | RegLocOffset Reg Int
                | Register Reg
                | ImmChar Char
                | ImmInt Int
  deriving (Show)

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
