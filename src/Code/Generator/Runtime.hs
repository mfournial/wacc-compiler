module Code.Generator.Runtime(
  generateRuntime,
  branchTo,
  RuntimeGenerator,
  RCID(..)
) where

import Prelude hiding (concat)

import Data.Sequence

import Code.Instructions
import Code.Generator.State
import Code.Generator.RetLoc
import Data.Sequence.Util(concat)
import Code.Generator.StateInstructions
import Code.Generator.Runtime.Internal

type RuntimeGenerator = ARM (RuntimeComponent, String)

generateRuntime' :: RCID -> ARM Instructions
generateRuntime' ThrowRuntimeErr =
  return $ Define (label ThrowRuntimeErr)
        <| BL AL (label PrintStr)
        <| BL AL "exit"
        <| empty

generateRuntime' FreePair = do
  sloc <- newStringLiteral "NullReferenceError: dereference a null reference\n\0"
  return $ ((Define (label FreePair)
          <| PUSH [LinkRegister]
          <| CMP AL R0 (ImmOpInt 0)
          <| LDR Eq W R0 (address sloc)
          <| B Eq (label ThrowRuntimeErr)
          <| PUSH [R0]
          <| storeToRegisterPure R0 (RegLoc R0))
          >< BL AL "free"
          <| storeToRegisterPure R0 (RegLoc StackPointer))
          |> LDR Eq W R0 (OffReg R0 (Int 4) False)
          |> BL AL "free"
          |> POP [R0]
          |> BL AL "free"
          |> POP [PC]

generateRuntime' PrintBool = do
  trueloc  <- newStringLiteral "true"
  falseloc <- newStringLiteral "false"
  return $ Define (label PrintBool)
        <| POP [LinkRegister]
        <| CMP AL R0 (ImmOpInt 0)
        <| LDR Neq W R0 (address trueloc)
        <| LDR Eq W R0 (address falseloc)
        <| ADD AL F R0 R0 (ImmOpInt 4)
        <| BL AL "printf"
        <| MOV AL F R0 (ImmOpInt 0)
        <| BL AL "fflush"
        <| POP [PC]
        <| empty

generateRuntime' PrintChar =
    return $ Define (label PrintChar)
          <| PUSH [LinkRegister]
          <| BL AL "putchar"
          <| POP [PC]
          <| empty

generateRuntime' PrintRef = do
  refloc <- newStringLiteral "%p\0"
  return $ (Define (label PrintRef)
        <| storeToRegisterPure R1 (Register R0))
        >< (storeToRegisterPure R0 refloc
        |> ADD AL F R0 R0 (ImmOpInt 4)
        |> BL AL "printf")
        >< (storeToRegisterPure R0 (ImmInt 0)
        |> BL AL "fflush"
        |> POP [PC])

generateRuntime' ArrayCheck = do
  negIndex <- newStringLiteral "ArrayIndexOutOfBoundsError: negative index\n\0"
  badIndex <- newStringLiteral "ArrayIndexOutOfBoundsError: index too large\n\0"
  return $ Define (label ArrayCheck)
        <| CMP AL R0 (ImmOpInt 0)
        <| LDR LTh W R0 (address negIndex)
        <| B LTh (label ThrowRuntimeErr)
        <| (storeToRegisterPure R1 (RegLoc R1)
        |> CMP AL R0 (ShiftReg R1 NSH)
        |> LDR CS W R0 (address badIndex)
        |> BL CS (label ThrowRuntimeErr)
        |> POP [PC])

generateRuntime' PrintInt = do
  intloc <- newStringLiteral "%d\0"
  return $ Define (label PrintInt)
       <| PUSH [LinkRegister]
       <| storeToRegisterPure R1 (Register R0) 
       >< storeToRegisterPure R0 intloc
       >< (ADD AL F R0 R0 (ImmOpInt 4)
       <| BL AL "printf"
       <| MOV AL F R0 (ImmOpInt 0)
       <| BL AL "fflush"
       <| POP [PC]
       <| empty)

generateRuntime' PrintStr = do
 sloc <- newStringLiteral "%.*s\\0"
 return $ (Define (label PrintStr) 
       <| PUSH [LinkRegister] 
       <| storeToRegisterPure R1 (RegLoc R0))
       >< (ADD AL F R2 R0 (ImmOpInt 4) 
       <| storeToRegisterPure R0 sloc) 
       >< (ADD AL F R0 R0 (ImmOpInt 4) <| BL AL "printf" 
       <| MOV AL F R0 (ImmOpInt 0) <| BL AL "fflush"
       <| POP [PC]
       <| empty)

generateRuntime' ReadChar = do
  chloc <- newStringLiteral " %c\0"
  return $ Define (label ReadChar) <| scanfCall chloc

generateRuntime' Checkdbz = do
  zloc <- newStringLiteral "%d\0"
  return $ Define (label Checkdbz)
        <| PUSH [LinkRegister]
        <| CMP AL R1 (ImmOpInt 0)
        <| LDR Eq W R0 (address zloc)
        <| BL AL (label ThrowRuntimeErr)
        <| POP [PC]
        <| empty

generateRuntime' ReadInt = do
  intloc <- newStringLiteral "%d\0"
  return $ Define (label ReadInt) <| scanfCall intloc

scanfCall :: PureRetLoc -> Instructions
scanfCall loc = (PUSH [LinkRegister] <| storeToRegisterPure R1 (Register R0))
             >< (storeToRegisterPure R0 loc
             |> ADD AL F R0 R0 (ImmOpInt 4)
             |> BL AL "scanf"
             |> POP [PC])

address :: PureRetLoc -> Address
address (StringLit s) = Label s
address _ = error "Kyyyyyyyyle"

generateRuntime :: ARM Instructions
generateRuntime = do
  ids <- runtimeInstructions
  instrs <- mapM generateRuntime' ids
  return $ concat instrs

generateRuntime'' :: [RuntimeComponent] -> Instructions
generateRuntime'' s = mconcat (fmap (\(RC _ ins) -> ins) s)

branchTo :: RCID -> ARM Instr
branchTo name = do
  addToRuntime name
  return $ BL AL (label name)