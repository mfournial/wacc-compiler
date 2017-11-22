module Code.Generator.Runtime(
  generateRuntime,
  branchTo
) where

import Prelude hiding (concat)
import Data.Maybe(fromJust)
import Data.Sequence

import Code.Instructions
import Code.Generator.ARM
import Code.Generator.State

generateRuntime :: RCID -> ARM Instructions
generateRuntime = generate

branchTo :: RCID -> ARM Instr
branchTo name = do
  addToRuntime name
  return $ BL AL (label name)

names :: [(RCID, String)]
names = [ (PrintStr, "runtime_print_string")
        , (PrintInt, "runtime_print_int")
        , (PrintChar, "runtime_print_char")
        , (PrintBool, "runtime_print_bool")
        , (PrintRef, "runtime_print_ref")
        , (ReadInt, "runtime_read_int")
        , (ReadChar, "runtime_read_char")
        , (ThrowRuntimeErr, "runtime_throw_err")
        , (Free, "runtime_free_pair")
        , (ArrayCheck, "runtime_array_check")
        , (Checkdbz, "runtime_check_division_by_zero")
        , (ThrowOverflowErr, "runtime_throw_overflow")
        ]

label :: RCID -> String
label = fromJust . flip lookup names

generate :: RCID -> ARM Instructions
generate ThrowRuntimeErr =
  return $ Define (label ThrowRuntimeErr)
        <| BL AL (label PrintStr)
        <| MOV AL F R0 (ImmOpInt (-1))
        <| BL AL "exit"
        <| empty

generate Free = do
  sloc <- newStringLiteral "NullReferenceError: dereference a null reference\n\0"
  return $ (Define (label Free)
        <| PUSH [LinkRegister, R1]
        <| storeToRegisterPure R1 (RegLoc R0))
        |> CMP AL R1 (ImmOpInt 0)
        |> LDR Eq W R0 (address sloc)
        |> B Eq (label ThrowRuntimeErr)
        |> BL AL "free"
        |> POP [R1, PC]

generate PrintBool = do
  trueloc  <- newStringLiteral "true\0"
  falseloc <- newStringLiteral "false\0"
  return $ Define (label PrintBool)
        <| PUSH [LinkRegister]
        <| CMP AL R0 (ImmOpInt 0)
        <| LDR Neq W R0 (address trueloc)
        <| LDR Eq W R0 (address falseloc)
        <| ADD AL F R0 R0 (ImmOpInt 4)
        <| BL AL "printf"
        <| MOV AL F R0 (ImmOpInt 0)
        <| BL AL "fflush"
        <| POP [PC]
        <| empty

generate PrintChar =
    return $ Define (label PrintChar)
          <| PUSH [LinkRegister]
          <| BL AL "putchar"
          <| POP [PC]
          <| empty

generate PrintRef = do
  refloc <- newStringLiteral "%p\0"
  return $ (Define (label PrintRef)
        <| PUSH [LinkRegister, R1]
        <| storeToRegisterPure R1 (Register R0))
        >< (storeToRegisterPure R0 refloc
        |> ADD AL F R0 R0 (ImmOpInt 4)
        |> BL AL "printf")
        >< (storeToRegisterPure R0 (ImmInt 0)
        |> BL AL "fflush"
        |> POP [PC, R1])

generate ArrayCheck = do
  negIndex <- newStringLiteral "ArrayIndexOutOfBoundsError: negative index\n\0"
  badIndex <- newStringLiteral "ArrayIndexOutOfBoundsError: index too large\n\0"
  return $ Define (label ArrayCheck)
        <| PUSH [LinkRegister, R0, R1, R2]
        <| MOV AL F R2 (ShiftReg R0 NSH)
        <| MOV AL F R0 (ShiftReg R1 NSH)
        <| MOV AL F R1 (ShiftReg R2 NSH)
        <| CMP AL R0 (ImmOpInt 0)
        <| LDR LTh W R0 (address negIndex)
        <| B LTh (label ThrowRuntimeErr)
        <| (storeToRegisterPure R1 (RegLoc R1)
        |> CMP AL R0 (ShiftReg R1 NSH)
        |> LDR GE W R0 (address badIndex)
        |> BL GE (label ThrowRuntimeErr)
        |> POP [R2, R1, R0, PC])

generate PrintInt = do
  intloc <- newStringLiteral "%d\0"
  return $ Define (label PrintInt)
       <| PUSH [LinkRegister, R1]
       <| storeToRegisterPure R1 (Register R0) 
       >< storeToRegisterPure R0 intloc
       >< (ADD AL F R0 R0 (ImmOpInt 4)
       <| BL AL "printf"
       <| MOV AL F R0 (ImmOpInt 0)
       <| BL AL "fflush"
       <| POP [R1, PC]
       <| empty)

generate PrintStr = do
 sloc <- newStringLiteral "%.*s\0"
 return $ (Define (label PrintStr) 
       <| PUSH [LinkRegister, R0, R1] 
       <| storeToRegisterPure R1 (RegLoc R0))
       >< (ADD AL F R2 R0 (ImmOpInt 4) 
       <| storeToRegisterPure R0 sloc) 
       >< (ADD AL F R0 R0 (ImmOpInt 4) <| BL AL "printf" 
       <| MOV AL F R0 (ImmOpInt 0) <| BL AL "fflush"
       <| POP [R1, R0, PC]
       <| empty)

generate ReadChar = do
  chloc <- newStringLiteral " %c\0"
  return $ Define (label ReadChar) <| scanfCall chloc

generate Checkdbz = do
  zloc <- newStringLiteral "DivideByZeroError: divide or modulo by zero\0"
  return $ Define (label Checkdbz)
        <| PUSH [LinkRegister]
        <| CMP AL R1 (ImmOpInt 0)
        <| LDR Eq W R0 (address zloc)
        <| BL AL (label ThrowRuntimeErr)
        <| POP [PC]
        <| empty

generate ReadInt = do
  intloc <- newStringLiteral "%d\0"
  return $ Define (label ReadInt) <| scanfCall intloc

generate ThrowOverflowErr = do
  msgloc <- newStringLiteral "OverflowError: the result is too small/large to store in a 4-byte signed-integer.\n\0"
  return $ Define (label ThrowOverflowErr)
        <| (storeToRegisterPure R0 msgloc
        |> BL AL (label ThrowRuntimeErr))

scanfCall :: PureRetLoc -> Instructions
scanfCall loc = (PUSH [LinkRegister] <| storeToRegisterPure R1 (Register R0))
             >< (storeToRegisterPure R0 loc
             |> ADD AL F R0 R0 (ImmOpInt 4)
             |> BL AL "scanf"
             |> POP [PC])

address :: PureRetLoc -> Address
address (StringLit s) = Label s
address _ = error "Kyyyyyyyyle"
