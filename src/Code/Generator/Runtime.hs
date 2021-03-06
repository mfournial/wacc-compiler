module Code.Generator.Runtime(
  generateRuntime,
  branchTo,
  branchToIf
) where

import Prelude hiding (concat)
import Data.Maybe(fromJust)
import Data.Sequence

import Code.Instructions
import Code.Generator.ARM
import Code.Generator.State

-- | generates runtime ARM Instructions based on the runtime Id
generateRuntime :: RCID -> ARM Instructions
generateRuntime = generate

-- | Creates a branch instructions and always jumps to the label
branchTo :: RCID -> ARM Instr
branchTo = branchToIf AL

-- | Creates an branch instruction to jump to the correct label if condition is met
branchToIf :: Condition -> RCID -> ARM Instr
branchToIf cond name = do
  addToRuntime name
  return $ BL cond (label name)

-- | All the runtime component identifiers with their coressponding string labels
names :: [(RCID, String)]
names = [ (PrintStr, "runtime_print_string")
        , (PrintInt, "runtime_print_int")
        , (PrintChar, "runtime_print_char")
        , (PrintCharArray, "runtime_print_char_array")
        , (PrintBool, "runtime_print_bool")
        , (PrintRef, "runtime_print_ref")
        , (ReadInt, "runtime_read_int")
        , (ReadChar, "runtime_read_char")
        , (ThrowRuntimeErr, "runtime_throw_err")
        , (ThrowDerefRuntimeErr, "runtime_throw_deref_err")
        , (Free, "runtime_free")
        , (ArrayCheck, "runtime_array_check")
        , (Checkdbz, "runtime_check_division_by_zero")
        , (NullCheck, "runtime_null_check")
        , (ThrowOverflowErr, "runtime_throw_overflow")
        ]

-- | Returns the label for each runtime component
label :: RCID -> String
label = fromJust . flip lookup names

-- | Generates ARM instructions which checks for errors during runtime and prints the error
generate :: RCID -> ARM Instructions
generate ThrowRuntimeErr =
  return $ Define (label ThrowRuntimeErr)
        <| BL AL (label PrintStr)
        <| MOV AL F R0 (ImmOpInt (-1))
        <| BL AL "exit"
        <| empty

generate ThrowDerefRuntimeErr =
  return $ Define (label ThrowDerefRuntimeErr)
        <| BL AL (label PrintStr)
        <| MOV AL F R0 (ImmOpInt (134))
        <| BL AL "exit"
        <| empty

generate Free = do
  sloc <- newStringLiteral "NullReferenceError: dereference a null reference\n\0"
  return $ (Define (label Free)
        <| PUSH [LinkRegister, R0, R1]
        <| CMP AL R0 (ImmOpInt 0)
        <| LDR Eq W R0 (address sloc)
        <| BL Eq (label ThrowDerefRuntimeErr)
        <| storePure R1 (RegLoc R0))
        |> CMP AL R1 (ImmOpInt 0)
        |> LDR Eq W R0 (address sloc)
        |> B Eq (label ThrowDerefRuntimeErr)
        |> BL AL "free"
        |> POP [R0, R1, PC]

generate PrintBool = do
  trueloc  <- newStringLiteral "true\0"
  falseloc <- newStringLiteral "false\0"
  return $ Define (label PrintBool)
        <| PUSH [LinkRegister, R0]
        <| CMP AL R0 (ImmOpInt 0)
        <| LDR Neq W R0 (address trueloc)
        <| LDR Eq W R0 (address falseloc)
        <| ADD AL F R0 R0 (ImmOpInt 4)
        <| BL AL "printf"
        <| MOV AL F R0 (ImmOpInt 0)
        <| BL AL "fflush"
        <| POP [R0, PC]
        <| empty

generate PrintChar =
  return $ Define (label PrintChar)
        <| PUSH [LinkRegister, R0]
        <| BL AL "putchar"
        <| POP [R0, PC]
        <| empty


generate PrintCharArray =
  return $ Define (label PrintCharArray)
        <| PUSH [LinkRegister, R0, R1, R2, R3]
        <| LDR AL W R1 (OffReg R0 (Int 0) False)
        <| MOV AL F R2 (ShiftReg R0 NSH)
        <| ADD AL F R3 R0 (ImmOpInt 4)
        <| LDR AL W R2 (OffReg R3 (Int 0) False)
        <| CMP AL R2 (ImmOpInt 255)
        <| B GTh "runtime_print_char_array_return"
        <| B AL "runtime_print_char_array_cond"
        <| Define "runtime_print_char_array_body"
        <| LDR AL W R0 (OffReg R3 (Int 0) False)
        <| PUSH [R1, R3]        
        <| BL AL "putchar"
        <| POP [R3, R1]
        <| ADD AL F R3 R3 (ImmOpInt 4)
        <| SUB AL F R1 R1 (ImmOpInt 1)
        <| Define "runtime_print_char_array_cond"
        <| CMP AL R1 (ImmOpInt 0)
        <| B Neq "runtime_print_char_array_body"
        <| POP [R3, R2, R1, R0, PC]
        <| Define "runtime_print_char_array_return"
        <| BL AL (label PrintStr)
        <| POP [R3, R2, R1, R0, PC]
        <| empty

generate PrintRef = do
  refloc <- newStringLiteral "%p\0"
  return $ (Define (label PrintRef)
        <| PUSH [LinkRegister, R0, R1]
        <| storePure R1 (Register R0))
        >< (storePure R0 refloc
        |> ADD AL F R0 R0 (ImmOpInt 4)
        |> BL AL "printf")
        >< (storePure R0 (ImmInt 0)
        |> BL AL "fflush"
        |> POP [R1, R0, PC])

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
        <| (storePure R1 (RegLoc R1)
        |> CMP AL R0 (ShiftReg R1 NSH)
        |> LDR GE W R0 (address badIndex)
        |> BL GE (label ThrowRuntimeErr)
        |> POP [R2, R1, R0, PC])

generate PrintInt = do
  intloc <- newStringLiteral "%d\0"
  return $ Define (label PrintInt)
       <| PUSH [LinkRegister, R0, R1]
       <| storePure R1 (Register R0) 
       >< storePure R0 intloc
       >< (ADD AL F R0 R0 (ImmOpInt 4)
       <| BL AL "printf"
       <| MOV AL F R0 (ImmOpInt 0)
       <| BL AL "fflush"
       <| POP [R1, R0, PC]
       <| empty)

generate PrintStr = do
 sloc <- newStringLiteral "%.*s\0"
 return $ (Define (label PrintStr) 
       <| PUSH [LinkRegister, R0, R1, R2] 
       <| storePure R1 (RegLoc R0))
       >< (ADD AL F R2 R0 (ImmOpInt 4) 
       <| storePure R0 sloc) 
       >< (ADD AL F R0 R0 (ImmOpInt 4) <| BL AL "printf" 
       <| MOV AL F R0 (ImmOpInt 0) <| BL AL "fflush"
       <| POP [R2, R1, R0, PC]
       <| empty)

generate Checkdbz = do
  zloc <- newStringLiteral "DivideByZeroError: divide or modulo by zero\0"
  return $ Define (label Checkdbz)
        <| PUSH [R0, R1, LinkRegister]
        <| CMP AL R1 (ImmOpInt 0)
        <| LDR Eq W R0 (address zloc)
        <| BL Eq (label ThrowRuntimeErr)
        <| POP [R0, R1, PC]
        <| empty

generate NullCheck = do
  zloc <- newStringLiteral "NullReferenceError: dereference a null reference\0"
  return $ Define (label NullCheck)
        <| PUSH [R0, LinkRegister]
        <| CMP AL R0 (ImmOpInt 0)
        <| LDR Eq W R0 (address zloc)
        <| BL Eq (label ThrowRuntimeErr)
        <| POP [R0, PC]
        <| empty

generate ReadChar = do
  chloc <- newStringLiteral " %c\0"
  return $ Define (label ReadChar) <| scanfCall chloc

generate ReadInt = do
  intloc <- newStringLiteral "%d\0"
  return $ Define (label ReadInt) <| scanfCall intloc

generate ThrowOverflowErr = do
  err <- newStringLiteral "Over/UnderflowError: the result is too large/small to store in a 4-byte signed-integer.\n\0"
  return $ Define (label ThrowOverflowErr)
        <| (storePure R0 err
        |> BL AL (label ThrowRuntimeErr))

-- | Generates ARM Instructions to get value from input
scanfCall :: PureLocation -> Instructions
scanfCall loc = (PUSH [R0, R1, LinkRegister] <| storePure R1 (Register R0))
             >< (storePure R0 loc
             |> ADD AL F R0 R0 (ImmOpInt 4)
             |> BL AL "scanf"
             |> POP [R0, R1, PC])

-- | returns an address  
address :: PureLocation -> Address
address (StringLit s) = Label s
address _ = error "Address type is not a String Literal"
