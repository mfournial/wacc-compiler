module Code.Generator.Runtime(
  generatePrintStrRuntime,
  generatePrintIntRuntime,
  generateReadCharRuntime,
  generateFreePairRuntime,
  generateRuntimeErrorRuntime,
  generateReadIntRuntime,
  generateRuntime,
  RuntimeGenerator
) where

import Prelude

import Data.Sequence

import Code.Instructions
import Code.Generator.State
import Code.Generator.RetLoc

import Code.Generator.Runtime.Internal

type RuntimeGenerator = ARM (RuntimeComponent, String)

generateRuntimeErrorRuntime :: RuntimeGenerator
generateRuntimeErrorRuntime = do
  let fname = "runtime_throw_runtime_error" in
    return (RC ThrowRuntimeErr (Define fname
                             <| BL AL "runtime_print_String"
                             <| BL AL "exit" <| empty), fname)

generateFreePairRuntime :: RuntimeGenerator
generateFreePairRuntime = do
  sloc <- newStringLiteral "NullReferenceError: dereference a null reference\n\0"
  let fname = "runtime_free_pair" in
    return (RC FreePair (((Define fname
                        <| PUSH [LinkRegister]
                        <| CMP AL R0 (ImmOpInt 0)
                        <| LDR Eq W R0 (Label (label sloc))
                        <| B Eq "runtime_throw_runtime_error"
                        <| PUSH [R0]
                        <| storeToRegisterPure R0 (RegLoc R0))
                        >< BL AL "free"
                        <| storeToRegisterPure R0 (RegLoc StackPointer))
                        |> LDR Eq W R0 (OffReg R0 (Int 4) False)
                        |> BL AL "free"
                        |> POP [R0]
                        |> BL AL "free"
                        |> POP [PC]), fname)

label :: PureRetLoc -> String
label (StringLit s) = s
label _ = error "Kyyyyyyyyle"

generatePrintRefRuntime :: RuntimeGenerator
generatePrintRefRuntime = do
  refloc <- newStringLiteral "%p\0"
  let fname = "runtime_print_reference" in
    return (RC PrintRef ((Define fname
                      <| storeToRegisterPure R1 (Register R0))
                      >< (storeToRegisterPure R0 refloc
                      |> ADD AL F R0 R0 (ImmOpInt 4)
                      |> BL AL "printf")
                      >< (storeToRegisterPure R0 (ImmInt 0)
                      |> BL AL "fflush"
                      |> POP [PC])), fname)

generateArrayCheckRuntime :: RuntimeGenerator
generateArrayCheckRuntime = do
  neg <- newStringLiteral "ArrayIndexOutOfBoundsError: negative index\n\0"
  badIndex <- newStringLiteral "ArrayIndexOutOfBoundsError: index too large\n\0"
  let fname = "runtime_check_array_bounds" in
    return (RC ArrayCheck (Define fname
                        <| CMP AL R0 (ImmOpInt 0)
                        <| LDR LTh W R0 (Label (label neg))
                        <| B LTh "runtime__throw_runtime_error"
                        <| (storeToRegisterPure R1 (RegLoc R1)
                        |> CMP AL R0 (ShiftReg R1 NSH)
                        |> LDR CS W R0 (Label (label badIndex))
                        |> BL CS "runtime__throw_runtime_error"
                        |> POP [PC])), fname)

generatePrintIntRuntime :: RuntimeGenerator
generatePrintIntRuntime = do
  intloc <- newStringLiteral "%d\0"
  let fname = "runtime_print_int" in
    return (RC PrintInt ( Define fname
                       <| PUSH [LinkRegister]
                       <| storeToRegisterPure R1 (Register R0) 
                       >< storeToRegisterPure R0 intloc
                       >< (ADD AL F R0 R0 (ImmOpInt 4)
                       <| BL AL "printf"
                       <| MOV AL F R0 (ImmOpInt 0)
                       <| BL AL "fflush"
                       <| POP [PC]
                       <| empty)), fname)

generatePrintStrRuntime :: RuntimeGenerator
generatePrintStrRuntime = do
 sloc <- newStringLiteral "%.*s\\0"
 let fname = "runtime_print_string" in 
  return (RC PrintStr ((Define fname 
                    <| PUSH [LinkRegister] 
                    <| storeToRegisterPure R1 (RegLoc R0))
                    >< (ADD AL F R2 R0 (ImmOpInt 4) 
                    <| storeToRegisterPure R0 sloc) 
                    >< (ADD AL F R0 R0 (ImmOpInt 4) <| BL AL "printf" 
                    <| MOV AL F R0 (ImmOpInt 0) <| BL AL "fflush"
                    <| POP [PC] <| empty)), fname) 
                   -- Why is there 2 of that instruction? l24 and l26

generateReadCharRuntime :: RuntimeGenerator
generateReadCharRuntime = do
  chloc <- newStringLiteral " %c\0"
  let fname = "runtime_read_char" in
    return (RC ReadChar (Define fname <| scanfCall chloc), fname)

generateDivideByZeroRuntime :: RuntimeGenerator
generateDivideByZeroRuntime = do
  zloc <- newStringLiteral "%d\0"
  let fname = "runtime_dbz" in 
    return (RC Checkdbz (Define fname
                      <| PUSH [LinkRegister]
                      <| CMP AL R1 (ImmOpInt 0)
                      <| LDR Eq W R0 (Label (label zloc))
                      <| BL AL "runtime_throw_runtime_error"
                      <| POP [PC]
                      <| empty), fname)

generateReadIntRuntime :: RuntimeGenerator
generateReadIntRuntime = do
  intloc <- newStringLiteral "%d\0"
  let fname = "runtime_read_int" in
    return (RC ReadInt (Define fname <| scanfCall intloc), fname)

scanfCall :: PureRetLoc -> Instructions
scanfCall loc = (PUSH [LinkRegister] <| storeToRegisterPure R1 (Register R0))
             >< (storeToRegisterPure R0 loc
             |> ADD AL F R0 R0 (ImmOpInt 4)
             |> BL AL "scanf"
             |> POP [PC])

generateRuntime :: [RuntimeComponent] -> Instructions
generateRuntime s = mconcat (fmap (\(RC _ ins) -> ins) s)
