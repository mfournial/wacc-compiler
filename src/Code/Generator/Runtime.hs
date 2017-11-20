module Code.Generator.Runtime(
  generatePrintStrRuntime,
  generatePrintIntRuntime,
  generatePrintCharRuntime,
  generatePrintBoolRuntime,
  generatePrintRefRuntime,
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
generateRuntimeErrorRuntime =
  return (RC ThrowRuntimeErr (Define fname
                             <| BL AL (label PrintStr)
                             <| BL AL "exit" <| empty), fname)
  where
    fname = label ThrowRuntimeErr

generateFreePairRuntime :: RuntimeGenerator
generateFreePairRuntime = do
  sloc <- newStringLiteral "NullReferenceError: dereference a null reference\n\0"
  let fname = label FreePair in
    return (RC FreePair (((Define fname
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
                        |> POP [PC]), fname)

address :: PureRetLoc -> Address
address (StringLit s) = Label s
address _ = error "Kyyyyyyyyle"

generatePrintBoolRuntime :: RuntimeGenerator
generatePrintBoolRuntime = do
  trueloc <- newStringLiteral "true"
  falseloc <- newStringLiteral "false"
  let fname = label PrintBool in
    return (RC PrintBool (Define fname
                       <| POP [LinkRegister]
                       <| CMP AL R0 (ImmOpInt 0)
                       <| LDR Neq W R0 (address trueloc)
                       <| LDR Eq W R0 (address falseloc)
                       <| ADD AL F R0 R0 (ImmOpInt 4)
                       <| BL AL "printf"
                       <| MOV AL F R0 (ImmOpInt 0)
                       <| BL AL "fflush"
                       <| POP [PC]
                       <| empty), fname)

generatePrintCharRuntime :: RuntimeGenerator
generatePrintCharRuntime = do
  let fname = label PrintChar in
    return (RC PrintChar (Define fname
                       <| PUSH [LinkRegister]
                       <| BL AL "putchar"
                       <| POP [PC] <| empty), fname)

generatePrintRefRuntime :: RuntimeGenerator
generatePrintRefRuntime = do
  refloc <- newStringLiteral "%p\0"
  let fname = label PrintRef in
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
  negIndex <- newStringLiteral "ArrayIndexOutOfBoundsError: negative index\n\0"
  badIndex <- newStringLiteral "ArrayIndexOutOfBoundsError: index too large\n\0"
  let fname = label ArrayCheck in
    return (RC ArrayCheck (Define fname
                        <| CMP AL R0 (ImmOpInt 0)
                        <| LDR LTh W R0 (address negIndex)
                        <| B LTh (label ThrowRuntimeErr)
                        <| (storeToRegisterPure R1 (RegLoc R1)
                        |> CMP AL R0 (ShiftReg R1 NSH)
                        |> LDR CS W R0 (address badIndex)
                        |> BL CS (label ThrowRuntimeErr)
                        |> POP [PC])), fname)

generatePrintIntRuntime :: RuntimeGenerator
generatePrintIntRuntime = do
  intloc <- newStringLiteral "%d\0"
  let fname = label PrintInt in
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
 let fname = label PrintStr in 
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
  let fname = label ReadChar in
    return (RC ReadChar (Define fname <| scanfCall chloc), fname)

generateDivideByZeroRuntime :: RuntimeGenerator
generateDivideByZeroRuntime = do
  zloc <- newStringLiteral "%d\0"
  let fname = label Checkdbz in 
    return (RC Checkdbz (Define fname
                      <| PUSH [LinkRegister]
                      <| CMP AL R1 (ImmOpInt 0)
                      <| LDR Eq W R0 (address zloc)
                      <| BL AL (label ThrowRuntimeErr)
                      <| POP [PC]
                      <| empty), fname)

generateReadIntRuntime :: RuntimeGenerator
generateReadIntRuntime = do
  intloc <- newStringLiteral "%d\0"
  let fname = label ReadInt in
    return (RC ReadInt (Define fname <| scanfCall intloc), fname)

scanfCall :: PureRetLoc -> Instructions
scanfCall loc = (PUSH [LinkRegister] <| storeToRegisterPure R1 (Register R0))
             >< (storeToRegisterPure R0 loc
             |> ADD AL F R0 R0 (ImmOpInt 4)
             |> BL AL "scanf"
             |> POP [PC])

generateRuntime :: [RuntimeComponent] -> Instructions
generateRuntime s = mconcat (fmap (\(RC _ ins) -> ins) s)
