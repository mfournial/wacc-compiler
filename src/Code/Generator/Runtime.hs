module Code.Generator.Runtime(generatePrintStrRuntime, generateRuntime, RuntimeGenerator) where

import Prelude

import Data.Sequence

import Code.Instructions
import Code.Generator.State
import Code.Generator.RetLoc

import Code.Generator.Runtime.Internal

type RuntimeGenerator = ARM (RuntimeComponent, String)

generateExitRuntime :: RuntimeGenerator 
generateExitRuntime = undefined

-- | Print char isn't a function - just put the char in R0 and call it
generatePrintChrRuntime :: Instr -- ^ 
generatePrintChrRuntime = BL AL "putchar"

generatePrintIntRuntime :: RuntimeGenerator
generatePrintIntRuntime = do
  intloc <- newStringLiteral "%d\0"
  let fname = "runtime_print_int" in
    return (RC PrintStr ( Define fname
                       <| PUSH [LinkRegister]
                       <| storeToRegisterPure R1 (RegLoc R0) 
                       >< (ADD AL F R0 R0 (ImmOpInt 4)
                       <| BL AL "printf"
                       <| MOV AL F R0 (ImmOpInt 0)
                       <| BL AL "fflush"
                       <| POP [PC]
                       <| empty)), fname)

generatePrintStrRuntime :: RuntimeGenerator
generatePrintStrRuntime = do
 sloc <- newStringLiteral "%.*s\\0" 
 return (RC PrintStr (Define "runtime_print_string" <| 
                     (PUSH [LinkRegister] <|
                     (storeToRegisterPure R1 (RegLoc R0) ><
                     (ADD AL F R2 R0 (ImmOpInt 4) <|
                     (storeToRegisterPure R0 sloc ><
                     (ADD AL F R0 R0 (ImmOpInt 4) <|
                     (BL AL "printf" <|
                     (MOV AL F R0 (ImmOpInt 0) <|
                     (BL AL "fflush" <|
                     (POP [PC] <| empty)))))))))), "runtime_print_string") -- Why is there 2 of that instruction? l24 and l26

generateReadCharRuntime :: RuntimeGenerator
generateReadCharRuntime = do
  chloc <- newStringLiteral " %c\0"
  let fname = "runtime_read_char" in
    return (RC PrintStr (Define fname <| scanfCall chloc), fname)

generateReadIntRuntime :: RuntimeGenerator
generateReadIntRuntime = do
  intloc <- newStringLiteral "%d\0"
  let fname = "runtime_read_int" in
    return (RC PrintStr (Define fname <| scanfCall intloc), fname)

scanfCall :: PureRetLoc -> Instructions
scanfCall loc = (PUSH [LinkRegister] <| storeToRegisterPure R1 (RegLoc R0))
             >< (storeToRegisterPure R0 loc
             |> ADD AL F R0 R0 (ImmOpInt 4)
             |> BL AL "scanf"
             |> POP [PC])

generateRuntime :: [RuntimeComponent] -> Instructions
generateRuntime s = mconcat (fmap (\(RC _ ins) -> ins) s)
