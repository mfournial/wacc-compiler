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

generatePrintStrRuntime :: RuntimeGenerator
generatePrintStrRuntime = do
 sloc <- newStringLiteral "%.*s\\0" 
 return (RC PrintStr (Define "runtime_print_string" <| 
                     (PUSH [LinkRegister] <|
                     (storeToRegister R1 (RegLoc R0) ><
                     (ADD AL F R2 R0 (ImmOpInt 4) <|
                     (storeToRegister R0 sloc ><
                     (ADD AL F R0 R0 (ImmOpInt 4) <|
                     (BL AL "printf" <|
                     (MOV AL F R0 (ImmOpInt 0) <|
                     (BL AL "fflush" <|
                     (POP [PC] <| empty)))))))))), "runtime_print_string")

generateRuntime :: [RuntimeComponent] -> Instructions
generateRuntime s = mconcat (fmap (\(RC _ ins) -> ins) s)
