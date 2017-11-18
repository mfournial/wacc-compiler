module Code.Generator.Runtime(RuntimeComponent, generateRuntime) where

import Prelude hiding (concat)

import Data.Sequence
import Data.Sequence.Util

import Code.Instructions
import Code.Generator.State
import Code.Generator.RetLoc

import Code.Generator.Runtime.Internal

generateExitRuntime :: ARM RuntimeComponent
generateExitRuntime = undefined

generatePrintStrRuntime :: ARM RuntimeComponent
generatePrintStrRuntime = do
 sloc <- newStringLiteral "%.*s\0" 
 return $ RC (Define "runtime_print_string" <| 
             (PUSH [LinkRegister] <|
             (storeToRegister R1 (RegLoc R0) ><
             (ADD AL F R2 R0 (ImmOpInt 4) <|
             (storeToRegister R0 sloc ><
             (ADD AL F R0 R0 (ImmOpInt 4) <|
             (BL AL "printf" <|
             (MOV AL F R0 (ImmOpInt 4) <|
             (BL AL "fflush" <|
             (POP [PC] <| empty))))))))))

generateRuntime :: Seq RuntimeComponent -> Instructions
generateRuntime s = concat (fmap (\(RC ins) -> ins) s)
