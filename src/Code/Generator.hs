module Code.Generator (produceASM) where

import Data.Foldable (toList)
import Data.Sequence hiding (length)
import Data.Sequence.Util
import Prelude hiding (null, concat)

import Code.Generator.State
import Code.Generator.Statement
import Code.Generator.Runtime
import Code.Instructions
import Data.Waskell.ADT 
import Data.Waskell.Scope(emptyScope)

-- | Generate ARM Instructions and Write to file based on filepath 
produceASM :: FilePath -> WaccTree -> IO ()
produceASM = (.genCode) . writeCode

-- | Takes a AST and generates ARM Instructions
-- | Split into data section and text section. If data section is empty then there should be no data section in the file
genCode :: WaccTree -> Seq Instr
genCode t =  dataSec' >< instr
  where
    dataSec  = dataSection (strLits st)
    dataSec' = if null dataSec then empty else dataSec |> DIVIDER
    (instr, st) = runState (genCode' t) newState 

-- | Generates the ARM Instructions
-- | Split into generate ARM function instructions, ARM Instruction for the main and the runtime instructions that are used by the functions and the main
-- | the runtimeInstructions should only return runtime code that is needed by the functions and the main
genCode' :: WaccTree -> ARM Instructions
genCode' (WaccTree (Program fcs sb)) = do
  finstr <- genFuncsCode $ fromList $ map getVal fcs
  minstr <- genScopeBlock sb
  ids <- runtimeInstructions
  instrs <- mapM generateRuntime ids
  let rinstr = concat instrs
  return $ ((empty
           |> Section "text" |> DIVIDER)
           >< ((Global <| finstr)
           |> Define "main" |> PUSH [LinkRegister]))
           >< (minstr
           |> LDR AL W R0 (Const 0) |> POP [PC] |> FunSection "ltorg")
           >< rinstr
  where
    genFuncsCode :: Seq Function -> ARM Instructions
    genFuncsCode fs = concat <$> mapM genFuncCode fs

-- | Generates ARM instructions for functions
-- | Callee saves convention - we're canceling our local effect of pushing the args to the stack plus the Linkregister
genFuncCode :: Function -> ARM Instructions
genFuncCode (Function _ iden params sb) = do
  newEnv emptyScope
  mapM_ pushVar (getIden params)
  (lr, _) <- push [LinkRegister]
  body <- genScopeBlock sb
  mapM_ (const decrementStack) [0.. length params] 
  closeEnv
  return $ ((Define ("fun_" ++ getVal iden)
        <| lr
        <| body)
        |> FunSection "ltorg"
        |> DIVIDER)
  where
    getIden [] = []
    getIden (Param _ piden : ps) = getIden ps ++ [getVal piden]

-- | Takes a filepath and a sequence of instructions and write them to file
writeCode :: FilePath -> Instructions -> IO ()
writeCode = (. (unlines . toList . fmap printARM)) . writeFile

