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

produceASM :: FilePath -> WaccTree -> IO ()
-- produceASM f = writeCode f . genCode 
produceASM = (.genCode) . writeCode

genCode :: WaccTree -> Seq Instr
genCode t =  dataSec' >< instr
  where
    dataSec  = dataSection (strLits st)
    dataSec' = if null dataSec then empty else dataSec |> DIVIDER |> DIVIDER
    (instr, st) = runState (genCode' t) newState 

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

genFuncCode :: Function -> ARM Instructions
genFuncCode (Function _ iden params sb) = do
  newEnv emptyScope
  mapM_ pushVar (getIden params)
  (lr, _) <- push [LinkRegister]
  body <- genScopeBlock sb
  mapM_ (const decrementStack) [0.. length params] -- Note we're canceling our local effect of pushing the args to the stack plus the Linkregister
  closeEnv
  return $ ((Define ("fun_" ++ getVal iden)
        <| lr
        <| body)
        |> FunSection "ltorg"
        |> DIVIDER)
  where
    getIden [] = []
    getIden (Param _ piden : ps) = getIden ps ++ [getVal piden]

writeCode :: FilePath -> Instructions -> IO ()
writeCode = (. (unlines . toList . fmap printARM)) . writeFile

