module Code.Generator (produceASM) where

import Data.Foldable (toList)
import Data.Sequence
import Prelude hiding (concat)

import Code.Generator.State
import Code.Instructions
import Data.Waskell.ADT 

produceASM :: FilePath -> WaccTree -> IO ()
produceASM f = writeCode f . genCode 

genCode :: WaccTree -> Seq Instr
genCode t = dataSection (strLits st) >< instr
  where
   (instr, st) = runState (genCode' t) (Junk empty) 

genCode' :: WaccTree -> ARM Instructions
genCode' (WaccTree (Program fps sb)) = do
  finstr <- genFuncsCode (fromList $ map getVal fps)
  minstr <- genScopeBlock sb
  return $ ((empty |> DIVIDER |> DIVIDER -- TODO put data section at the front
           |> Section "text")
           >< (finstr
           |> Global |> Define "main" |> PUSH [LinkRegister]))
           >< (minstr
           |> LDR AL W R0 (Const 0) |> POP [PC] |> FunSection "ltorg")
  where
    genFuncsCode :: Seq Function -> ARM Instructions
    genFuncsCode fs = fmap concat $ mapM genFuncCodeWithDiv fs
    genFuncCodeWithDiv :: Function -> ARM Instructions
    genFuncCodeWithDiv f = genFuncCode f >>= \k -> return (k |> DIVIDER)

genFuncCode :: Function -> ARM Instructions
genFuncCode = undefined

  --   (instr, state) = runState genInstrs []
  --   genFuncsCode :: ARM ([Function] -> Seq Instr)
  --   genFuncsCode = undefined
  --   concatMap ((genFuncCode state) . getVal) fs

genScopeBlock :: ScopeBlock -> ARM Instructions
genScopeBlock = undefined

-- genFuncCode :: ARM (Function -> Seq Instr)
-- genFuncCode state (Function _ fid _ sb)  = 
--   concat 
--   [ [Define (getVal fid)]
--   , genCode' state sb, [Section "ltorg"]
--   ]

writeCode :: FilePath -> Instructions -> IO ()
writeCode = (. (unlines . toList . fmap printARM)) . writeFile
