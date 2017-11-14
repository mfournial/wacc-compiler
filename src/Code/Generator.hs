module Code.Generator (produceASM) where

import Code.Instructions

import Data.Waskell.ADT



produceASM :: FilePath -> WaccTree -> IO ()
produceASM f = (writeCode f) . genCode

genCode :: WaccTree -> [Instr]
genCode (WaccTree (Program fs sb)) = concatMap (genFuncCode . getVal) fs ++ genCode' sb

genCode' :: ScopeBlock -> [Instr]
genCode' = undefined

genFuncCode :: Function -> [Instr]
genFuncCode (Function _ fid _ sb)  = concat [[Define (getVal fid)], genCode' sb, [Section "ltorg"]]

writeCode :: FilePath -> [Instr] -> IO ()
writeCode f = (writeFile f) . unlines . (map show)
