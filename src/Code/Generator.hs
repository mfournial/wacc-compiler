module Code.Generator (produceASM) where

import Code.Instructions

import Data.Waskell.ADT



produceASM :: FilePath -> WaccTree -> IO ()
produceASM f = (writeCode f) . genCode

genCode :: WaccTree -> [Instr]
genCode = undefined

writeCode :: FilePath -> [Instr] -> IO ()
writeCode f = (writeFile f) . unlines . (map show)
