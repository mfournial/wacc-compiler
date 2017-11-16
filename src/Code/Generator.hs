module Code.Generator (produceASM) where

import Data.Char(intToDigit)
import Control.Monad.State.Lazy

import Code.Instructions
import Data.Waskell.ADT 

type Ass = State Junk

newStringLiteral :: String -> Ass ()
newStringLiteral str = state $ \junk -> ((), junk{strLits = strLits junk ++ [str]})  

data Junk = Junk {
  strLits:: Data
}

type Data = [String]

dataSection :: Data -> [Instr]
dataSection [] = []
dataSection strs = (Section "data") : concat (zipWith dataElem labels strs)
  where
    labels = map (("msg_" ++) . (pure . intToDigit)) [0..]
    dataElem label str = [Define label, Word (length str), Ascii str]

produceASM :: FilePath -> WaccTree -> IO ()
produceASM f = writeCode f . genCode 

genCode :: WaccTree -> [Instr]
genCode t = dataSection (strLits st) ++ instr
  where
   (instr, st) = runState (genCode' t) (Junk []) 

genCode' :: WaccTree -> Ass [Instr]
genCode' (WaccTree (Program fps sb)) = do
  finstr <- genFuncsCode (map getVal fps)
  minstr <- genScopeBlock sb
  return $ concat [ [DIVIDER, DIVIDER],
                    [Section "text"],
                    finstr,
                    [Global, Define "main", PUSH [LinkRegister]],
                    minstr,
                    [LDR AL W R0 (Const 0), POP [PC], FunSection "ltorg"] ] 
  where
    genFuncsCode :: [Function] -> Ass [Instr]
    genFuncsCode fs = fmap concat $ mapM genFuncCodeWithDiv fs
    genFuncCodeWithDiv :: Function -> Ass [Instr]
    genFuncCodeWithDiv f = genFuncCode f >>= \k -> return (k ++ [DIVIDER])

genFuncCode :: Function -> Ass [Instr]
genFuncCode = undefined

  --   (instr, state) = runState genInstrs []
  --   genFuncsCode :: Ass ([Function] -> [Instr])
  --   genFuncsCode = undefined
  --   concatMap ((genFuncCode state) . getVal) fs

genScopeBlock :: ScopeBlock -> Ass [Instr]
genScopeBlock = undefined

-- genFuncCode :: Ass (Function -> [Instr])
-- genFuncCode state (Function _ fid _ sb)  = 
--   concat 
--   [ [Define (getVal fid)]
--   , genCode' state sb, [Section "ltorg"]
--   ]

writeCode :: FilePath -> [Instr] -> IO ()
writeCode f = (writeFile f) . unlines . (map printARM)
