module Code.Generator (produceASM) where

import Code.Instructions

import Data.Waskell.ADT

data State = State {
  regs     :: [Reg],
  sp       :: Reg,
  link     :: Reg,
  pc       :: Reg,
  exitCode :: Int
}

produceASM :: FilePath -> WaccTree -> IO ()
produceASM f = (writeCode f) . (genCode (State [minBound..] StackPointer LinkRegister PC 0))

genCode :: State -> WaccTree -> [Instr]
genCode state (WaccTree (Program fs sb)) =
  concat
  [ [Section "text"]
  , concatMap ((genFuncCode state) . getVal) fs
  , [Global, PUSH AL [(link state)]]
  , genCode' state sb
  , [LDR AL W R0 (Const (exitCode state))]
  ]

genCode' :: State -> ScopeBlock -> [Instr]
genCode' _ _ = []

genFuncCode :: State -> Function -> [Instr]
genFuncCode state (Function _ fid _ sb)  = concat [[Define (getVal fid)], genCode' state sb, [Section "ltorg"]]

writeCode :: FilePath -> [Instr] -> IO ()
writeCode f = (writeFile f) . unlines . (map printARM)
