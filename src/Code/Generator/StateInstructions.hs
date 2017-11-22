module Code.Generator.StateInstructions where

import Code.Instructions
import Code.Generator.State
import Code.Generator.ARM
import Data.Bifunctor(first)

push :: [Reg] -> ARM (Instr, [RetLoc])
push rs = do
  locs <- mapM (\r -> incrementStack >> getSP >>= return . StackPtr) rs
  return (PUSH rs, locs)

pop :: [Reg] -> ARM Instr
pop = fmap POP . (mapM (\r -> decrementStack >> return r))

referencedPush :: [Reg] -> [String] -> ARM (Instr, [RetLoc])
referencedPush = ((.).(.)) ((fmap (first PUSH . unzip)) . (mapM (\ (r,n) -> pushVar n >>= \k -> return (r,k)))) zip
