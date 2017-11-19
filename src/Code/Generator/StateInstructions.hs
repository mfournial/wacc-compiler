module Code.Generator.StateInstructions where

import Code.Instructions
import Code.Generator.State
import Code.Generator.Runtime(RuntimeGenerator)
import Code.Generator.RetLoc
import Code.Generator.RetLoc.Internal

push :: [Reg] -> ARM (Instr, [RetLoc])
push rs = do
  locs <- mapM (\r -> incrementStack >> getSP >>= return . StackPtr) rs
  return (PUSH rs, locs)

pop :: [Reg] -> ARM Instr
pop = fmap POP . (mapM (\r -> decrementStack >> return r))

referencedPush :: [Reg] -> [String] -> ARM Instr
referencedPush = ((.).(.)) (fmap PUSH  . (mapM (\(r,n) -> pushVar n >> return r))) zip

branchRuntime :: RuntimeGenerator -> ARM Instr
branchRuntime rg = do
  (cpt, lab) <- rg
  addToRuntime cpt
  return (BL AL lab)
