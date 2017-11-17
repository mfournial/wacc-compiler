module Code.Generator.Statement (
  generate
)
where


import Data.Sequence
import Prelude hiding(concat)

import Data.Waskell.ADT
import Code.Instructions()
import Code.Generator.State


generate :: Statement -> ARM Instructions
generate StatSkip = return empty
generate _ = error "How end up here ???"
