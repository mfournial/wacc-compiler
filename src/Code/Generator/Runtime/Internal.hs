module Code.Generator.Runtime.Internal where

import Code.Instructions

data RuntimeComponent = RC RCID Instructions

instance Eq RuntimeComponent where
  (==) (RC rid _) (RC rid' _) = rid == rid'

data RCID = PrintStr
          | PrintInt
          | ReadInt
          | ReadChar
          | ThrowRuntimeErr
          | FreePair
          | PrintRef
          | ArrayCheck
          | Checkdbz
          deriving (Eq)