module Code.Generator.Runtime.Internal where

import Code.Instructions

data RuntimeComponent = RC RCID Instructions

instance Eq RuntimeComponent where
  (==) (RC rid _) (RC rid' _) = rid == rid'

data RCID = PrintStr | Exit --etc to add items as we generate new runtime component
  deriving (Eq)
