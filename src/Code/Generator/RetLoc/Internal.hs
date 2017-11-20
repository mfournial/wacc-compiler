module Code.Generator.RetLoc.Internal where

import Code.Instructions(Reg)

data RetLoc = PRL PureRetLoc | StackPtr Int
  deriving (Show)

data PureRetLoc = HeapAddr Int
                | StringLit String
                | RegLoc Reg
                | Register Reg
                | ImmChar Char
                | ImmInt Int
  deriving (Show)
