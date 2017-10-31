module Data.Waskell.Scope where
newtype NewScope = NewScope ()
  deriving (Eq, Ord, Show, Read)

emptyScope :: NewScope
emptyScope = NewScope ()
