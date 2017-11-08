module Tree where

class Tree a where
  printTree :: a -> IO ()
  -- default definition
  printTree = return () --just for now
