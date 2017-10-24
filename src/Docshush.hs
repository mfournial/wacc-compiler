-- This file exists because no one has written doctests yet

module DOcshush where

-- | test
--
--  >>> let one = 1 :: Int
--  >>> let two = succ one
--  >>> before two
--  1
--  >>> after one
--  2
--
--  prop> x == before (after x)
--  
before :: Int -> Int
before = pred

after :: Int -> Int
after = succ
