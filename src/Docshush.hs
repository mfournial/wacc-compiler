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
--  prop> before . after $ x == x
--  
before :: Int -> Int
before = (1+)

after :: Int -> Int
after = (1-)
