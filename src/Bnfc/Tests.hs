{-|
This module tests the Lexer and Parser which are generated from the LBNF file wacc.cf
Module      : FrontEnd Tests
Description : Tests for lexer and parser
Maintainer  : ono16@ic.ac.uk
License     : ISC
Stability   : experimental
Portability : POSIX

This module should not conflict with libraries and is designed to be imported
qualified. E.g. @import qualified Tests as T@
-}

import LexWacc
import ParWacc
import ErrM
import AbsWacc

-- | Test Functions

-- | Test, Name, Expected, Got
data Test a 
  = Test Name a a
type Name = String

-- | We dont care whether the position is the same
--   We re-declare Position here with modified concept of equality

-- | evaluate test
checkTest :: (Eq a) => Test a -> Bool
checkTest (Test _ f s)
  = f == s

-- | number of failed tests from a list
checkTests :: (Eq a) => [Test a] -> Int
checkTests
  = foldr (\x acc -> (1 - fromEnum (checkTest x)) + acc) 0

runTest :: (Eq a, Show a) => Test a -> IO ()
runTest test@(Test n e g) = do
  let checked = checkTest test
  if checked
    then putStrLn ("Test \"" ++ n ++ "\" passed!\n")
    else putStrLn ("Test \"" ++ n ++ "\" failed:")
      >> putStrLn ("  Expected: " ++ show e)
      >> putStrLn ("  Got: " ++ show g)
      >> putStrLn ""

main :: IO ()
main = do
  putStrLn "Running tests.."
  putStr . show . checkTests $ tests
  putStrLn " test(s) failed!\n"
  mapM_ runTest tests

-- | Test data

createQuickLiteral :: Int -> Expression
createQuickLiteral int
  = IntExp (IntLiteral (IntDigit ((Pos dontCare), (show int))))
  where
    dontCare = (0,0)

createQuickBracketsExpression :: Expression -> Expression
createQuickBracketsExpression exp
  = BracketExp (LParenT (Pos dc)) exp (RParenT (Pos dc))
  where
    dc = dontCare
    dontCare = (0,0)

testQuickExpression :: String -> Expression
testQuickExpression e
  = strip . testParse $ ("begin x = " ++ e ++ " end")
  where
    strip (WaccTree 
            (Program _ _
              [StatAss (AssignToIdent _) _ (AssignExp expression)]
             _))
      = expression

testParse :: String -> WaccTree
testParse s
  = let ts = myLexer s in 
    case pExp ts of
      Bad s -> error "bad lex/parse"
      Ok tree -> tree
      
one = createQuickLiteral 1
two = createQuickLiteral 2
three = createQuickLiteral 3
plus = BPlus (PlusToken (Pos (0,0)))
times = BTimes (TimesT (Pos (0,0)))
brac = createQuickBracketsExpression
test = testQuickExpression

tests =
  [Test "A + B * C = A + (B * C)" 
      (BExp one plus (BExp two times three)) 
      (test "1 + 2 * 3")
  ,Test "A * B + C = (A * B) + C" 
      (BExp (BExp one times two) plus three)
      (test "1 * 2 + 3")
  ,Test "(A + B) * C with brackets" 
      (BExp (brac (BExp one plus two)) times three)
      (test "(1 + 2) * 3")
  ,Test "A + (B * C) with brackets" 
      (BExp one plus (brac (BExp two times three)))
      (test "1 + (2 * 3)")
  ,Test "(A * B) + C with brackets" 
      (BExp (brac (BExp one times two)) plus three)
      (test "(1 * 2) + 3")
  ,Test "A * (B + C) with brackets" 
      (BExp one times (brac (BExp two plus three)))
      (test "1 * (2 + 3)")
  ,Test "idk yet" 
      (one)
      (one)
  ]

