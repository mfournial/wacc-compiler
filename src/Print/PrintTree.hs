module Bnfc.PrintTree where

import Bnfc.AbsWacc
import System.IO

-- System.IO helper methods
--
-- temp file to write to
_tmp :: FilePath
_tmp = "./PrintedTree.ast"

write :: String -> IO ()
write = appendFile _tmp

writeln :: String -> IO ()
writeln s = appendFile _tmp (s++"\n")

-- printing wacc tree from file
-- currently prints line numbers
printWaccTree :: (Tree a) => a -> IO ()
printWaccTree p = do
  putStrLn ("(Contents of file:" ++ _tmp ++ ")\n")
  writeFile _tmp "" -- clear/create file
  printTreeHelper 2 p 0 True -- write tree to file
  handle <- openFile _tmp ReadMode -- open file
  contents <- hGetContents handle
  -- line numbers
  let c = zipWith (\n l -> show n ++ l) [0..] $ lines contents
  putStrLn . unlines $ c -- printfile to stdout
  hClose handle -- close file
  

class Tree a where
  printTree :: a -> IO ()
  -- printTree is currently only defined for WaccTree type
  printTree _ = return ()

  -- prints tree element with correct indentation
  printTreeHelper :: Int -> a -> Int -> Bool -> IO ()
  printTreeHelper n t fp b = do
    write $ replicate (2 * n) ' '
    write "- "
    printTree t

-- prints terminal tree element with correct indentation
appendLn :: Int -> String -> Int -> Bool -> IO ()
appendLn n s  fp b
 | fp == 1 = do
              if(s /= "TYPE") then do
                write s
              else
                return ()
 | fp == 2  = do
              if (b) then do
                 write $ replicate (2 * n) ' '
                 write "- "
                 write s
              else 
                 write s
 | fp == 3 = do
             if (s /= "TYPE") then do
                if (b) then do
                 write s
                else do
                 write $ replicate (2 * n) ' '
                 write "- "
                 write s
             else
               return ()
 | fp == 4 = do 
             if (s /= "TYPE") then do
               write s
               write ", "
             else
               return ()
 | otherwise  = do
                 if(b) then do
                   write $ replicate (2 * n) ' '
                   write "- "
                   writeln s
                 else do 
                   write $ replicate (2 * n) ' '
                   write "- "
                   write s

-- prints list of tree elements with correct indentation
printTrees :: (Tree a) => Int -> [a] -> Int -> Bool ->  IO ()
printTrees n s fp b =  let flip3 f a b c = f c a b in  mapM_ ((.) flip3 printTreeHelper n fp b) s

printParam :: Int -> [ Parameter] -> Int -> Bool -> IO ()
printParam n [] fp b  = return ()
printParam n ((Param t i) : []) fp b = do 
                                        printTreeHelper n t 1 b
                                        write " "
                                        printTreeHelper n i 1 b
printParam n ((Param t i):ss) fp b = do 
                            printTreeHelper n t 1 b
                            write " "
                            printTreeHelper n i 4 b
                            printParam n ss fp b


instance Tree Position where
instance Tree EndT where
instance Tree BeginT where
instance Tree SkipT where
instance Tree ReadT where
instance Tree PrintT where
instance Tree PrintLnT where
instance Tree FreeT where
instance Tree ExitT where
instance Tree IntDigit where
  printTreeHelper n (IntDigit (_, s)) fp b  = writeln s
instance Tree PlusToken where
instance Tree MinusToken where
instance Tree BoolLiteral where
  printTreeHelper n (TrueToken _) fp  b = appendLn n "True" fp b
  printTreeHelper n (FalseToken _) fp b = appendLn n "False" fp b
instance Tree IntT where
instance Tree BoolT where
instance Tree CharT where
instance Tree StringT where
instance Tree TimesT where
instance Tree DivideT where
instance Tree ModuloT where
instance Tree GreaterT where
instance Tree LessT where
instance Tree GreaterEqT where
instance Tree LessEqT where
instance Tree EqT where
instance Tree NotEqT where
instance Tree AndT where
instance Tree OrT where
instance Tree LParenT where
instance Tree RParenT where
instance Tree LBracketT where
instance Tree RBracketT where
instance Tree IsT where
instance Tree WhileT where
instance Tree DoT where
instance Tree DoneT where
instance Tree IfT where
instance Tree FiT where
instance Tree ThenT where
instance Tree ElseT where
instance Tree PairT where
instance Tree NewpairT where
instance Tree CallT where
instance Tree FstT where
instance Tree SndT where
instance Tree EqualT where
instance Tree LenT where
instance Tree OrdT where
instance Tree ChrT where
instance Tree ReturnT where
instance Tree NotT where
instance Tree PairLiteral where
  printTreeHelper n (PairLiteral (_, s)) fp b  = appendLn n s fp b
instance Tree CharLiteral where
  printTreeHelper n (CharLiteral (_, s)) fp b = appendLn n s fp b
instance Tree StringLiteral where
  printTreeHelper n (StringLiteral (_, s)) fp b = appendLn n s fp b
instance Tree Identifier where
  printTreeHelper n (Identifier (_, s)) fp b = appendLn n s fp b

-- start of main print function
instance Tree WaccTree where
  printTree (WaccTree p)
    = printWaccTree p

-- nest 1
instance Tree Program where
  printTreeHelper n (Program _ fs ss _) fp b  = do
    appendLn n "Program" fp b
    printTrees (n+1) fs 1 b
    appendLn (n+1) "int main()" fp b
    printTrees (n+2) ss fp b

-- nest 2 (etc)
instance Tree Function where
  printTreeHelper n (Function t i _ ps _ _ ss _) fp b = do
    write $ replicate (2 * n) ' '
    write "- " 
    printTreeHelper (n+1) t 1 b
    write " "
    printTreeHelper (n+1) i 1 True
    write "("
    printParam (n+1) ps 1 True
    writeln ")"
    printTrees (n+1) ss 0 True
    
instance Tree Parameter where
  printTreeHelper n (Param t i) fp b = do
    printTreeHelper (n+1) t fp b 
    printTreeHelper (n+1) i fp b

instance Tree Statement where
  printTreeHelper n (StatSkip s) fp b  = appendLn n "SKIP" fp b
  printTreeHelper n (StatDecAss t i _ ar) fp b = do
    appendLn n "DECLARE" fp b
    printTreeHelper (n+1) t fp False
    writeln ""
    printTreeHelper (n+1) (AssignToIdent i) fp b
    printTreeHelper (n+1) ar fp b
  printTreeHelper n (StatAss al _ ar) fp b = do
    appendLn n "ASSIGNMENT" fp b
    printTreeHelper (n+1) al fp b
    printTreeHelper (n+1) ar fp b
  printTreeHelper n (StatRead _ al) fp b  = do
    appendLn n "READ" fp b
    printTreeHelper (n+1) al fp b
  printTreeHelper n (StatFree _ e) fp b  = do
    appendLn n "FREE" fp b
    printTreeHelper n e fp b
  printTreeHelper n (StatReturn _ e) fp b = do
    appendLn n "RETURN" fp b
    printTreeHelper n e fp b
  printTreeHelper n (StatExit _ e) fp b = do
    appendLn n "EXIT" fp b
    printTreeHelper n e fp b
  printTreeHelper n (StatPrint _ e) fp b = do
    appendLn n "PRINT" fp b
    printTreeHelper n e fp b
  printTreeHelper n (StatPrintLn _ e) fp b = do
    appendLn n "PRINTLN" fp b
    printTreeHelper n e fp b
  printTreeHelper n (StatIf _ e _ ssi _ sse _) fp b = do
    appendLn n "IF" fp b
    appendLn (n+1) "CONDITION" fp b
    printTreeHelper (n+1) e fp b
    appendLn (n+1) "THEN" fp b
    printTrees (n+2) ssi fp b
    appendLn (n+1) "ELSE" fp b
    printTrees (n+2) sse fp b
  printTreeHelper n (StatWhile _ e _ ss _) fp b = do
    appendLn n "LOOP" fp b
    appendLn (n+2) "CONDITION" fp b
    printTreeHelper (n+2) e fp b
    appendLn (n+2) "DO" fp b
    printTrees (n+3) ss fp b
  printTreeHelper n (StatScope _ ss _) fp b  = printTrees (n+1) ss fp b

instance Tree AssignLhs where
  printTreeHelper n (AssignToIdent i) fp b = do
    appendLn n "LHS" fp b
    printTreeHelper (n+1) i fp b
  printTreeHelper n (AssignToArrayElem ae) fp b = do
    appendLn n "LHS" fp b
    printTreeHelper (n+1) ae fp b
  printTreeHelper n (AssignToPair pe) fp b = do
    appendLn n "LHS" fp b
    printTreeHelper (n+1) pe fp b

instance Tree AssignRhs where
  printTreeHelper n (AssignExp e) fp b = do
    appendLn n "RHS" fp b
    printTreeHelper n e fp b
  printTreeHelper n (AssignArrayLit al) fp b = do
    appendLn n "RHS" fp b
    printTreeHelper (n+1) al fp b
  printTreeHelper n (AssignPair _ _ el er _) fp b = do
    appendLn n "RHS" fp b
    appendLn (n+1) "NEW_PAIR" fp b
    appendLn (n+2) "FST" fp b
    printTreeHelper (n+2) el fp b
    appendLn (n+2) "SND" fp b
    printTreeHelper (n+2) er fp b
  printTreeHelper n (AssignPairElem pe) fp b = do
    appendLn n "RHS" fp b
    printTreeHelper (n+1) pe fp b
  printTreeHelper n (AssignFunctionCall _ i _ as _) fp b = do
    appendLn n "RHS" fp b
    printTreeHelper (n+1) i fp b
    printTrees n as fp b

instance Tree ArgumentList where
  printTreeHelper n (ArgumentList e) fp b = do
    printTreeHelper n e fp b

instance Tree PairElem where
  printTreeHelper n (PairFst _ e) fp b = do
    appendLn n "FST" fp b
    printTreeHelper n e fp b
  printTreeHelper n (PairSnd _ e) fp b = do
    appendLn n "SND" fp b
    printTreeHelper n e fp b
  
instance Tree Type where
  printTreeHelper n (BaseType b) fp bo = do
    appendLn n "TYPE" fp True
    printTreeHelper (n+1) b fp bo
  printTreeHelper n (ArrayType al) fp b = do
    appendLn n "TYPE" fp True
    printTreeHelper n al 3 b
  printTreeHelper n (PairType _ _ pl pr _) fp b = do
    appendLn n "TYPE" fp True
    if (fp == 1) then do
      printTreeHelper (n+1) pl fp True
      printTreeHelper (n+1) pr fp False
    else do
      printTreeHelper (n+1) pl 2 True
      printTreeHelper (n+1) pr 2 False

instance Tree BaseType where
  printTreeHelper n (IntType _) fp b = do
    appendLn n "int" fp b
  printTreeHelper n (BoolType _) fp b = do
    appendLn n "bool" fp b
  printTreeHelper n (CharType _) fp b = do
    appendLn n "char" fp b
  printTreeHelper n (StringType _) fp b  = do
    appendLn n "string" fp b

instance Tree ArrayDeclarationLiteral where
  printTreeHelper n (ArrayDeclarationLiteral t _ _) fp b  = do
    printTreeHelper (n) t fp b
    write "[]"

instance Tree ArrayElem where
  printTreeHelper n (ArrayElem i as) fp b  = do
    printTreeHelper (n+1) i fp b
    printTrees (n+1) as fp b

instance Tree ArrayAccess where
  printTreeHelper n (ArrayAccess _ e _) fp b = do
    appendLn (n+1) "[]" fp b
    printTreeHelper (n+1) e fp b

instance Tree ArrayLiteral where
  printTreeHelper n (ArrayLiteral _ as _) fp b = do
    appendLn n "ARRAY LITERAL" fp b
    printTrees (n) as fp b

instance Tree ArrayLiteralElem where
  printTreeHelper n (ArrayLiteralElem e) fp b = do
    printTreeHelper n e fp b

instance Tree PairElemType where
  printTreeHelper n (PairElemTypeBase b) fp bo = do
    printTreeHelper (n+1) b fp bo
  printTreeHelper n (PairElemTypeArray al) fp b = do
    printTreeHelper (n+1) al fp b
  printTreeHelper n (PairElemTypePair _) fp b = do
    appendLn n "" fp b
  
instance Tree Expression where
  printTreeHelper n (IntExp e) fp b = do
    printTreeHelper (n+1) e fp b
  printTreeHelper n (BoolExp e) fp b = do
    printTreeHelper (n+1) e fp b
  printTreeHelper n (CharExpr e) fp b = do
    printTreeHelper (n+1) e fp b
  printTreeHelper n (StringExpr e) fp b  = do
    printTreeHelper (n+1) e fp b
  printTreeHelper n (PairExpr e) fp b  = do
    printTreeHelper (n+1) e fp b
  printTreeHelper n (IdentExpr e) fp b = do
    printTreeHelper (n+1) e fp b
  printTreeHelper n (ArrayExpr e) fp b = do
    printTreeHelper (n+1) e fp b
  printTreeHelper n (UExpr u e) fp b = do
    printTreeHelper (n+1) u fp b
    printTreeHelper (n+1) e fp b
  printTreeHelper n (BExp el bo er) fp b = do
    printTreeHelper (n+1) bo fp b
    printTreeHelper (n+1) el fp b
    printTreeHelper (n+1) er fp b
  printTreeHelper n (BracketExp _ e _) fp b = do
    printTreeHelper (n+1) e fp b

instance Tree UnaryOperator where
  printTreeHelper n (UBang _) fp b    = appendLn n "!" fp b
  printTreeHelper n (UMinus _) fp b   = appendLn n "-" fp b
  printTreeHelper n (ULength _) fp b  = appendLn n "len" fp b
  printTreeHelper n (UOrd _) fp b     = appendLn n "ord" fp b
  printTreeHelper n (UChr _) fp b     = appendLn n "chr" fp b

instance Tree BinaryOperator where
  printTreeHelper n (BTimes _) fp b         = appendLn n "*" fp b
  printTreeHelper n (BDivide _) fp b        = appendLn n "/" fp b
  printTreeHelper n (BModulus _) fp b       = appendLn n "mod" fp b
  printTreeHelper n (BPlus _)  fp b         = appendLn n "+" fp b
  printTreeHelper n (BMinus _)  fp b        = appendLn n "-" fp b
  printTreeHelper n (BGreater _) fp b       = appendLn n ">" fp b
  printTreeHelper n (BLess _)    fp b       = appendLn n "<" fp b
  printTreeHelper n (BGreaterEqual _) fp b  = appendLn n ">=" fp b
  printTreeHelper n (BLessEqual _) fp b     = appendLn n "<=" fp b
  printTreeHelper n (BEqual _)     fp b     = appendLn n "==" fp b
  printTreeHelper n (BNotEqual _)  fp b     = appendLn n "/=" fp b
  printTreeHelper n (BAnd _)       fp b     = appendLn n "&&" fp b
  printTreeHelper n (BOr _)        fp b     = appendLn n "||" fp b

instance Tree IntLiteral where
  printTreeHelper n (IntPlus _ i)  fp b = do 
         write $ replicate (2 * n) ' '
         write "- "
         write "+" >> printTreeHelper (n+1) i fp b
  printTreeHelper n (IntMinus _ i) fp b = do 
         write $ replicate (2 * n) ' ' 
         write "- " 
         write "-" >> printTreeHelper (n+1) i fp b
  printTreeHelper n (IntLiteral i) fp b = do 
         write $ replicate (2 * n) ' '
         write "- " >> printTreeHelper (n+1) i fp b