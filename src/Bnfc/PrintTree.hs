module PrintWacc where

import Bnfc.AbsWacc
import Bnfc.Tree

printTreeHelper :: (Tree a) => Int -> a -> IO ()
printTreeHelper n t = do
  putStr $ replicate (2 * n) ' '
  putStr "- "
  printTree t


-- printTrees []     = return ()
-- printTrees (t:ts) = printTree t >> printTrees ts
printTrees = mapM_ printTree

-- printParameters  

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
instance Tree PlusToken where
instance Tree MinusToken where
instance Tree BoolLiteral where
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
instance Tree CharLiteral where
instance Tree StringLiteral where
instance Tree Identifier where

-- start of function
instance Tree WaccTree where
  printTree (WaccTree p)
    = printTreeHelper 0 p 

-- nest 1
instance Tree Program where
  printTree (Program _ fs ss _) = do
    putStrLn "Program"
    printTrees fs
    printTrees ss

instance Tree Function where
  printTree (Function t i _ ps _ _ ss _) = do
  -- debug line
    putStrLn "function"
    printTree t
    printTree i
    printTrees ps
    printTrees ss
    
instance Tree Parameter where
  printTree (Param t i) = do
    printTree t
    printTree i

instance Tree Statement where
  printTree (StatSkip s) = putStrLn "SKIP"
  printTree (StatDecAss t i _ ar) = do
    putStrLn "DECLARE"
    printTree t
    printTree (AssignLhs i)
    printTree ar
  printTree (StatAss al _ ar) = do
    putStrLn "DECLARE"
    printTree al
    printTree ar
  printTree (StatRead _ al) = do
    putStrLn "READ"
    printTree al
  printTree (StatFree _ e) = do
    putStrLn "FREE"
    printTree e
  printTree (StatReturn _ e) = do
    putStrLn "RETURN"
    printTree e
  printTree (StatExit _ e) = do
    putStrLn "EXIT"
    printTree e
  printTree (StatPrint _ e) = do
    putStrLn "PRINT"
    printTree e
  printTree (StatPrintLn _ e) = do
    putStrLn "PRINTLN"
    printTree e
  printTree (StatIf _ e _ ssi _ sse _) = do
    putStrLn "IF"
    putStrLn "- CONDITION"
    printTree e
    putStrLn "- THEN"
    printTrees ssi
    putStrLn "- ELSE"
    printTrees sse
  printTree (StatWhile _ e _ ss _) = do
    putStrLn "LOOP"
    putStrLn "- CONDITION"
    printTree e
    putStrLn "- DO"
    printTrees ss
  printTree (StatScope _ ss _) = printTrees ss

instance Tree AssignLhs where
  printTree (AssignToIdent i) = do
    putStrLn "LHS"
    printTree i
  printTree (AssignPairElem ae) = do
    putStrLn "LHS"
    printTree ae
  printTree (AssignPair pe) = do
    putStrLn "LHS"
    printTree pe

instance Tree AssignRhs where
  printTree (AssignExp e) = do
    putStrLn "RHS"
    printTree e
  -- WATCH
  printTree (AssignArrayLit al) = do
    putStrLn "RHS"
    printTree al
  printTree (AssignPair _ _ el er _) = do
    putStrLn "RHS"
    putStrLn "- NEWPAIR"
    putStrLn "- FST"
    printTree el
    putStrLn "- SND"
    printTree er
  printTree (AssignPairElem pe) = do
    putStrLn "RHS"
    printTree pe
  printTree (AssignFunctionCall _ i _ as _) = do
    putStrLn "RHS"
    printTree i
    printTrees as

instance Tree ArgumentList where
  printTree (ArgumentList e) = do
    putStrLn "ARGUMENT LIST"
    printTree e

instance Tree PairElem where
  printTree (PairFst _ e) = do
    putStrLn "PAIRFST"
    printTree e
  printTree (PairSnd _ e) = do
    putStrLn "PAIRSND"
    printTree e
  
instance Tree Type where
  printTree (BaseType b) = do
    printTree b
  --do the rest of the patterns

instance Tree BaseType where
instance Tree ArrayDeclarationLiteral where
instance Tree ArrayElem where
instance Tree ArrayAccess where
instance Tree ArrayLiteral where
instance Tree ArrayLiteralElem where
instance Tree PairElemType where
instance Tree Expression where
instance Tree UnaryOperator where
instance Tree BinaryOperator where
instance Tree IntLiteral where
