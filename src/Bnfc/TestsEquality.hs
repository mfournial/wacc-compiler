module TestsEquality where

import AbsWacc

class (Eq a) => TestEq a where
  (=#=) :: a -> a -> Bool
  infixr 4 =#=

  -- default to ordinary equality
  (=#=) = (==)

instance TestEq a => TestEq [a] where
  (a:as) =#= []     = False
  []     =#= (b:bs) = False
  []     =#= []     = True
  (a:as) =#= (b:bs)
    | a =#= b   = as =#= bs
    | otherwise = False

instance TestEq Position where
  _ =#= _ = True

instance TestEq EndT where
  _ =#= _ = True 

instance TestEq BeginT where
  _ =#= _ = True 

instance TestEq SkipT where
  _ =#= _ = True 

instance TestEq ReadT where
  _ =#= _ = True 

instance TestEq PrintT where
  _ =#= _ = True 

instance TestEq PrintLnT where
  _ =#= _ = True 

instance TestEq FreeT where
  _ =#= _ = True 

instance TestEq ExitT where
  _ =#= _ = True 

instance TestEq IntDigit where
  (IntDigit (_, s)) =#= (IntDigit (_, f)) = s == f

instance TestEq PlusToken where
  _ =#= _ = True 

instance TestEq MinusToken where
  _ =#= _ = True 

instance TestEq BoolLiteral where
  _ =#= _ = True 

instance TestEq IntT where
  _ =#= _ = True 

instance TestEq BoolT where
  _ =#= _ = True 

instance TestEq CharT where
  _ =#= _ = True 

instance TestEq StringT where
  _ =#= _ = True 

instance TestEq TimesT where
  _ =#= _ = True 

instance TestEq DivideT where
  _ =#= _ = True 

instance TestEq ModuloT where
  _ =#= _ = True 

instance TestEq GreaterT where
  _ =#= _ = True 

instance TestEq LessT where
  _ =#= _ = True 

instance TestEq GreaterEqT where
  _ =#= _ = True 

instance TestEq LessEqT where
  _ =#= _ = True 

instance TestEq EqT where
  _ =#= _ = True 

instance TestEq NotEqT where
  _ =#= _ = True 

instance TestEq AndT where
  _ =#= _ = True 

instance TestEq OrT where
  _ =#= _ = True 

instance TestEq LParenT where
  _ =#= _ = True 

instance TestEq RParenT where
  _ =#= _ = True 

instance TestEq LBracketT where
  _ =#= _ = True 

instance TestEq RBracketT where
  _ =#= _ = True 

instance TestEq IsT where
  _ =#= _ = True 

instance TestEq WhileT where
  _ =#= _ = True 

instance TestEq DoT where
  _ =#= _ = True 

instance TestEq DoneT where
  _ =#= _ = True 

instance TestEq IfT where
  _ =#= _ = True 

instance TestEq FiT where
  _ =#= _ = True 

instance TestEq ThenT where
  _ =#= _ = True 

instance TestEq ElseT where
  _ =#= _ = True 

instance TestEq PairT where
  _ =#= _ = True 

instance TestEq NewpairT where
  _ =#= _ = True 

instance TestEq CallT where
  _ =#= _ = True 

instance TestEq FstT where
  _ =#= _ = True 

instance TestEq SndT where
  _ =#= _ = True 

instance TestEq EqualT where
  _ =#= _ = True 

instance TestEq LenT where
  _ =#= _ = True 

instance TestEq OrdT where
  _ =#= _ = True 

instance TestEq ChrT where
  _ =#= _ = True 

instance TestEq ReturnT where
  _ =#= _ = True 

instance TestEq NotT where
  _ =#= _ = True 

instance TestEq PairLiteral where
  (PairLiteral (_,s)) =#= (PairLiteral (_,f)) = s == f

instance TestEq CharLiteral where
  (CharLiteral (_,s)) =#= (CharLiteral (_,f)) = s == f

instance TestEq StringLiteral where
  (StringLiteral (_,s)) =#= (StringLiteral (_,f)) = s == f

instance TestEq Identifier where
  (Identifier (_,s)) =#= (Identifier (_,f)) = s == f

instance TestEq WaccTree where
  WaccTree p =#= WaccTree f = p =#= f

instance TestEq Program where
  Program _ fs ss _
    =#= Program _ fs' ss' _
    = (fs =#= fs') && (ss =#= ss')

instance TestEq Function where
  Function t i _ ps _ _ ss _
    =#= Function t' i' _ ps' _ _ ss' _
    = (t =#= t') &&
      (i =#= i') &&
      (ps =#= ps') &&
      (ss =#= ss')
      
instance TestEq Parameter where
  Param t i
    =#= Param t' i'
    = (t =#= t') && (i =#= i')

instance TestEq Statement where
instance TestEq AssignLhs where
instance TestEq AssignRhs where
instance TestEq ArgumentList where
instance TestEq Type where
instance TestEq BaseType where
instance TestEq ArrayDeclarationLiteral where
instance TestEq ArrayElem where
instance TestEq ArrayAccess where
instance TestEq ArrayLiteral where
instance TestEq ArrayLiteralElem where
instance TestEq PairElemType where
instance TestEq Expression where
  -- UExpr 
  -- BExp 
  -- BracketExp
  
  -- IntExp a =#= IntExp b = a =#= b
  -- BoolExp a =#= BoolExp b = a =#= b
  -- CharExpr a =#= CharExpr b = a =#= b
  -- StringExpr a =#= StringExpr b = a =#= b
  -- PairExpr a =#= PairExpr b = a =#= b 
  -- IdentExpr a =#= IdentExpr b = a =#= b
  -- ArrayExpr a =#= ArrayExpr b = a =#= b

instance TestEq UnaryOperator where
instance TestEq BinaryOperator where
instance TestEq IntLiteral where
