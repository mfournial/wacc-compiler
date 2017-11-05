-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Bnfc.ParWacc where
import Bnfc.AbsWacc as AbsWacc
import Bnfc.LexWacc as LexWacc

import Data.Waskell.Error
}

%name pExp WaccTree
%name pProgram Program
%name pFunction Function
%name pListFunction ListFunction
%name pParameter Parameter
%name pListParameter ListParameter
%name pStatement Statement
%name pListStatement ListStatement
%name pAssignLhs AssignLhs
%name pAssignRhs AssignRhs
%name pArgumentList ArgumentList
%name pListArgumentList ListArgumentList
%name pPairElem PairElem
%name pType Type
%name pBaseType BaseType
%name pArrayDeclarationLiteral ArrayDeclarationLiteral
%name pArrayElem ArrayElem
%name pArrayAccess ArrayAccess
%name pListArrayAccess ListArrayAccess
%name pArrayLiteral ArrayLiteral
%name pArrayLiteralElem ArrayLiteralElem
%name pListArrayLiteralElem ListArrayLiteralElem
%name pPairElemType PairElemType
%name pExpression Expression
%name pUnaryOperator UnaryOperator
%monad { ErrorList } { (>>=) } { return }
%tokentype { Token }
%token
  ',' {PT _ T_CoT}
  ';' { PT _ T_SepT }
  'null' { PT _ T_NullT }
  'end' { PT _ T_EndT }

'begin' { PT _ (T_BeginT) }
'skip' { PT _ (T_SkipT) }
'read' { PT _ (T_ReadT) }
'print' { PT _ (T_PrintT) }
'printLn' { PT _ (T_PrintLnT) }
'free' { PT _ (T_FreeT) }
'exit' { PT _ (T_ExitT) }
L_IntDigit { PT _ (T_IntDigit _) }
L_PlusToken { PT _ (T_PlusToken) }
L_MinusToken { PT _ (T_MinusToken) }
'true' { PT _ T_TrueToken }
'false' { PT _ T_FalseToken }
'int' { PT _ (T_IntT) }
'bool' { PT _ (T_BoolT) }
'char' { PT _ (T_CharT) }
'string' { PT _ (T_StringT) }
'timesT { PT _ (T_TimesT) }
'divideT { PT _ (T_DivideT) }
'moduloT { PT _ (T_ModuloT) }
'greaterT { PT _ (T_GreaterT) }
'lessT { PT _ (T_LessT) }
'greaterEqT { PT _ (T_GreaterEqT) }
'lessEqT { PT _ (T_LessEqT) }
'eqT { PT _ (T_EqT) }
'notEqT { PT _ (T_NotEqT) }
'andT { PT _ (T_AndT) }
'orT { PT _ (T_OrT) }
'(' { PT _ (T_LParenT) }
')' { PT _ (T_RParenT) }
'[' { PT _ (T_LBracketT) }
']' { PT _ (T_RBracketT) }
'is' { PT _ (T_IsT) }
'while' { PT _ (T_WhileT) }
'do' { PT _ (T_DoT) }
'done' { PT _ (T_DoneT) }
'if' { PT _ (T_IfT) }
'fi' { PT _ (T_FiT) }
'then' { PT _ (T_ThenT) }
'else' { PT _ (T_ElseT) }
'pair' { PT _ (T_PairT) }
'newpair' { PT _ (T_NewpairT) }
'call' { PT _ (T_CallT) }
'fst' { PT _ (T_FstT) }
'snd' { PT _ (T_SndT) }
L_EqualT { PT _ (T_EqualT) }
'len' { PT _ (T_LenT) }
'ord' { PT _ (T_OrdT) }
'chr' { PT _ (T_ChrT) }
'return' { PT _ (T_ReturnT) }
L_notT { PT _ (T_NotT) }
L_CharLiteral { PT _ (T_CharLiteral _) }
L_StringLiteral { PT _ (T_StringLiteral _) }
L_Identifier { PT _ (T_Identifier _) }

%left L_PlusToken L_MinusToken
%left L_TimesT L_DivideT

%%

BeginT    :: { BeginT} : L_BeginT { BeginT (mkPosToken $1)}
SkipT    :: { SkipT} : L_SkipT { SkipT (mkPosToken $1)}
ReadT    :: { ReadT} : L_ReadT { ReadT (mkPosToken $1)}
PrintT    :: { PrintT} : L_PrintT { PrintT (mkPosToken $1)}
PrintLnT    :: { PrintLnT} : L_PrintLnT { PrintLnT (mkPosToken $1)}
FreeT    :: { FreeT} : L_FreeT { FreeT (mkPosToken $1)}
ExitT    :: { ExitT} : L_ExitT { ExitT (mkPosToken $1)}
IntDigit    :: { (String, Position) } : L_IntDigit { mkPosStrToken $1 }
PlusToken    :: { PlusToken} : L_PlusToken { PlusToken (mkPosToken $1)}
MinusToken    :: { MinusToken} : L_MinusToken { MinusToken (mkPosToken $1)}
TrueToken :: { Position } : L_TrueToken { (mkPosToken $1)}
FalseToken :: { Position } : L_FalseToken { (mkPosToken $1)}
IntT    :: { IntT} : L_IntT { IntT (mkPosToken $1)}
BoolT    :: { BoolT} : L_BoolT { BoolT (mkPosToken $1)}
CharT    :: { CharT} : L_CharT { CharT (mkPosToken $1)}
StringT    :: { StringT} : L_StringT { StringT (mkPosToken $1)}
TimesT    :: { TimesT} : L_TimesT { TimesT (mkPosToken $1)}
DivideT    :: { DivideT} : L_DivideT { DivideT (mkPosToken $1)}
ModuloT    :: { ModuloT} : L_ModuloT { ModuloT (mkPosToken $1)}
GreaterT    :: { GreaterT} : L_GreaterT { GreaterT (mkPosToken $1)}
LessT    :: { LessT} : L_LessT { LessT (mkPosToken $1)}
GreaterEqT    :: { GreaterEqT} : L_GreaterEqT { GreaterEqT (mkPosToken $1)}
LessEqT    :: { LessEqT} : L_LessEqT { LessEqT (mkPosToken $1)}
EqT    :: { EqT} : L_EqT { EqT (mkPosToken $1)}
NotEqT    :: { NotEqT} : L_NotEqT { NotEqT (mkPosToken $1)}
AndT    :: { AndT} : L_AndT { AndT (mkPosToken $1)}
OrT    :: { OrT} : L_OrT { OrT (mkPosToken $1)}
LParenT    :: { LParenT} : L_LParenT { LParenT (mkPosToken $1)}
RParenT    :: { RParenT} : L_RParenT { RParenT (mkPosToken $1)}
LBracketT    :: { LBracketT} : L_LBracketT { LBracketT (mkPosToken $1)}
RBracketT    :: { RBracketT} : L_RBracketT { RBracketT (mkPosToken $1)}
IsT    :: { IsT} : L_IsT { IsT (mkPosToken $1)}
WhileT    :: { WhileT} : L_WhileT { WhileT (mkPosToken $1)}
DoT    :: { DoT} : L_DoT { DoT (mkPosToken $1)}
DoneT    :: { DoneT} : L_DoneT { DoneT (mkPosToken $1)}
IfT    :: { IfT} : L_IfT { IfT (mkPosToken $1)}
FiT    :: { FiT} : L_FiT { FiT (mkPosToken $1)}
ThenT    :: { ThenT} : L_ThenT { ThenT (mkPosToken $1)}
ElseT    :: { ElseT} : L_ElseT { ElseT (mkPosToken $1)}
PairT    :: { PairT} : L_PairT { PairT (mkPosToken $1)}
NewpairT    :: { NewpairT} : L_NewpairT { NewpairT (mkPosToken $1)}
CallT    :: { CallT} : L_CallT { CallT (mkPosToken $1)}
FstT    :: { FstT} : L_FstT { FstT (mkPosToken $1)}
SndT    :: { SndT} : L_SndT { SndT (mkPosToken $1)}
EqualT    :: { EqualT} : L_EqualT { EqualT (mkPosToken $1)}
LenT    :: { LenT} : L_LenT { LenT (mkPosToken $1)}
OrdT    :: { OrdT} : L_OrdT { OrdT (mkPosToken $1)}
ChrT    :: { ChrT} : L_ChrT { ChrT (mkPosToken $1)}
ReturnT    :: { ReturnT} : L_ReturnT { ReturnT (mkPosToken $1)}
NotT    :: { NotT} : L_NotT { NotT (mkPosToken $1)}
CharLiteral    :: { CharLiteral} : L_CharLiteral { CharLiteral (mkPosStrToken $1)}
StringLiteral    :: { StringLiteral} : L_StringLiteral { StringLiteral (mkPosStrToken $1)}
Identifier    :: { Identifier} : L_Identifier { Identifier (mkPosStrToken $1)}

WaccTree :: { WaccTree }
WaccTree : Program { AbsWacc.WaccTree $1 }
Program :: { Program }
Program : BeginT ListFunction ListStatement EndT { AbsWacc.Program $1 (reverse $2) $3 $4 }
Function :: { Function }
Function : Type Identifier LParenT ListParameter RParenT IsT FunListStatement EndT { AbsWacc.Function $1 $2 $3 $4 $5 $6 $7 $8 }
FunListStatement :: { [Statement] }
FunListStatement : EndFunListStatement { (:[]) $1 }
                 | LimitedStatement ';' FunListStatement { (:) $1 $3 }
EndFunListStatement :: { Statement }
EndFunListStatement : ReturnT Expression { AbsWacc.StatReturn $1 $2 }
                    | IfT Expression ThenT FunListStatement ElseT FunListStatement FiT { AbsWacc.StatIf $1 $2 $3 $4 $5 $6 $7 }
                    | WhileT Expression DoT FunListStatement DoneT { AbsWacc.StatWhile $1 $2 $3 $4 $5 }
                    | BeginT FunListStatement EndT { AbsWacc.StatScope $1 $2 $3 }
                    | ExitT Expression { AbsWacc.StatExit $1 $2 }
LimitedStatement :: { Statement }
LimitedStatement : SkipT { AbsWacc.StatSkip $1 }
                 | Type Identifier EqualT AssignRhs { AbsWacc.StatDecAss $1 $2 $3 $4 }
                 | AssignLhs EqualT AssignRhs { AbsWacc.StatAss $1 $2 $3 }
                 | ReadT AssignLhs { AbsWacc.StatRead $1 $2 }
                 | FreeT Expression { AbsWacc.StatFree $1 $2 }
                 | PrintT Expression { AbsWacc.StatPrint $1 $2 }
                 | PrintLnT Expression { AbsWacc.StatPrintLn $1 $2 }
                 | IfT Expression ThenT FunListStatement ElseT LimitedListStatement FiT { AbsWacc.StatIf $1 $2 $3 $4 $5 $6 $7 }
                 | IfT Expression ThenT LimitedListStatement ElseT FunListStatement FiT { AbsWacc.StatIf $1 $2 $3 $4 $5 $6 $7 }
                 | IfT Expression ThenT LimitedListStatement ElseT LimitedListStatement FiT { AbsWacc.StatIf $1 $2 $3 $4 $5 $6 $7 }
                 | WhileT Expression DoT LimitedListStatement DoneT { AbsWacc.StatWhile $1 $2 $3 $4 $5 }
                 | BeginT LimitedListStatement EndT { AbsWacc.StatScope $1 $2 $3 }
LimitedListStatement :: { [Statement] }
LimitedListStatement : LimitedStatement { (:[]) $1 }
                     | LimitedStatement ';' LimitedListStatement {(:) $1 $3}
ListFunction :: { [Function] }
ListFunction : {- empty -} { [] }
             | ListFunction Function { flip (:) $1 $2 }
Parameter :: { Parameter }
Parameter : Type Identifier { AbsWacc.Param $1 $2 }
ListParameter :: { [Parameter] }
ListParameter : {- empty -} { [] }
              | NonEmptyListParameter { $1 }
NonEmptyListParameter :: { [Parameter] }
NonEmptyListParameter : Parameter { (:[]) $1 }
                      | Parameter ',' NonEmptyListParameter { (:) $1 $3 }
Statement :: { Statement }
Statement : SkipT { AbsWacc.StatSkip $1 }
          | Type Identifier EqualT AssignRhs { AbsWacc.StatDecAss $1 $2 $3 $4 }
          | AssignLhs EqualT AssignRhs { AbsWacc.StatAss $1 $2 $3 }
          | ReadT AssignLhs { AbsWacc.StatRead $1 $2 }
          | FreeT Expression { AbsWacc.StatFree $1 $2 }
          | ReturnT Expression { AbsWacc.StatReturn $1 $2 }
          | ExitT Expression { AbsWacc.StatExit $1 $2 }
          | PrintT Expression { AbsWacc.StatPrint $1 $2 }
          | PrintLnT Expression { AbsWacc.StatPrintLn $1 $2 }
          | IfT Expression ThenT ListStatement ElseT ListStatement FiT { AbsWacc.StatIf $1 $2 $3 $4 $5 $6 $7 }
          | WhileT Expression DoT ListStatement DoneT { AbsWacc.StatWhile $1 $2 $3 $4 $5 }
          | BeginT ListStatement EndT { AbsWacc.StatScope $1 $2 $3 }
ListStatement :: { [Statement] }
ListStatement : Statement { (:[]) $1 }
              | Statement ';' ListStatement { (:) $1 $3 }
AssignLhs :: { AssignLhs }
AssignLhs : Identifier { AbsWacc.AssignToIdent $1 }
          | ArrayElem { AbsWacc.AssignToArrayElem $1 }
          | PairElem { AbsWacc.AssignToPair $1 }
AssignRhs :: { AssignRhs }
AssignRhs : Expression { AbsWacc.AssignExp $1 }
          | ArrayLiteral { AbsWacc.AssignArrayLit $1 }
          | NewpairT LParenT Expression ',' Expression RParenT { AbsWacc.AssignPair $1 $2 $3 $5 $6 }
          | PairElem { AbsWacc.AssignPairElem $1 }
          | CallT Identifier LParenT ListArgumentList RParenT { AbsWacc.AssignFunctionCall $1 $2 $3 $4 $5 }
ArgumentList :: { ArgumentList }
ArgumentList : Expression { AbsWacc.ArgumentList $1 }
ListArgumentList :: { [ArgumentList] }
ListArgumentList : {- empty -} { [] }
                 | ArgumentList { (:[]) $1 }
                 | ArgumentList ',' ListArgumentList { (:) $1 $3 }
PairElem :: { PairElem }
PairElem : FstT Expression { AbsWacc.PairFst $1 $2 }
         | SndT Expression { AbsWacc.PairSnd $1 $2 }
Type :: { Type }
Type : BaseType { AbsWacc.BaseType $1 }
     | ArrayDeclarationLiteral { AbsWacc.ArrayType $1 }
     | PairT LParenT PairElemType ',' PairElemType RParenT { AbsWacc.PairType $1 $2 $3 $5 $6 }
BaseType :: { BaseType }
BaseType : 'int' { AbsWacc.IntType }
         | BoolT { AbsWacc.BoolType $1 }
         | CharT { AbsWacc.CharType $1 }
         | StringT { AbsWacc.StringType $1 }
ArrayDeclarationLiteral :: { ArrayDeclarationLiteral }
ArrayDeclarationLiteral : Type LBracketT RBracketT { AbsWacc.ArrayDeclarationLiteral $1 $2 $3 }
ArrayElem :: { ArrayElem }
ArrayElem : Identifier ListArrayAccess { AbsWacc.ArrayElem $1 $2 }
ArrayAccess :: { ArrayAccess }
ArrayAccess : LBracketT Expression RBracketT { AbsWacc.ArrayAccess $1 $2 $3 }
ListArrayAccess :: { [ArrayAccess] }
ListArrayAccess : ArrayAccess { (:[]) $1 }
                | ArrayAccess ListArrayAccess { (:) $1 $2 }
ArrayLiteral :: { ArrayLiteral }
ArrayLiteral : LBracketT ListArrayLiteralElem RBracketT { AbsWacc.ArrayLiteral $1 $2 $3 }
ArrayLiteralElem :: { ArrayLiteralElem }
ArrayLiteralElem : Expression { AbsWacc.ArrayLiteralElem $1 }
ListArrayLiteralElem :: { [ArrayLiteralElem] }
ListArrayLiteralElem : {- empty -} { [] }
                     | ArrayLiteralElem { (:[]) $1 }
                     | ArrayLiteralElem ',' ListArrayLiteralElem { (:) $1 $3 }
PairElemType :: { PairElemType }
PairElemType : BaseType { AbsWacc.PairElemTypeBase $1 }
             | ArrayDeclarationLiteral { AbsWacc.PairElemTypeArray $1 }
             | PairT { AbsWacc.PairElemTypePair $1 }
Expression :: { Expression }
Expression : ExpressionBool { $1 }
ExpressionBool :: { Expression }
ExpressionBool : ExpressionBool OrT ExpressionBool1 { AbsWacc.BExp $1 (AbsWacc.BOr $2) $3 }
               | ExpressionBool1 { $1 } 
ExpressionBool1 :: { Expression }
ExpressionBool1 : ExpressionBool1 AndT ExpressionBool2 { AbsWacc.BExp $1 (AbsWacc.BAnd $2) $3 }
                | ExpressionBool2 { $1 }
ExpressionBool2 :: { Expression }
ExpressionBool2 : ExpressionBool2 EqT ExpressionBool3 { AbsWacc.BExp $1 (AbsWacc.BEqual $2) $3 }
                | ExpressionBool2 NotEqT ExpressionBool3 { AbsWacc.BExp $1 ( AbsWacc.BNotEqual $2 ) $3 }
                | ExpressionBool3 { $1 }
ExpressionBool3 :: { Expression }
ExpressionBool3 : ExpressionBool3 GreaterT ExpressionValue { AbsWacc.BExp $1 (AbsWacc.BGreater $2) $3 }
                | ExpressionBool3 LessT ExpressionValue { AbsWacc.BExp $1 (AbsWacc.BLess $2) $3 }
                | ExpressionBool3 GreaterEqT ExpressionValue { AbsWacc.BExp $1 (AbsWacc.BGreaterEqual $2) $3 }
                | ExpressionBool3 LessEqT ExpressionValue { AbsWacc.BExp $1 (AbsWacc.BLessEqual $2) $3 }
                | ExpressionTerm { $1 }
ExpressionTerm :: { Expression }
ExpressionTerm : ExpressionTerm MinusToken ExpressionFactor { AbsWacc.BExp $1 (AbsWacc.BMinus $2) $3 }
               | ExpressionTerm PlusToken ExpressionFactor { AbsWacc.BExp $1 (AbsWacc.BPlus $2) $3 }
               | ExpressionFactor { $1 }
ExpressionFactor :: { Expression }
ExpressionFactor : ExpressionFactor TimesT ExpressionValue { AbsWacc.BExp $1 (AbsWacc.BTimes $2) $3 }
                 | ExpressionFactor DivideT ExpressionValue { AbsWacc.BExp $1 (AbsWacc.BDivide $2) $3 }
                 | ExpressionFactor ModuloT ExpressionValue { AbsWacc.BExp $1 (AbsWacc.BModulus $2) $3 }
                 | ExpressionValue { $1 }
ExpressionValue :: { Expression }
ExpressionValue : Value { $1 }
                | LParenT Expression RParenT { AbsWacc.BracketExp $1 $2 $3 }
Value :: { Expression }
Value : IntLiteral { AbsWacc.IntExp $1 }
      | TrueToken { AbsWacc.BoolExp (AbsWacc.TrueToken $1) }
      | FalseToken { AbsWacc.BoolExp (AbsWacc.FalseToken $1) }
      | CharLiteral { AbsWacc.CharExpr $1 }
      | StringLiteral { AbsWacc.StringExpr $1 }
      | 'null' { AbsWacc.PairExpr }
      | Identifier { AbsWacc.IdentExpr $1 }
      | ArrayElem { AbsWacc.ArrayExpr $1 }
      | UnaryOperator Expression { AbsWacc.UExpr $1 $2 }
UnaryOperator :: { UnaryOperator }
UnaryOperator : NotT { AbsWacc.UBang $1 }
              | MinusToken { AbsWacc.UMinus $1 }
              | LenT { AbsWacc.ULength $1 }
              | OrdT { AbsWacc.UOrd $1 }
              | ChrT { AbsWacc.UChr $1 }
IntLiteral :: { Int }
IntLiteral : PlusToken IntDigit { % if checkOverflow $2 
                                      then throwFlow $2 "Int Overflow in "
                                      else return (read' $2) 
                                }
           | MinusToken IntDigit { % if checkUnderflow $2 
                                      then throwFlow $2 "Int Underflow in "
                                      else return ( - read' $2)
                                 }
           | IntDigit { % if checkOverflow $1
                            then throwFlow $1 "Int Overflow in "
                            else return (read' $1) 
                      }
{

read' :: (String, Position) -> Int
read' = read . fst

throwFlow :: (String, Position)  -- ^ Token to throw
          -> String -- ^ Error message to display to user
          -> ErrorList Int -- ^ Returned error
throwFlow (s, p) msg = throwError 0 (ErrorData FatalLevel ParserStage p (msg ++ s) 100)

mkPosToken :: Token -> Position
mkPosToken t@(PT p _) = posLineCol p

mkPosStrToken :: Token -> (String, Position)
mkPosStrToken t@(PT p _) = (prToken t, posLineCol p)


happyError :: [Token] -> ErrorList a
happyError [] = die ParserStage (0, 0) "File ended unexpectedly" 100
happyError ts@((PT (Pn _ l c) _) : ts') = die ParserStage (l, c) str 100
  where
    str = case ts of
        [] -> []
        _ -> " error before " ++ unwords (map (id . prToken) (take 4 ts))

-- | Create digit safely create a digit checking for overflow
checkOverflow :: (String, Position) -- ^ IntDigit token to be checked for overflow
              -> Bool -- ^ Returns true if overflow is detected
checkOverflow (s, _) = (read s :: Integer) > upperBound

-- | Create digit safely create a digit checking for underflow
checkUnderflow :: (String, Position) -- ^ IntDigit token to be checked for underflow
               -> Bool -- ^ Returns truew if underflow is detected
checkUnderflow (s, _) = (read s :: Integer) > lowerBound

-- | Value of min/max supported integer
lowerBound :: Integer -- ^ @ 2^31 @ 32 bit signed int
lowerBound = 2147483648

upperBound :: Integer -- ^ @ 2^31 - 1 @ 32 bit signed int
upperBound = 2147483647

myLexer = tokens
}

