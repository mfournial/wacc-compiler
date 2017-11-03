-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Bnfc.ParWacc where
import Bnfc.AbsWacc as AbsWacc
import Bnfc.LexWacc as LexWacc
import Bnfc.ErrM as ErrM

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
%name pBinaryOperator BinaryOperator
%name pIntLiteral IntLiteral
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  ',' {PT _ T_CoT}
  ';' { PT _ T_SepT }

L_EndT { PT _ T_EndT }
L_BeginT { PT _ (T_BeginT) }
L_SkipT { PT _ (T_SkipT) }
L_ReadT { PT _ (T_ReadT) }
L_PrintT { PT _ (T_PrintT) }
L_PrintLnT { PT _ (T_PrintLnT) }
L_FreeT { PT _ (T_FreeT) }
L_ExitT { PT _ (T_ExitT) }
L_IntDigit { PT _ (T_IntDigit _) }
L_PlusToken { PT _ (T_PlusToken) }
L_MinusToken { PT _ (T_MinusToken) }
L_TrueToken { PT _ T_TrueToken }
L_FalseToken { PT _ T_FalseToken }
L_IntT { PT _ (T_IntT) }
L_BoolT { PT _ (T_BoolT) }
L_CharT { PT _ (T_CharT) }
L_StringT { PT _ (T_StringT) }
L_TimesT { PT _ (T_TimesT) }
L_DivideT { PT _ (T_DivideT) }
L_ModuloT { PT _ (T_ModuloT) }
L_GreaterT { PT _ (T_GreaterT) }
L_LessT { PT _ (T_LessT) }
L_GreaterEqT { PT _ (T_GreaterEqT) }
L_LessEqT { PT _ (T_LessEqT) }
L_EqT { PT _ (T_EqT) }
L_NotEqT { PT _ (T_NotEqT) }
L_AndT { PT _ (T_AndT) }
L_OrT { PT _ (T_OrT) }
L_LParenT { PT _ (T_LParenT) }
L_RParenT { PT _ (T_RParenT) }
L_LBracketT { PT _ (T_LBracketT) }
L_RBracketT { PT _ (T_RBracketT) }
L_IsT { PT _ (T_IsT) }
L_WhileT { PT _ (T_WhileT) }
L_DoT { PT _ (T_DoT) }
L_DoneT { PT _ (T_DoneT) }
L_IfT { PT _ (T_IfT) }
L_FiT { PT _ (T_FiT) }
L_ThenT { PT _ (T_ThenT) }
L_ElseT { PT _ (T_ElseT) }
L_PairT { PT _ (T_PairT) }
L_NewpairT { PT _ (T_NewpairT) }
L_CallT { PT _ (T_CallT) }
L_FstT { PT _ (T_FstT) }
L_SndT { PT _ (T_SndT) }
L_EqualT { PT _ (T_EqualT) }
L_LenT { PT _ (T_LenT) }
L_OrdT { PT _ (T_OrdT) }
L_ChrT { PT _ (T_ChrT) }
L_ReturnT { PT _ (T_ReturnT) }
L_NotT { PT _ (T_NotT) }
L_PairLiteral { PT _ (T_PairLiteral) }
L_CharLiteral { PT _ (T_CharLiteral _) }
L_StringLiteral { PT _ (T_StringLiteral _) }
L_Identifier { PT _ (T_Identifier _) }

%left L_PlusToken L_MinusToken
%left L_TimesT L_DivideT

%%

EndT    :: { EndT} : L_EndT { EndT (mkPosToken $1)}
BeginT    :: { BeginT} : L_BeginT { BeginT (mkPosToken $1)}
SkipT    :: { SkipT} : L_SkipT { SkipT (mkPosToken $1)}
ReadT    :: { ReadT} : L_ReadT { ReadT (mkPosToken $1)}
PrintT    :: { PrintT} : L_PrintT { PrintT (mkPosToken $1)}
PrintLnT    :: { PrintLnT} : L_PrintLnT { PrintLnT (mkPosToken $1)}
FreeT    :: { FreeT} : L_FreeT { FreeT (mkPosToken $1)}
ExitT    :: { ExitT} : L_ExitT { ExitT (mkPosToken $1)}
IntDigit    :: { IntDigit} : L_IntDigit { IntDigit (mkPosStrToken $1)}
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
PairLiteral    :: { PairLiteral} : L_PairLiteral { PairLiteral (mkPosStrToken $1)}
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
FunListStatement : ReturnT Expression { (:[]) $ AbsWacc.StatReturn $1 $2 }
                 | Statement ';' ListStatement { (:) $1 $3 }
ListFunction :: { [Function] }
ListFunction : {- empty -} { [] }
             | ListFunction Function { flip (:) $1 $2 }
Parameter :: { Parameter }
Parameter : Type Identifier { AbsWacc.Param $1 $2 }
ListParameter :: { [Parameter] }
ListParameter : {- empty -} { [] }
              | Parameter { (:[]) $1 }
              | Parameter ',' ListParameter { (:) $1 $3 }
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
BaseType : IntT { AbsWacc.IntType $1 }
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
Expression : Expression BinaryOperator Expression { AbsWacc.BExp $1 $2 $3 }
           | Expression1 { $1 }
Expression1 :: { Expression }
Expression1 : Expression1 MinusToken Expression2 { AbsWacc.BExp $1 (AbsWacc.BMinus $2) $3 }
            | Expression1 PlusToken Expression2 { AbsWacc.BExp $1 (AbsWacc.BPlus $2) $3 }
            | Expression2 { $1 }
Expression2 :: { Expression }
Expression2 : Expression2 TimesT Expression3 { AbsWacc.BExp $1 (AbsWacc.BTimes $2) $3 }
            | Expression2 DivideT Expression3 { AbsWacc.BExp $1 (AbsWacc.BDivide $2) $3 }
            | Expression3 { $1 }
Expression3 :: { Expression }
Expression3 : Final { $1 }
            | LParenT Expression RParenT { AbsWacc.BracketExp $1 $2 $3 }
Final :: { Expression }
Final : IntLiteral { AbsWacc.IntExp $1 }
      | TrueToken { AbsWacc.BoolExp (AbsWacc.TrueToken $1) }
      | FalseToken { AbsWacc.BoolExp (AbsWacc.FalseToken $1) }
      | CharLiteral { AbsWacc.CharExpr $1 }
      | StringLiteral { AbsWacc.StringExpr $1 }
      | PairLiteral { AbsWacc.PairExpr $1 }
      | Identifier { AbsWacc.IdentExpr $1 }
      | ArrayElem { AbsWacc.ArrayExpr $1 }
      | UnaryOperator Expression { AbsWacc.UExpr $1 $2 }
UnaryOperator :: { UnaryOperator }
UnaryOperator : NotT { AbsWacc.UBang $1 }
              | MinusToken { AbsWacc.UMinus $1 }
              | LenT { AbsWacc.ULength $1 }
              | OrdT { AbsWacc.UOrd $1 }
              | ChrT { AbsWacc.UChr $1 }
BinaryOperator :: { BinaryOperator }
BinaryOperator : ModuloT { AbsWacc.BModulus $1 }
               | GreaterT { AbsWacc.BGreater $1 }
               | LessT { AbsWacc.BLess $1 }
               | GreaterEqT { AbsWacc.BGreaterEqual $1 }
               | LessEqT { AbsWacc.BLessEqual $1 }
               | EqT { AbsWacc.BEqual $1 }
               | NotEqT { AbsWacc.BNotEqual $1 }
               | AndT { AbsWacc.BAnd $1 }
               | OrT { AbsWacc.BOr $1 }
IntLiteral :: { IntLiteral }
IntLiteral : PlusToken IntDigit { AbsWacc.IntPlus $1 $2 }
           | MinusToken IntDigit { AbsWacc.IntMinus $1 $2 }
           | IntDigit { AbsWacc.IntLiteral $1 }
{

mkPosToken :: Token -> Position
mkPosToken t@(PT p _) = Pos $ posLineCol p

mkPosStrToken :: Token -> (Position, String)
mkPosStrToken t@(PT p _) = (Pos $ posLineCol p, prToken t)

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

