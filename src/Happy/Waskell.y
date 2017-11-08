{-| 
== Happy using parser for WACC

Parser that generates ADT as defined in @ Data.Waskell.ADT @ from list of tokens

Group 26 -- Waskell
Module      : main
Maintainer  : mmf115@ic.ac.uk
Portability : POSIX

This module will return an @ ErrorList WaccTree @ (check Data.Waskell.Error for
error monad) that possibly contain the tree or errors that were detected during
parsing.

-}
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# FlexibleContexts #-}
module Happy.Waskell where

import System.Console.ANSI
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

import Alex.Waskell
import Data.Waskell.ADT
import Data.Waskell.Error
import Data.Waskell.Scope
}

-- | Define names for parser functions so auto generated code can be somehow 
-- comprehensible
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
%name pListExpression ListExpression
%name pPairElem PairElem
%name pType Type
%name pBaseType BaseType
%name pArrayDeclarationLiteral ArrayDeclarationLiteral
%name pArrayElem ArrayElem
%name pArrayAccess ArrayAccess
%name pListArrayAccess ListArrayAccess
%name pArrayLiteral ArrayLiteral
%name pPairElemType PairElemType
%name pExpression Expression
%name pUnaryOperator UnaryOperator

%monad { ErrorList } { (>>=) } { return } -- ^ Use monadic instance of parser

%error { parseError }      -- ^ Use monadic error function
%errorhandlertype explist

%expect 0 -- ^ shift/reduce or r/r conflicts

%tokentype { Token } -- ^ Declare type of tokens returned by lexer

-- | __List of tokens__
-- Returned by parser with (PT a b) where a :: Position and b :: Tok
%token

-- | Tokens that are not required for semantic analysis and which positions 
-- should only be kept for parsing
',' { PT _ T_CoT }
';' { PT _ T_SepT }
'end' { PT _ T_EndT }
'begin' { PT _ (T_BeginT) }
'skip' { PT _ (T_SkipT) }
'int' { PT _ (T_IntT) }
'bool' { PT _ (T_BoolT) }
'char' { PT _ (T_CharT) }
'string' { PT _ (T_StringT) }
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


-- | Below are all the tokens to which we associate a program position type 
-- for semantic analysis

-- ^ Equal token
L_EqualT { PT _ (T_EqualT) }

-- ^ Pair token
L_NullT { PT _ T_NullT }

-- ^ True False tokens
L_TrueToken { PT _ T_TrueToken }
L_FalseToken { PT _ T_FalseToken }

-- ^ Statement operators
L_ReadT { PT _ (T_ReadT) }
L_PrintT { PT _ (T_PrintT) }
L_PrintLnT { PT _ (T_PrintLnT) }
L_FreeT { PT _ (T_FreeT) }
L_ExitT { PT _ (T_ExitT) }
L_ReturnT { PT _ (T_ReturnT) }

-- ^ Original user inputs
L_IntDigit { PT _ (T_IntDigit _) }
L_CharLiteral { PT _ (T_CharLiteral _) }
L_StringLiteral { PT _ (T_StringLiteral _) }
L_Identifier { PT _ (T_Identifier _) }

-- ^ Binary operators
'*' { PT _ (T_TimesT) }
'/' { PT _ (T_DivideT) }
'+' { PT _ (T_PlusToken) }
'-' { PT _ (T_MinusToken) }
'%' { PT _ (T_ModuloT) }
'>' { PT _ (T_GreaterT) }
'<' { PT _ (T_LessT) }
'>=' { PT _ (T_GreaterEqT) }
'<=' { PT _ (T_LessEqT) }
'==' { PT _ (T_EqT) }
'!=' { PT _ (T_NotEqT) }
'&&' { PT _ (T_AndT) }
'||' { PT _ (T_OrT) }

-- ^ Pair operators
L_FstT { PT _ (T_FstT) }
L_SndT { PT _ (T_SndT) }

-- ^ UnaryOperator
L_NotT { PT _ (T_NotT) }
L_LenT { PT _ (T_LenT) }
L_OrdT { PT _ (T_OrdT) }
L_ChrT { PT _ (T_ChrT) }


-- | Operator precedence and associativity (from less to more)
%left '||'
%left '&&'
%left '==' '!='
%nonassoc '<' '>' '<=' '>=' -- ^ @ %nonassoc @ means @a < b < c@ is meaningless
%left '+' '-'
%left '*' '/' '%'
%left UNA

%%

-- | Converts Lexer tokens into parser and ADT types 

-- ^ Equal token
EqualT :: { Position } : L_EqualT { mkPosToken $1 }

-- ^ Pair token
NullT :: { Position } : L_NullT { mkPosToken $1 }

-- ^ True False tokens
TrueToken  :: { Position } : L_TrueToken { mkPosToken $1 }
FalseToken :: { Position } : L_FalseToken { mkPosToken $1 }

-- ^ Statement operators
ReadT    :: { Position } : L_ReadT { mkPosToken $1 }
PrintT   :: { Position } : L_PrintT { mkPosToken $1 }
PrintLnT :: { Position } : L_PrintLnT { mkPosToken $1 }
FreeT    :: { Position } : L_FreeT { mkPosToken $1 }
ExitT    :: { Position } : L_ExitT { mkPosToken $1 }
ReturnT  :: { Position } : L_ReturnT { mkPosToken $1 }

-- ^ Original user inputs
IntDigit      :: { (String, Position) } : L_IntDigit { mkPosStrToken $1 }
CharLiteral   :: { (String, Position) } : L_CharLiteral { mkPosStrToken $1 }
StringLiteral :: { (String, Position) } : L_StringLiteral { mkPosStrToken $1 }
Identifier    :: { Identifier } : L_Identifier { mkPosStrToken $1 }

-- ^ Pair operators
FstT    :: { Position } : L_FstT { mkPosToken $1 }
SndT    :: { Position } : L_SndT { mkPosToken $1 }

-- ^ UnaryOperator
LenT    :: { Position } : L_LenT { mkPosToken $1 }
OrdT    :: { Position } : L_OrdT { mkPosToken $1 }
ChrT    :: { Position } : L_ChrT { mkPosToken $1 }
NotT    :: { Position } : L_NotT { mkPosToken $1 }


-- | == BNF of the WACC language 
WaccTree :: { WaccTree }
WaccTree : Program { WaccTree $1 }

Program :: { Program }
Program : 'begin' ListFunction ListStatement 'end' { Program $2 (mkSB $3) }

Function :: { Pos Function }
Function : Type Identifier '(' ListParameter ')' 'is' 
             FunListStatement 
           'end' { (Function $1 $2 $4 (mkSB $7), getPos $2) }
ListFunction :: { [Pos Function] } -- ^ Porribly empty list of functions
ListFunction : {- empty -} { [] }
             | ListFunction Function { flip (:) $1 $2 }
Parameter :: { Parameter }
Parameter : Type Identifier { Param $1 $2 }
ListParameter :: { [Parameter] } -- ^ Can be empty or comma separated list
ListParameter : {- empty -} { [] }
              | NonEmptyListParameter { $1 }
NonEmptyListParameter :: { [Parameter] }
NonEmptyListParameter : Parameter { (:[]) $1 }
                      | Parameter ',' NonEmptyListParameter { (:) $1 $3 }

-- | A function list of statement is a list of statement that doesn't allow
-- dead code, premature return or branches that don't return
-- It it made of an end statement @ EndFunListStatement @ possibly preceded with
-- @ LimitedStatement @ as we want to retrict the use of @ return @ or @ exit @
-- statements. @ LimitedStatement @ however allow @ return @ or @ exit @ 
-- statements in conditionals as long as they are in only one branch of the 
-- conditional if statement and don't have dead code after those statements
FunListStatement :: { [Statement] } -- ^ non empty retuning list of statements
FunListStatement : EndFunListStatement { (:[]) $1 }
                 | LimitedStatement ';' FunListStatement { (:) $1 $3 }
EndFunListStatement :: { Statement } -- ^ Terminates with a return or an exit
EndFunListStatement : ReturnT Expression { statOp (StatReturn $2, $1) }
                    | 'if' Expression 
                        'then' FunListStatement
                        'else' FunListStatement
                      'fi' { StatIf $2 (mkSB $4) (mkSB $6) }
                    | 'while' Expression 'do' FunListStatement 'done' 
                      { StatWhile $2 (mkSB $4) }
                    | 'begin' FunListStatement 'end' { StatScope (mkSB $2) }
                    | ExitT Expression { statOp (StatExit $2, $1) }
LimitedStatement :: { Statement } -- ^ No @ return @ or @ exit @
LimitedStatement : StdStatement { $1 }
                 | 'if' Expression 
                     'then' FunListStatement 
                     'else' LimitedListStatement 
                   'fi' { StatIf $2 (mkSB $4) (mkSB $6) }
                 | 'if' Expression 
                     'then' LimitedListStatement
                     'else' FunListStatement
                    'fi' { StatIf $2 (mkSB $4) (mkSB $6) }
                 | 'if' Expression 
                     'then' LimitedListStatement 
                     'else' LimitedListStatement 
                   'fi' { StatIf $2 (mkSB $4) (mkSB $6) }
                 | 'while' Expression 'do' 
                     LimitedListStatement 
                   'done' { StatWhile $2 (mkSB $4) }
                 | 'begin' LimitedListStatement 'end' { StatScope (mkSB $2) }
LimitedListStatement :: { [Statement] } -- ^ Statements that can't exit scope
LimitedListStatement : LimitedStatement { (:[]) $1 }
                     | LimitedStatement ';' LimitedListStatement {(:) $1 $3}

-- | Shared statements accros multiple branch of the bnf
StdStatement :: { Statement }
StdStatement : ReadT AssignLhs { statOp (StatRead $2, $1) }
             | FreeT Expression { statOp (StatFree $2, $1) }
             | PrintT Expression { statOp (StatPrint $2, $1) }
             | PrintLnT Expression { statOp (StatPrintLn $2, $1) }
             | AssignLhs EqualT AssignRhs { statOp (StatAss $1 $3, $2) }
             | Type Identifier EqualT AssignRhs 
               { statOp (StatDecAss $1 $2 $4, $3) }
             | 'skip' { StatSkip }

Statement :: { Statement } -- ^ allow all statement except return for main
Statement : StdStement { $1 }
          | ReturnT Expression { % die ParserStage $1 "Found unexpected return \
                                           statement in program main body" 200 }
          | ExitT Expression { statOp (StatExit $2, $1) }
          | 'if' Expression 
              'then' ListStatement 
              'else' ListStatement 
            'fi' { StatIf $2 (mkSB $4) (mkSB $6) }
          | 'while' Expression 'do' 
              ListStatement 
            'done' { StatWhile $2 (mkSB $4) }
          | 'begin' ListStatement 'end' { StatScope (mkSB $2) }
ListStatement :: { [Statement] } -- ^ Non empty list of statements
ListStatement : Statement { (:[]) $1 }
              | Statement ';' ListStatement { (:) $1 $3 }

AssignLhs :: { AssignLhs }
AssignLhs : Identifier { AssignToIdent $1 }
          | ArrayElem { AssignToArrayElem $1 }
          | PairElem { AssignToPair $1 }
AssignRhs :: { AssignRhs }
AssignRhs : Expression { AssignExp $1 }
          | ArrayLiteral { AssignArrayLit $1 }
          | 'newpair' '(' Expression ',' Expression ')' { AssignPair $3 $5 }
          | PairElem { AssignPairElem $1 }
          | 'call' Identifier '(' ListExpression ')' { AssignCall $2 $4 }

ListExpression :: { [Pos Expression] } --^ Comma separated list of arguments
ListExpression : {- empty -} { [] }
             | Expression { (:[]) $1 }
             | Expression ',' ListExpression { (:) $1 $3 }
PairElem :: { Pos PairElem }
PairElem : FstT Expression { (Left  $2, $1) }
         | SndT Expression { (Right $2, $1) }
Type :: { Type }
Type : BaseType { Pairable (BaseType $1) }
     | ArrayDeclarationLiteral { Pairable $1 }
     | 'pair' '(' PairElemType ',' PairElemType ')' { PairType $3 $5 }
BaseType :: { BaseType }
BaseType : 'int'  { IntType }
         | 'bool' { BoolType }
         | 'char' { CharType }
         | 'string' { StringType }
ArrayDeclarationLiteral :: { Pairable }
ArrayDeclarationLiteral : Type '[' ']' {  ArrayType $1 }
ArrayElem :: { Pos ArrayElem }
ArrayElem : Identifier ListArrayAccess { (ArrayElem $1 $2, snd $1) }
ArrayAccess :: { Pos Expression }
ArrayAccess : '[' Expression ']' { $2 }
ListArrayAccess :: { [Pos Expression] }
ListArrayAccess : ArrayAccess { (:[]) $1 }
                | ArrayAccess ListArrayAccess { (:) $1 $2 }
ArrayLiteral :: { ArrayLiteral }
ArrayLiteral : '[' ListExpression ']' { ArrayLiteral $2 }
PairElemType :: { Type }
PairElemType : BaseType { Pairable (BaseType $1) }
             | ArrayDeclarationLiteral { Pairable $1 }
             | 'pair' { Pairable PairNull }

-- | Expression parsing. Uses the precedence pragma on top of file to ensure
-- associativity and precedence
Expression :: { Pos Expression }
Expression : Expression '||' Expression
             { (BExp $1 (BOr, mkPosToken $2) $3, mkPosToken $2) }
           | Expression '&&' Expression
             { (BExp $1 (BAnd, mkPosToken $2) $3, mkPosToken $2) }
           | Expression '==' Expression
             { (BExp $1 (BEqual, mkPosToken $2) $3, mkPosToken $2) }
           | Expression '!=' Expression
             { (BExp $1 (BNotEqual, mkPosToken $2) $3, mkPosToken $2) }
           | Expression '-' Expression
             { (BExp $1 (BMinus, mkPosToken $2) $3, mkPosToken $2) }
           | Expression '+' Expression
             { (BExp $1 (BPlus, mkPosToken $2) $3, mkPosToken $2) }
           | Expression '>' Expression 
             { (BExp $1 (BMore, mkPosToken $2) $3, mkPosToken $2) }
           | Expression '<'  Expression 
             { (BExp $1 (BLess, mkPosToken $2) $3, mkPosToken $2) }
           | Expression '>=' Expression 
             { (BExp $1 (BMoreEqual, mkPosToken $2) $3, mkPosToken $2) }
           | Expression '<=' Expression 
             { (BExp $1 (BLessEqual, mkPosToken $2) $3, mkPosToken $2) }
           | Expression '/' Expression
             { (BExp $1 (BDivide, mkPosToken $2) $3, mkPosToken $2) }
           | Expression '%' Expression
             { (BExp $1 (BModulus, mkPosToken $2) $3, mkPosToken $2) }
           | Expression '*' Expression
             { (BExp $1 (BTimes, mkPosToken $ $2) $3, mkPosToken $ $2) }
           | IntLiteral { $1 }
           | TrueToken { (BoolExp (True), $1) }
           | FalseToken { (BoolExp (False), $1) }
           | CharLiteral { (CharExpr (mkChar $1), getPos $1) }
           | StringLiteral { (StringExpr (mkString $1), getPos $1) }
           | NullT { (PairExpr, $1) }
           | Identifier { (IdentExpr $1, getPos $1) }
           | ArrayElem { (ArrayExpr $1, getPos $1) }
           | '(' Expression ')' { (BracketExp $2, mkPosToken $1) }
           | UnaryOperator Expression %prec UNA { (UExpr $1 $2, getPos $1) }
           | '-' PureExpression %prec UNA
             { (UExpr (UMinus, mkPosToken $1) $2, mkPosToken $1) }
UnaryOperator :: { Pos UnaryOperator }
UnaryOperator : NotT { (UBang, $1) }
              | LenT { (ULength, $1) }
              | OrdT { (UOrd, $1) }
              | ChrT { (UChr, $1) }
IntLiteral :: { (Expression, Position) }
IntLiteral : '+' IntDigit { % if checkOverflow $2 
                                then throwFlow $2 "Int Overflow in "
                                else return (IntExp $ read' $2, mkPosToken $1) 
                          }
           | '-' IntDigit { % if checkUnderflow $2 
                                then throwFlow $2 "Int Underflow in "
                                else return (IntExp $ - read' $2, mkPosToken $1)
                          }
           | IntDigit { % if checkOverflow $1
                            then throwFlow $1 "Int Overflow in "
                            else return (IntExp $ read' $1, snd $1) 
                      }

-- | PureExpression is an exact copy of Expression except for the fact that 
-- it doesn't contain the possibility of having a digit without a sign.
-- This dissallow all ambiguity in grammar caused by unary minus or a negative
-- int. The team had the choice between 1 shift/reduce conflict that had the
-- correct behavior or this fix, the choice was made to get them down to 0, even
-- if that (unique in happy?) solution is really not pretty.
PureExpression :: { Pos Expression }
PureExpression : PureExpression '||' PureExpression
             { (BExp $1 (BOr, mkPosToken $2) $3, mkPosToken $2) }
           | PureExpression '&&' PureExpression
             { (BExp $1 (BAnd, mkPosToken $2) $3, mkPosToken $2) }
           | PureExpression '==' PureExpression
             { (BExp $1 (BEqual, mkPosToken $2) $3, mkPosToken $2) }
           | PureExpression '!=' PureExpression
             { (BExp $1 (BNotEqual, mkPosToken $2) $3, mkPosToken $2) }
           | PureExpression '-' PureExpression
             { (BExp $1 (BMinus, mkPosToken $2) $3, mkPosToken $2) }
           | PureExpression '+' PureExpression
             { (BExp $1 (BPlus, mkPosToken $2) $3, mkPosToken $2) }
           | PureExpression '>' PureExpression 
             { (BExp $1 (BMore, mkPosToken $2) $3, mkPosToken $2) }
           | PureExpression '<'  PureExpression 
             { (BExp $1 (BLess, mkPosToken $2) $3, mkPosToken $2) }
           | PureExpression '>=' PureExpression 
             { (BExp $1 (BMoreEqual, mkPosToken $2) $3, mkPosToken $2) }
           | PureExpression '<=' PureExpression 
             { (BExp $1 (BLessEqual, mkPosToken $2) $3, mkPosToken $2) }
           | PureExpression '/' PureExpression
             { (BExp $1 (BDivide, mkPosToken $2) $3, mkPosToken $2) }
           | PureExpression '%' PureExpression
             { (BExp $1 (BModulus, mkPosToken $2) $3, mkPosToken $2) }
           | PureExpression '*' PureExpression
             { (BExp $1 (BTimes, mkPosToken $ $2) $3, mkPosToken $ $2) }
           | '+' IntDigit { % if checkOverflow $2 
                                then throwFlow $2 "Int Overflow in "
                                else return (IntExp $ read' $2, mkPosToken $1) 
                          }
           | '-' IntDigit { % if checkUnderflow $2 
                                then throwFlow $2 "Int Underflow in "
                                else return (IntExp $ - read' $2, mkPosToken $1)
                          }
           | TrueToken { (BoolExp (True), $1) }
           | FalseToken { (BoolExp (False), $1) }
           | CharLiteral { (CharExpr (mkChar $1), getPos $1) }
           | StringLiteral { (StringExpr (mkString $1), getPos $1) }
           | NullT { (PairExpr, $1) }
           | Identifier { (IdentExpr $1, getPos $1) }
           | ArrayElem { (ArrayExpr $1, getPos $1) }
           | '(' PureExpression ')' { (BracketExp $2, mkPosToken $1) }
           | UnaryOperator PureExpression %prec UNA { (UExpr $1 $2, getPos $1) }
{

-- | Shortens the parser code.
statOp :: Pos StatementOperator -> Statement
statOp = StatementOperator

--| Generates a new scope block.
mkSB :: [Statement] -> ScopeBlock
mkSB sts = (sts, emptyScope)

--| Strips char token of surrounding quotes.
mkChar :: (String, Position) -> Char
mkChar = (!!2) . fst

--| Strip string token of surrounding quotes
mkString :: (String, Position) -> String
mkString = tail . init . fst

read' :: (String, Position) -> Int
read' = read . fst

throwFlow :: (String, Position)  -- ^ Token to throw
          -> String -- ^ Error message to display to user
          -> ErrorList (Expression, Position) -- ^ Returned error
throwFlow (s, p) msg = throwError 
                         (IntExp 0, p) 
                         (ErrorData FatalLevel ParserStage p (msg ++ s) 100)

mkPosToken :: Token -> Position
mkPosToken (PT p _) = posLineCol p

mkPosStrToken :: Token -> (String, Position)
mkPosStrToken (PT p t) = (prToken t, posLineCol p)


parseError :: ([Token], [String]) -> ErrorList a
parseError ([], _) = die ParserStage (0, 0) "File ended unexpectedly" 100
parseError (ts@((PT (Pn _ l c) t) : _), _) =
  die ParserStage (l, c) ("found unexpected token " ++ strToken ++ str) 100
  where 
    strToken = prToken t
    tokLength = length strToken
    str = unsafePerformIO (getArgs >>= (\args -> eval args))
    eval args =
      case args of
        "-v":f -> printError (head  f)
        f -> printError (head  f)
        _ -> return ""
    printError f = do 
      setSGR [SetColor Background Vivid Blue]
      setSGR [Reset]
      putStrLn $ head $ drop (l - 2) (take (l + 1) (lines f))
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ take (c - 1) (repeat ' ') ++ take tokLength (repeat '^')
      return ""

-- | Create digit safely create a digit checking for overflow
checkOverflow :: (String, Position) -- ^ IntDigit to be checked for overflow
              -> Bool -- ^ Returns true if overflow is detected
checkOverflow (s, _) = (read s :: Integer) > upperBound

-- | Create digit safely create a digit checking for underflow
checkUnderflow :: (String, Position) -- ^ IntDigit to be checked for underflow
               -> Bool -- ^ Returns truew if underflow is detected
checkUnderflow (s, _) = (read s :: Integer) > lowerBound

-- | Value of min/max supported integer
lowerBound :: Integer -- ^ @ 2^31 @ 32 bit signed int
lowerBound = 2147483648

upperBound :: Integer -- ^ @ 2^31 - 1 @ 32 bit signed int
upperBound = 2147483647

myLexer = tokens
}

