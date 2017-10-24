{-|
Module      : Data.Waskell
Description : Short description
Maintainer  : kc1616@ic.ac.uk
License     : ISC
Stability   : experimental
Portability : POSIX

This module provides the abstract syntax tree data types for the Waskell 
compiler. This module is designed to be imported qualified to avoid conflicts 
with the prelude. E.g @import qualified Data.Waskell as W@
-}

module Data.Waskell
  (
    WaccTree,
    Program,
    Function,
    PairAccess,
    PairElem,
    ArrayElem,
    Ident,
    Type,
    UnaryOperator,
    BinaryOperator,
    Statement,
    Expression,
    AssignLeft,
    AssignRight
  ) where

import qualified Prelude as P

newtype WaccTree = WaccTree Program
data Program = Program [Function] [Statement]
data Function = Function Ident [(Type, Ident)] [Statement]

data PairAccess = First | Second
type PairElem = (PairAccess, Expression)

type ArrayElem = (Ident, P.Int)
type Ident = P.String

data Type = Int 
          | Bool 
          | Char 
          | String 
          | Array Type 
          | Pair (Type, Type)

data UnaryOperator = Not | UMinus | Len | Ord | Chr
data BinaryOperator = Times 
                    | Divide
                    | Modulo
                    | Plus 
                    | Minus 
                    | GreaterThan 
                    | GreaterEqual
                    | LessThan
                    | LessEqual
                    | Equals
                    | NotEquals
                    | And
                    | Or

data Statement = Skip 
               | Declare Type Ident AssignRight
               | Assign AssignLeft AssignRight
               | Read AssignLeft
               | Free Expression
               | Return Expression
               | Exit Expression
               | Print Expression
               | PrintLn Expression
               | If Expression [Statement] [Statement]
               | While Expression [Statement]
               | NewScope [Statement]

data Expression = IntLit P.Int 
                | BoolLit P.Bool
                | CharLit P.Char
                | StringLit P.String
                | NullPairLit
                | EvalIdent Ident
                | ArrayAccess ArrayElem
                | UnApp UnaryOperator Expression
                | BinApp BinaryOperator (Expression, Expression)
                | Bracket Expression

data AssignLeft = AssignIdent Ident 
                | ArrayAssign ArrayElem
                | PairAssign PairElem

data AssignRight = Expression
                 | ArrayLiteral [Expression]
                 | PairLiteral (Expression, Expression)
                 | PairVal PairElem
                 | Call Ident [Expression]
