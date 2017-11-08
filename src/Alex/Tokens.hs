{-| 
= WACC Compiler 

This is used by the Lexer and Parser

Group 26 -- Waskell
Module      : Tokens
Maintainer  : pvk16@ic.ac.uk
Portability : POSIX

This module contains the list of tokens and the function to print the tokens when an error has occured.
-}

module Alex.Tokens where

-- | Data type declaration for all the tokens used in WACC
data Tok
 = T_CoT
 | T_SepT
 | T_EndT
 | T_BeginT 
 | T_SkipT 
 | T_ReadT 
 | T_PrintT 
 | T_PrintLnT 
 | T_FreeT 
 | T_ExitT 
 | T_IntDigit !String 
 | T_PlusToken 
 | T_MinusToken 
 | T_TrueToken
 | T_FalseToken 
 | T_IntT 
 | T_BoolT 
 | T_CharT 
 | T_StringT 
 | T_TimesT 
 | T_DivideT 
 | T_ModuloT 
 | T_GreaterT 
 | T_LessT 
 | T_GreaterEqT 
 | T_LessEqT 
 | T_EqT 
 | T_NotEqT 
 | T_AndT 
 | T_OrT 
 | T_LParenT 
 | T_RParenT 
 | T_LBracketT 
 | T_RBracketT 
 | T_IsT 
 | T_WhileT 
 | T_DoT 
 | T_DoneT 
 | T_IfT 
 | T_FiT 
 | T_ThenT 
 | T_ElseT 
 | T_PairT 
 | T_NewpairT 
 | T_CallT 
 | T_FstT 
 | T_SndT 
 | T_EqualT 
 | T_LenT 
 | T_OrdT 
 | T_ChrT 
 | T_ReturnT 
 | T_NotT 
 | T_NullT
 | T_CharLiteral !String
 | T_StringLiteral !String
 | T_Identifier !String

 deriving (Eq,Show,Ord)

-- | Print Token
-- When parsing is not successfull it prints the token converting them back to
-- their string representation in the program
prToken :: Tok    -- ^ The token to convert
        -> String -- ^ original string representation
prToken t = case t of
  T_CoT -> ","
  T_SepT -> ";"
  T_NullT -> "null"
  T_EndT -> "end"
  T_BeginT -> "begin"
  T_SkipT -> "skip"
  T_ReadT -> "read"
  T_PrintT -> "print"
  T_PrintLnT -> "println"
  T_FreeT -> "free"
  T_ExitT -> "exit"
  (T_IntDigit s) -> s
  T_PlusToken -> "+"
  T_MinusToken -> "-"
  (T_TrueToken) -> "true"
  (T_FalseToken) -> "false"
  T_IntT -> "int"
  T_BoolT -> "bool"
  T_CharT -> "char"
  T_StringT -> "string"
  T_TimesT -> "*"
  T_DivideT -> "/"
  T_ModuloT -> "%"
  T_GreaterT -> ">"
  T_LessT -> "<"
  T_GreaterEqT -> ">="
  T_LessEqT -> "<="
  T_EqT -> "=="
  T_NotEqT -> "!="
  T_AndT -> "&&"
  T_OrT -> "||"
  T_LParenT -> ""
  T_RParenT -> ""
  T_LBracketT -> "["
  T_RBracketT -> "]"
  T_IsT -> "is"
  T_WhileT -> "while"
  T_DoT -> "do"
  T_DoneT -> "done"
  T_IfT -> "if"
  T_FiT -> "fi"
  T_ThenT -> "then"
  T_ElseT -> "else"
  T_PairT -> "pair"
  T_NewpairT -> "newpair"
  T_CallT -> "call"
  T_FstT -> "fst"
  T_SndT -> "snd"
  T_EqualT -> "="
  T_LenT -> "len"
  T_OrdT -> "ord"
  T_ChrT -> "chr"
  T_ReturnT -> "return"
  T_NotT -> "!"
  (T_CharLiteral s) -> s
  (T_StringLiteral s) -> s
  (T_Identifier s) -> s


