{
{-| 
== Alex using Lexer for Wacc

Lexer for WACC language

Group 26 -- Waskell
Module      : Alex.Waskell
Maintainer  : mmf115@ic.ac.uk
Portability : POSIX

This module was designed call tokens to tokenize input

-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -w #-}
module Alex.Waskell where

import qualified Data.Bits
import Data.Word (Word8)
import Data.Char (ord)

import Data.Waskell.Error
}

-- | Alex lexer file
-- It creates tokens based on input.
-- Our base tokens contain a position that we carry all the way in the ADT to
-- be able to send precise semantic errors

-- List of tokens: 
$d = [0-9]                              -- ^ digit
$u = [\0-\255]                          -- ^ universal: any character
$s = [a-z\222-\255] # [\247]            -- ^ lowercase letter
$l = [a-zA-Z\192 - \255] # [\215 \247]  -- ^ letter


@rsyms =    -- ^ symbols and non-identifier-like reserved words
   \, | \;

:-
"#" [.]* ;  -- ^ Toss single line comments

-- | These are all the keywords token generation code, wrapped in a position
$white+ ;

\,                  { (\p s -> PT p T_CoT) }
\;                  { (\p s -> PT p T_SepT) }
n u l l             { (\p s -> PT p T_NullT) }
e n d               { (\p s -> PT p T_EndT) }
b e g i n           { (\p s -> PT p T_BeginT) }
s k i p             { (\p s -> PT p T_SkipT) }
r e a d             { (\p s -> PT p T_ReadT) }
p r i n t           { (\p s -> PT p T_PrintT) }
p r i n t l n       { (\p s -> PT p T_PrintLnT) }
f r e e             { (\p s -> PT p T_FreeT) }
e x i t             { (\p s -> PT p T_ExitT) }
$d +                { (\p s -> PT p (T_IntDigit s))}
\+                  { (\p s -> PT p T_PlusToken) } 
\-                  { (\p s -> PT p T_MinusToken) } 
t r u e             { (\p s -> PT p T_TrueToken) }
f a l s e           { (\p s -> PT p T_FalseToken)}
i n t               { (\p s -> PT p T_IntT) }
b o o l             { (\p s -> PT p T_BoolT) }
c h a r             { (\p s -> PT p T_CharT) }
s t r i n g         { (\p s -> PT p T_StringT) }
\*                  { (\p s -> PT p T_TimesT) }
\/                  { (\p s -> PT p T_DivideT) }
\%                  { (\p s -> PT p T_ModuloT) }
\>                  { (\p s -> PT p T_GreaterT) }
\<                  { (\p s -> PT p T_LessT) }
\> \=               { (\p s -> PT p T_GreaterEqT) }
\< \=               { (\p s -> PT p T_LessEqT) }
\= \=               { (\p s -> PT p T_EqT) }
\! \=               { (\p s -> PT p T_NotEqT) }
\& \&               { (\p s -> PT p T_AndT) }
\| \|               { (\p s -> PT p T_OrT) }
\(                  { (\p s -> PT p T_LParenT) }
\)                  { (\p s -> PT p T_RParenT) }
\[                  { (\p s -> PT p T_LBracketT) }
\]                  { (\p s -> PT p T_RBracketT) }
i s                 { (\p s -> PT p T_IsT) }
w h i l e           { (\p s -> PT p T_WhileT) }
d o                 { (\p s -> PT p T_DoT) }
d o n e             { (\p s -> PT p T_DoneT) }
i f                 { (\p s -> PT p T_IfT) }
f i                 { (\p s -> PT p T_FiT) }
t h e n             { (\p s -> PT p T_ThenT) }
e l s e             { (\p s -> PT p T_ElseT) }
p a i r             { (\p s -> PT p T_PairT) }
n e w p a i r       { (\p s -> PT p T_NewpairT) }
c a l l             { (\p s -> PT p T_CallT) }
f s t               { (\p s -> PT p T_FstT) }
s n d               { (\p s -> PT p T_SndT) }
\=                  { (\p s -> PT p T_EqualT) }
l e n               { (\p s -> PT p T_LenT) }
o r d               { (\p s -> PT p T_OrdT) }
c h r               { (\p s -> PT p T_ChrT) }
r e t u r n         { (\p s -> PT p T_ReturnT) }
\!                  { (\p s -> PT p T_NotT) }

-- | Special tokens matched with regualr exp
\' ($u # [\' \\ \"]| \\ [\' \\ n t 0 b f \"]) \'  { (\p s -> PT p ((T_CharLiteral ) s)) } -- ^ Char token
\" ($u # [\' \\ \"]| \\ [\' \\ n t 0 b f \"]) * \"{ (\p s -> PT p ((T_StringLiteral ) s)) } -- ^ String token
(\_ | $l)($l | $d | \_)*                          { (\p s -> PT p ((T_Identifier ) s)) } -- ^ Identifier token

{
{- | Create digit safely create a digit checking for overflow
createDigit :: Posn    -- ^ Position of the checked token
            -> String  -- ^ String 
            -> Token   -- ^ Token returned, might be an Err token
createDigit p s = 
  if checkBound s 
    then Err p 
    else PT p (T_IntDigit s)

-- | Value of min/max supported integer
bound :: Integer -- ^ @ 2^31 @ 32 bit signed int
bound = 2147483648

-- | Checks overflow and underflow assignments
checkBound :: String -> Bool
checkBound s = (read s :: Integer) > bound-}

share :: String -> String
share = id

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

-- | Token type declaration it can be made of:
data Token
 = PT Posn Tok -- ^ Tok is raw identifier of the token wrapped in a Position
  deriving (Eq,Show,Ord)

-- | Pretty prints position
tokenPos :: [Token] -> String
tokenPos (PT (Pn _ l c) _ :_) = "(" ++ show l ++ ", " ++ show c ++ ")"
tokenPos _ = "at end of file"

-- | Starps Token to return position
tokenPosn :: Token -> Posn
tokenPosn (PT p _) = p

-- | Converts Token into a position pair
tokenLineCol :: Token -> (Int, Int)
tokenLineCol = posLineCol . tokenPosn

posLineCol :: Posn -> (Int, Int)
posLineCol (Pn _ l c) = (l,c)

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

-------------------------------------------------------------------
-- | Alex wrapper code.
-- A modified "posn" wrapper. 
-- Walks the file assuming UTF-8 encoding (for extension?) 
-------------------------------------------------------------------

data Posn = Pn !Int !Int !Int
      deriving (Eq, Show,Ord)

alexStartPos :: Posn
alexStartPos = Pn 0 1 1

alexMove :: Posn -> Char -> Posn
alexMove (Pn a l c) '\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (Pn a l c) '\n' = Pn (a+1) (l+1)   1
alexMove (Pn a l c) _    = Pn (a+1)  l     (c+1)

type Byte = Word8

type AlexInput = (Posn,     -- current position
                  Char,     -- previous char
                  [Byte],   -- pending bytes on the current char
                  String)   -- current input string

tokens :: String -> ErrorList [Token]
tokens str = sequence $ go (alexStartPos, '\n', [], str)
    where
      go :: AlexInput -> [ErrorList Token]
      go inp@(pos, _, _, str) =
               case alexScan inp 0 of
                 AlexEOF                   -> []
                 AlexError ((Pn _ l c), chr, _, s)  -> [die LexorStage (l, c) 
                                                        ([chr] ++ " on " ++ s)
                                                        100]
                 AlexSkip  inp' len        -> go inp'
                 AlexToken inp' len act    -> (return 
                                          (act pos (take len str))): (go inp')

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p, c, (b:bs), s) = Just (b, (p, c, bs, s))
alexGetByte (p, _, [], s) =
  case  s of
    []  -> Nothing
    (c:s) ->
             let p'     = alexMove p c
                 (b:bs) = utf8Encode c
              in p' `seq` Just (b, (p', c, bs, s))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p, c, bs, s) = c

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
}
