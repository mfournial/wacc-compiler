{
{-| 
== Lexer for Wacc using Alex

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

import Alex.Tokens
import Data.Waskell.Error
}

-- | Alex lexer file
-- It creates tokens based on input.
-- Our base tokens contain a position that we carry all the way in the ADT to
-- be able to send precise semantic errors

-- List of tokens: 
$digit     = [0-9]                              -- ^ digit regex
$universal = [\0-\255]                          -- ^ universal: any character
$letter    = [a-zA-Z\192 - \255] # [\215 \247]  -- ^ letter regex


tokens :-
"#" [.]*            ;  -- ^ Toss single line comments

-- | These are all the keywords token generation code, wrapped in a position
$white+             ;

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
$digit +            { (\p s -> PT p (T_IntDigit s))}
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
\' ($universal # [\' \\ \"]| \\ [\' \\ r n t 0 b f \"]) \'  { (\p s -> PT p ((T_CharLiteral ) s)) }   -- ^ Char token
\" ($universal # [\' \\ \"]| \\ [\' \\ r n t 0 b f \"]) * \"{ (\p s -> PT p ((T_StringLiteral ) s)) } -- ^ String token
(\_ | $letter)($letter | $digit | \_)*                    { (\p s -> PT p ((T_Identifier ) s)) }    -- ^ Identifier token

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
                                                        ([chr] ++ " unknown: " ++ s)
                                                        syntaxErrorCode]
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
