-- -*- haskell -*-
-- This Alex file was machine-generated by the BNF converter
{
{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -w #-}
module Bnfc.LexWacc where



import qualified Data.Bits
import Data.Word (Word8)
import Data.Char (ord)
import Data.List (reverse)
}


$l = [a-zA-Z\192 - \255] # [\215 \247]    -- isolatin1 letter FIXME
$c = [A-Z\192-\221] # [\215]    -- capital isolatin1 letter FIXME
$s = [a-z\222-\255] # [\247]    -- small isolatin1 letter FIXME
$d = [0-9]                -- digit
$i = [$l $d _ ']          -- identifier character
$u = [\0-\255]          -- universal: any character

@rsyms =    -- symbols and non-identifier-like reserved words
   \, | \;

:-
"#" [.]* ; -- Toss single line comments

$white+ ;
\,              { tok (\p s -> PT p (T_CoT)) }
\; 							{ tok (\p s -> PT p (T_SepT)) }
e n d 							{ tok (\p s -> PT p T_EndT) }
b e g i n 						{ tok (\p s -> PT p T_BeginT) }
s k i p		        				{ tok (\p s -> PT p (T_SkipT)) }
r e a d 						{ tok (\p s -> PT p (T_ReadT ))}
p r i n t 						{ tok (\p s -> PT p ((T_PrintT ))) }
p r i n t l n 						{ tok (\p s -> PT p ((T_PrintLnT ))) }
f r e e 						{ tok (\p s -> PT p ((T_FreeT ))) }
e x i t 						{ tok (\p s -> PT p ((T_ExitT ))) }
$d + 							{ tok (\p s -> if checkOverflow s then Err p else PT p ((T_IntDigit) s)) }
\+ 							{ tok (\p s -> PT p ((T_PlusToken ) )) }
\- 							{ tok (\p s -> PT p ((T_MinusToken ) )) }
t r u e  					{ tok (\p s -> PT p T_TrueToken) }
f a l s e         { tok (\p s -> PT p T_FalseToken)}
i n t 							{ tok (\p s -> PT p ((T_IntT ) )) }
b o o l 						{ tok (\p s -> PT p ((T_BoolT ) )) }
c h a r 						{ tok (\p s -> PT p ((T_CharT ) )) }
s t r i n g 						{ tok (\p s -> PT p ((T_StringT ) )) }
\* 							{ tok (\p s -> PT p ((T_TimesT ) )) }
\/ 							{ tok (\p s -> PT p ((T_DivideT ) )) }
\% 							{ tok (\p s -> PT p ((T_ModuloT ) )) }
\> 							{ tok (\p s -> PT p ((T_GreaterT ) )) }
\< 							{ tok (\p s -> PT p ((T_LessT ) )) }
\> \=							{ tok (\p s -> PT p ((T_GreaterEqT ) )) }
\< \= 							{ tok (\p s -> PT p ((T_LessEqT ) )) }
\= \= 							{ tok (\p s -> PT p ((T_EqT ) )) }
\! \=		 					{ tok (\p s -> PT p ((T_NotEqT ) )) }
\& \& 							{ tok (\p s -> PT p ((T_AndT ) )) }
\| \| 							{ tok (\p s -> PT p ((T_OrT ) )) }
\( 							{ tok (\p s -> PT p ((T_LParenT ) )) }
\) 							{ tok (\p s -> PT p ((T_RParenT ) )) }
\[ 							{ tok (\p s -> PT p ((T_LBracketT ) )) }
\] 							{ tok (\p s -> PT p ((T_RBracketT ) )) }
i s 							{ tok (\p s -> PT p ((T_IsT ) )) }
w h i l e 						{ tok (\p s -> PT p ((T_WhileT ) )) }
d o 							{ tok (\p s -> PT p ((T_DoT ) )) }
d o n e 						{ tok (\p s -> PT p ((T_DoneT ) )) }
i f 							{ tok (\p s -> PT p ((T_IfT ) )) }
f i 							{ tok (\p s -> PT p ((T_FiT ) )) }
t h e n 						{ tok (\p s -> PT p ((T_ThenT ) )) }
e l s e 						{ tok (\p s -> PT p ((T_ElseT ) )) }
p a i r 						{ tok (\p s -> PT p ((T_PairT ) )) }
n e w p a i r 						{ tok (\p s -> PT p ((T_NewpairT ) )) }
c a l l 						{ tok (\p s -> PT p ((T_CallT ) )) }
f s t 							{ tok (\p s -> PT p ((T_FstT ) )) }
s n d 							{ tok (\p s -> PT p ((T_SndT ) )) }
\= 							{ tok (\p s -> PT p ((T_EqualT ) )) }
l e n 							{ tok (\p s -> PT p ((T_LenT ) )) }
o r d 							{ tok (\p s -> PT p ((T_OrdT ) )) }
c h r 							{ tok (\p s -> PT p ((T_ChrT ) )) }
r e t u r n 						{ tok (\p s -> PT p ((T_ReturnT ) )) }
\! 							{ tok (\p s -> PT p ((T_NotT ) )) }
n u l l 						{ tok (\p s -> PT p ((T_PairLiteral ) )) }
\' ($u # [\' \\ \"]| \\ [\' \\ n t 0 b f \"]) \' 	{ tok (\p s -> PT p ((T_CharLiteral ) s)) }
\" ($u # [\' \\ \"]| \\ [\' \\ n t 0 b f \"]) * \" 	{ tok (\p s -> PT p ((T_StringLiteral ) s)) }
(\_ | $s)($l | $d | \_)* 				{ tok (\p s -> PT p ((T_Identifier ) s)) }






{

tok :: (Posn -> String -> Token) -> (Posn -> String -> Token)
tok f p s = f p s

share :: String -> String
share = id

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
 | T_PairLiteral
 | T_CharLiteral !String
 | T_StringLiteral !String
 | T_Identifier !String

 deriving (Eq,Show,Ord)

data Token =
   PT  Posn Tok
 | Err Posn
  deriving (Eq,Show,Ord)

tokenPos :: [Token] -> String
tokenPos (PT (Pn _ l _) _ :_) = "line " ++ show l
tokenPos (Err (Pn _ l _) :_) = "line " ++ show l
tokenPos _ = "end of file"

tokenPosn :: Token -> Posn
tokenPosn (PT p _) = p
tokenPosn (Err p) = p

tokenLineCol :: Token -> (Int, Int)
tokenLineCol = posLineCol . tokenPosn

posLineCol :: Posn -> (Int, Int)
posLineCol (Pn _ l c) = (l,c)

prToken :: Token -> String
prToken (Err (Pn _ _ col)) 
  = " the col " ++ show col ++ ", probably due to an int overflow"
prToken t = case t of
  PT _ (T_CoT) -> ","
  PT _ (T_SepT) -> ";"
  PT _ (T_EndT) -> "end"
  PT _ (T_BeginT) -> "begin"
  PT _ (T_SkipT) -> "skip"
  PT _ (T_ReadT) -> "read"
  PT _ (T_PrintT) -> "print"
  PT _ (T_PrintLnT) -> "println"
  PT _ (T_FreeT) -> "free"
  PT _ (T_ExitT) -> "exit"
  PT _ (T_IntDigit s) -> s
  PT _ (T_PlusToken) -> "+"
  PT _ (T_MinusToken) -> "-"
  PT _ (T_TrueToken) -> "true"
  PT _ (T_FalseToken) -> "false"
  PT _ (T_IntT) -> "int"
  PT _ (T_BoolT) -> "bool"
  PT _ (T_CharT) -> "char"
  PT _ (T_StringT) -> "string"
  PT _ (T_TimesT) -> "*"
  PT _ (T_DivideT) -> "/"
  PT _ (T_ModuloT) -> "%"
  PT _ (T_GreaterT) -> ">"
  PT _ (T_LessT) -> "<"
  PT _ (T_GreaterEqT) -> ">="
  PT _ (T_LessEqT) -> "<="
  PT _ (T_EqT) -> "=="
  PT _ (T_NotEqT) -> "!="
  PT _ (T_AndT) -> "&&"
  PT _ (T_OrT) -> "||"
  PT _ (T_LParenT) -> "("
  PT _ (T_RParenT) -> ")"
  PT _ (T_LBracketT) -> "["
  PT _ (T_RBracketT) -> "]"
  PT _ (T_IsT) -> "is"
  PT _ (T_WhileT) -> "while"
  PT _ (T_DoT) -> "do"
  PT _ (T_DoneT) -> "done"
  PT _ (T_IfT) -> "if"
  PT _ (T_FiT) -> "fi"
  PT _ (T_ThenT) -> "then"
  PT _ (T_ElseT) -> "else"
  PT _ (T_PairT) -> "pair"
  PT _ (T_NewpairT) -> "newpair"
  PT _ (T_CallT) -> "call"
  PT _ (T_FstT) -> "fst"
  PT _ (T_SndT) -> "snd"
  PT _ (T_EqualT) -> "="
  PT _ (T_LenT) -> "len"
  PT _ (T_OrdT) -> "ord"
  PT _ (T_ChrT) -> "chr"
  PT _ (T_ReturnT) -> "return"
  PT _ (T_NotT) -> "!"
  PT _ (T_PairLiteral) -> "null"
  PT _ (T_CharLiteral s) -> s
  PT _ (T_StringLiteral s) -> s
  PT _ (T_Identifier s) -> s

maxInteger :: String
maxInteger = "2147483647" -- ^ 2^31 - 1 32 bit signed int

checkOverflow :: String -> Bool
checkOverflow s = (read s :: Integer) > (read maxInteger :: Integer)

data BTree = N | B String Tok BTree BTree deriving (Show)

-- eitherResIdent :: (String -> Tok) -> String -> Tok
-- eitherResIdent tv s = treeFind resWords
--  where
--  treeFind N = tv s
--  treeFind (B a t left right) | s < a  = treeFind left
--                              | s > a  = treeFind right
--                              | s == a = t

-- resWords :: BTree
-- resWords = b ";" 2 (b "," 1 N N) N
--   where b s n = let bs = id s
--                  in B bs (TS bs n)

unescapeInitTail :: String -> String
unescapeInitTail = id . unesc . tail . id where
  unesc s = case s of
    '\\':c:cs | elem c ['\"', '\\', '\''] -> c : unesc cs
    '\\':'n':cs  -> '\n' : unesc cs
    '\\':'t':cs  -> '\t' : unesc cs
    '"':[]    -> []
    c:cs      -> c : unesc cs
    _         -> []

-------------------------------------------------------------------
-- Alex wrapper code.
-- A modified "posn" wrapper.
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

type AlexInput = (Posn,     -- current position,
                  Char,     -- previous char
                  [Byte],   -- pending bytes on the current char
                  String)   -- current input string

tokens :: String -> [Token]
tokens str = go (alexStartPos, '\n', [], str)
    where
      go :: AlexInput -> [Token]
      go inp@(pos, _, _, str) =
               case alexScan inp 0 of
                AlexEOF                   -> []
                AlexError (pos, _, _, _)  -> [Err pos]
                AlexSkip  inp' len        -> go inp'
                AlexToken inp' len act    -> act pos (take len str) : (go inp')

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
