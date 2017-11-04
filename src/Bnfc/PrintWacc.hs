{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Bnfc.PrintWacc where

-- pretty-printer generated by the BNF converter

import Bnfc.AbsWacc as AbsWacc
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)



instance Print EndT where
  prt _ (EndT (_)) = doc . showString $ "END"


instance Print BeginT where
  prt _ (BeginT (_)) = doc . showString $ "BEGIN"


instance Print SkipT where
  prt _ (SkipT (_)) = doc . showString $ "SKIP"


instance Print ReadT where
  prt _ (ReadT (_)) = doc . showString $ "READ"


instance Print PrintT where
  prt _ (PrintT (_)) = doc . showString $ "PRINT"


instance Print PrintLnT where
  prt _ (PrintLnT (_)) = doc . showString $ "PRINTLN"


instance Print FreeT where
  prt _ (FreeT (_)) = doc . showString $ "FREE"


instance Print ExitT where
  prt _ (ExitT (_)) = doc . showString $ "EXIT"


instance Print IntDigit where
  prt _ (IntDigit (_, i)) = doc .showString $ i


instance Print PlusToken where
  prt _ (PlusToken (_)) = doc . showString $ "+"


instance Print MinusToken where
  prt _ (MinusToken (_)) = doc . showString $ "-"


instance Print BoolLiteral where
  prt _ e = case e of
    (TrueToken _)  -> doc . showString $ "TRUE"
    (FalseToken _) -> doc . showString $ "FALSE"

instance Print IntT where
  prt _ (IntT (_)) = doc . showString $ "INT"


instance Print BoolT where
  prt _ (BoolT (_)) = doc . showString $ "BOOL"


instance Print CharT where
  prt _ (CharT (_)) = doc . showString $ "CHAR"


instance Print StringT where
  prt _ (StringT (_)) = doc . showString $ "STRING"


instance Print TimesT where
  prt _ (TimesT (_)) = doc . showString $ "*"


instance Print DivideT where
  prt _ (DivideT (_)) = doc . showString $ "/"


instance Print ModuloT where
  prt _ (ModuloT (_)) = doc . showString $ "%"


instance Print GreaterT where
  prt _ (GreaterT (_)) = doc . showString $ ">"


instance Print LessT where
  prt _ (LessT (_)) = doc . showString $ "<"


instance Print GreaterEqT where
  prt _ (GreaterEqT (_)) = doc . showString $ ">="


instance Print LessEqT where
  prt _ (LessEqT (_)) = doc . showString $ "<="


instance Print EqT where
  prt _ (EqT (_)) = doc . showString $ "=="


instance Print NotEqT where
  prt _ (NotEqT (_)) = doc . showString $ "!="


instance Print AndT where
  prt _ (AndT (_)) = doc . showString $ "&&"


instance Print OrT where
  prt _ (OrT (_)) = doc . showString $ "||"


instance Print LParenT where
  prt _ (LParenT (_)) = doc . showString $ "("


instance Print RParenT where
  prt _ (RParenT (_)) = doc . showString $ ")"


instance Print LBracketT where
  prt _ (LBracketT (_)) = doc . showString $ "["


instance Print RBracketT where
  prt _ (RBracketT (_)) = doc . showString $ "]"


instance Print IsT where
  prt _ (IsT (_)) = doc . showString $ "IS"


instance Print WhileT where
  prt _ (WhileT (_)) = doc . showString $ "WHILE"


instance Print DoT where
  prt _ (DoT (_)) = doc . showString $ "DO"


instance Print DoneT where
  prt _ (DoneT (_)) = doc . showString $ "DONE"


instance Print IfT where
  prt _ (IfT (_)) = doc . showString $ "IF"


instance Print FiT where
  prt _ (FiT (_)) = doc . showString $ "FI"


instance Print ThenT where
  prt _ (ThenT (_)) = doc . showString $ "THEN"


instance Print ElseT where
  prt _ (ElseT (_)) = doc . showString $ "ELSE"


instance Print PairT where
  prt _ (PairT (_)) = doc . showString $ "PAIR"


instance Print NewpairT where
  prt _ (NewpairT (_)) = doc . showString $ "NEWPAIR"


instance Print CallT where
  prt _ (CallT (_)) = doc . showString $ "CALL"


instance Print FstT where
  prt _ (FstT (_)) = doc . showString $ "FST"


instance Print SndT where
  prt _ (SndT (_)) = doc . showString $ "SND"


instance Print EqualT where
  prt _ (EqualT (_)) = doc . showString $ "="


instance Print LenT where
  prt _ (LenT (_)) = doc . showString $ "LEN"


instance Print OrdT where
  prt _ (OrdT (_)) = doc . showString $ "ORD"


instance Print ChrT where
  prt _ (ChrT (_)) = doc . showString $ "CHR"


instance Print ReturnT where
  prt _ (ReturnT (_)) = doc . showString $ "RETURN"


instance Print NotT where
  prt _ (NotT (_)) = doc . showString $ "!"


instance Print PairLiteral where
  prt _ (PairLiteral (_)) = doc . showString $ "NULL"


instance Print CharLiteral where
  prt _ (CharLiteral (_, i)) = doc . showString $ i


instance Print StringLiteral where
  prt _ (StringLiteral (_, i)) = doc . showString $ i


instance Print Identifier where
  prt _ (Identifier (_, i)) = doc . showString $ i



instance Print WaccTree where
  prt i e = case e of
    WaccTree program -> prPrec i 0 (concatD [prt 0 program])

instance Print Program where
  prt i e = case e of
    Program begint functions statements endt -> prPrec i 0 (concatD [prt 0 begint, prt 0 functions, prt 0 statements, prt 0 endt])

instance Print Function where
  prt i e = case e of
    Function type_ identifier lparent parameters rparent ist statements endt -> prPrec i 0 (concatD [prt 0 type_, prt 0 identifier, prt 0 lparent, prt 0 parameters, prt 0 rparent, prt 0 ist, prt 0 statements, prt 0 endt])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print Parameter where
  prt i e = case e of
    Param type_ identifier -> prPrec i 0 (concatD [prt 0 type_, prt 0 identifier])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Statement where
  prt i e = case e of
    StatSkip skipt -> prPrec i 0 (concatD [prt 0 skipt])
    StatDecAss type_ identifier equalt assignrhs -> prPrec i 0 (concatD [prt 0 type_, prt 0 identifier, prt 0 equalt, prt 0 assignrhs])
    StatAss assignlhs equalt assignrhs -> prPrec i 0 (concatD [prt 0 assignlhs, prt 0 equalt, prt 0 assignrhs])
    StatRead readt assignlhs -> prPrec i 0 (concatD [prt 0 readt, prt 0 assignlhs])
    StatFree freet expression -> prPrec i 0 (concatD [prt 0 freet, prt 0 expression])
    StatReturn returnt expression -> prPrec i 0 (concatD [prt 0 returnt, prt 0 expression])
    StatExit exitt expression -> prPrec i 0 (concatD [prt 0 exitt, prt 0 expression])
    StatPrint printt expression -> prPrec i 0 (concatD [prt 0 printt, prt 0 expression])
    StatPrintLn printlnt expression -> prPrec i 0 (concatD [prt 0 printlnt, prt 0 expression])
    StatIf ift expression thent statements1 elset statements2 fit -> prPrec i 0 (concatD [prt 0 ift, prt 0 expression, prt 0 thent, prt 0 statements1, prt 0 elset, prt 0 statements2, prt 0 fit])
    StatWhile whilet expression dot statements donet -> prPrec i 0 (concatD [prt 0 whilet, prt 0 expression, prt 0 dot, prt 0 statements, prt 0 donet])
    StatScope begint statements endt -> prPrec i 0 (concatD [prt 0 begint, prt 0 statements, prt 0 endt])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print AssignLhs where
  prt i e = case e of
    AssignToIdent identifier -> prPrec i 0 (concatD [prt 0 identifier])
    AssignToArrayElem arrayelem -> prPrec i 0 (concatD [prt 0 arrayelem])
    AssignToPair pairelem -> prPrec i 0 (concatD [prt 0 pairelem])

instance Print AssignRhs where
  prt i e = case e of
    AssignExp expression -> prPrec i 0 (concatD [prt 0 expression])
    AssignArrayLit arrayliteral -> prPrec i 0 (concatD [prt 0 arrayliteral])
    AssignPair newpairt lparent expression1 expression2 rparent -> prPrec i 0 (concatD [prt 0 newpairt, prt 0 lparent, prt 0 expression1, doc (showString ","), prt 0 expression2, prt 0 rparent])
    AssignPairElem pairelem -> prPrec i 0 (concatD [prt 0 pairelem])
    AssignFunctionCall callt identifier lparent argumentlists rparent -> prPrec i 0 (concatD [prt 0 callt, prt 0 identifier, prt 0 lparent, prt 0 argumentlists, prt 0 rparent])

instance Print ArgumentList where
  prt i e = case e of
    ArgumentList expression -> prPrec i 0 (concatD [prt 0 expression])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print PairElem where
  prt i e = case e of
    PairFst fstt expression -> prPrec i 0 (concatD [prt 0 fstt, prt 0 expression])
    PairSnd sndt expression -> prPrec i 0 (concatD [prt 0 sndt, prt 0 expression])

instance Print Type where
  prt i e = case e of
    BaseType basetype -> prPrec i 0 (concatD [prt 0 basetype])
    ArrayType arraydeclarationliteral -> prPrec i 0 (concatD [prt 0 arraydeclarationliteral])
    PairType pairt lparent pairelemtype1 pairelemtype2 rparent -> prPrec i 0 (concatD [prt 0 pairt, prt 0 lparent, prt 0 pairelemtype1, doc (showString ","), prt 0 pairelemtype2, prt 0 rparent])

instance Print BaseType where
  prt i e = case e of
    IntType intt -> prPrec i 0 (concatD [prt 0 intt])
    BoolType boolt -> prPrec i 0 (concatD [prt 0 boolt])
    CharType chart -> prPrec i 0 (concatD [prt 0 chart])
    StringType stringt -> prPrec i 0 (concatD [prt 0 stringt])

instance Print ArrayDeclarationLiteral where
  prt i e = case e of
    ArrayDeclarationLiteral type_ lbrackett rbrackett -> prPrec i 0 (concatD [prt 0 type_, prt 0 lbrackett, prt 0 rbrackett])

instance Print ArrayElem where
  prt i e = case e of
    ArrayElem identifier arrayaccesss -> prPrec i 0 (concatD [prt 0 identifier, prt 0 arrayaccesss])

instance Print ArrayAccess where
  prt i e = case e of
    ArrayAccess lbrackett expression rbrackett -> prPrec i 0 (concatD [prt 0 lbrackett, prt 0 expression, prt 0 rbrackett])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print ArrayLiteral where
  prt i e = case e of
    ArrayLiteral lbrackett arrayliteralelems rbrackett -> prPrec i 0 (concatD [prt 0 lbrackett, prt 0 arrayliteralelems, prt 0 rbrackett])

instance Print ArrayLiteralElem where
  prt i e = case e of
    ArrayLiteralElem expression -> prPrec i 0 (concatD [prt 0 expression])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print PairElemType where
  prt i e = case e of
    PairElemTypeBase basetype -> prPrec i 0 (concatD [prt 0 basetype])
    PairElemTypeArray arraydeclarationliteral -> prPrec i 0 (concatD [prt 0 arraydeclarationliteral])
    PairElemTypePair pairt -> prPrec i 0 (concatD [prt 0 pairt])

instance Print Expression where
  prt i e = case e of
    IntExp intliteral -> prPrec i 0 (concatD [prt 0 intliteral])
    BoolExp boolliteral -> prPrec i 0 (concatD [prt 0 boolliteral])
    CharExpr charliteral -> prPrec i 0 (concatD [prt 0 charliteral])
    StringExpr stringliteral -> prPrec i 0 (concatD [prt 0 stringliteral])
    PairExpr pairliteral -> prPrec i 0 (concatD [prt 0 pairliteral])
    IdentExpr identifier -> prPrec i 0 (concatD [prt 0 identifier])
    ArrayExpr arrayelem -> prPrec i 0 (concatD [prt 0 arrayelem])
    UExpr unaryoperator expression -> prPrec i 0 (concatD [prt 0 unaryoperator, prt 0 expression])
    BExp expression1 binaryoperator expression2 -> prPrec i 0 (concatD [prt 0 expression1, prt 0 binaryoperator, prt 0 expression2])
    BracketExp lparent expression rparent -> prPrec i 0 (concatD [prt 0 lparent, prt 0 expression, prt 0 rparent])

instance Print UnaryOperator where
  prt i e = case e of
    UBang nott -> prPrec i 0 (concatD [prt 0 nott])
    UMinus minustoken -> prPrec i 0 (concatD [prt 0 minustoken])
    ULength lent -> prPrec i 0 (concatD [prt 0 lent])
    UOrd ordt -> prPrec i 0 (concatD [prt 0 ordt])
    UChr chrt -> prPrec i 0 (concatD [prt 0 chrt])

instance Print BinaryOperator where
  prt i e = case e of
    BTimes timest -> prPrec i 0 (concatD [prt 0 timest])
    BDivide dividet -> prPrec i 0 (concatD [prt 0 dividet])
    BModulus modulot -> prPrec i 0 (concatD [prt 0 modulot])
    BPlus plustoken -> prPrec i 0 (concatD [prt 0 plustoken])
    BMinus minustoken -> prPrec i 0 (concatD [prt 0 minustoken])
    BGreater greatert -> prPrec i 0 (concatD [prt 0 greatert])
    BLess lesst -> prPrec i 0 (concatD [prt 0 lesst])
    BGreaterEqual greatereqt -> prPrec i 0 (concatD [prt 0 greatereqt])
    BLessEqual lesseqt -> prPrec i 0 (concatD [prt 0 lesseqt])
    BEqual eqt -> prPrec i 0 (concatD [prt 0 eqt])
    BNotEqual noteqt -> prPrec i 0 (concatD [prt 0 noteqt])
    BAnd andt -> prPrec i 0 (concatD [prt 0 andt])
    BOr ort -> prPrec i 0 (concatD [prt 0 ort])

instance Print IntLiteral where
  prt i e = case e of
    IntPlus plustoken intdigit -> prPrec i 0 (concatD [prt 0 plustoken, prt 0 intdigit])
    IntMinus minustoken intdigit -> prPrec i 0 (concatD [prt 0 minustoken, prt 0 intdigit])
    IntLiteral intdigit -> prPrec i 0 (concatD [prt 0 intdigit])