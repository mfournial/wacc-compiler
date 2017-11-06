{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Waskell.Types where
  
import Data.Waskell.ADT
import Data.Waskell.Error

import qualified Data.HashMap.Lazy as M

infixr :->
infixr :=>
infixr :+>
infixr ::>

data WaccType = Type :-> WaccType 
                         | TypeID :=> WaccType 
                         | ((Type, Type), TypeID) :+> WaccType 
                         | ArrayWaccType ::> WaccType 
                         | RetWT
  deriving (Eq)

newtype ArrayWaccType = ArrayWaccType WaccType
  deriving (Eq)

liftType :: BaseType -> Type
liftType = Pairable . BaseType

wplus :: BaseType -> BaseType -> (Type, Type) 
wplus a b = (liftType a, liftType b)

newtype TypeID = TypeID String
  deriving (Eq)

instance Show TypeID where
  show (TypeID s) = s

data Scop a = Scop (a, [NewScope])

instance Show WaccType where
  show (a :-> RetWT) = show a
  show (a :=> RetWT) = show a
  show ((p, tid) :+> RetWT) = show tid ++ " elem of " ++ show p
  show ((ArrayWaccType wt) ::> RetWT) = "[" ++ show wt ++ "]"
  show (a :-> b) = show a ++ (" -> ") ++ show b
  show (tid :=> b) = show tid ++ " -> " ++ show b
  show ((p, tid) :+> b) = show tid ++ " elem of " ++ show p ++ " -> " ++ show b
  show ((ArrayWaccType wt) ::> b) = "[" ++ show wt ++ "]" ++ " -> " ++ show b 
  show RetWT = ""

class Typeable a where
  getType :: a -> ErrorList Type

class WaccTypeable a where
  getWType :: a -> WaccType

instance WaccTypeable StatementOperator where
  getWType (StatDecAss typ _ _)  = typ :-> IOUnit :-> RetWT
  getWType (StatAss _ _)         = (TypeID "a") :=> (TypeID "a") :=> IOUnit :-> RetWT
  getWType (StatFree _)          = (TypeID "a") :=> IOUnit :-> RetWT
  getWType (StatRead _)          = ((wplus IntType CharType), (TypeID "a")) :+> IOUnit :-> RetWT
  getWType (StatReturn _)        = (TypeID "a") :=> IOUnit :-> RetWT 
  getWType (StatExit _)          = (liftType IntType) :-> IOUnit :-> RetWT
  getWType (StatPrint _)         = (TypeID "a") :=> IOUnit :-> RetWT 
  getWType (StatPrintLn _)       = (TypeID "a") :=> IOUnit :-> RetWT


instance WaccTypeable BinaryOperator where
  getWType BTimes     = liftType IntType                         :-> liftType IntType      :-> liftType IntType  :-> RetWT
  getWType BDivide    = liftType IntType                         :-> liftType IntType      :-> liftType IntType  :-> RetWT
  getWType BModulus   = liftType IntType                         :-> liftType IntType      :-> liftType IntType  :-> RetWT
  getWType BPlus      = liftType IntType                         :-> liftType IntType      :-> liftType IntType  :-> RetWT
  getWType BMinus     = liftType IntType                         :-> liftType IntType      :-> liftType IntType  :-> RetWT
  getWType BMore      = ((wplus IntType CharType), (TypeID "a")) :+> TypeID "a"            :=> liftType BoolType :-> RetWT
  getWType BLess      = ((wplus IntType CharType), (TypeID "a")) :+> TypeID "a"            :=> liftType BoolType :-> RetWT
  getWType BMoreEqual = ((wplus IntType CharType), (TypeID "a")) :+> TypeID "a"            :=> liftType BoolType :-> RetWT
  getWType BLessEqual = ((wplus IntType CharType), (TypeID "a")) :+> TypeID "a"            :=> liftType BoolType :-> RetWT
  getWType BEqual     = TypeID "a"                               :=> TypeID "a"            :=> liftType BoolType :-> RetWT
  getWType BNotEqual  = TypeID "a"                               :=> TypeID "a"            :=> liftType BoolType :-> RetWT
  getWType BAnd       = liftType BoolType                        :-> liftType BoolType     :-> liftType BoolType :-> RetWT
  getWType BOr        = liftType BoolType                        :-> liftType BoolType     :-> liftType BoolType :-> RetWT

instance WaccTypeable UnaryOperator where
  getWType UBang    = liftType BoolType                     :-> liftType BoolType :-> RetWT
  getWType UMinus   = liftType IntType                      :-> liftType IntType  :-> RetWT
  getWType ULength  = ArrayWaccType (TypeID "a" :=> RetWT)  ::> liftType IntType  :-> RetWT
  getWType UOrd     = liftType CharType                     :-> liftType IntType  :-> RetWT
  getWType UChr     = liftType IntType                      :-> liftType CharType :-> RetWT

instance (WaccTypeable a) => WaccTypeable (Pos a) where
  getWType (a, _) = getWType a

instance Typeable BaseType where
  getType a = getType (liftType a)

instance Typeable Pairable where
  getType a = return (Pairable a)

instance Typeable Type where
  getType a = return a

instance WaccTypeable Type where
  getWType a = a :-> RetWT


instance Typeable (Scop AssignRhs) where
  getType (Scop (AssignExp e, scps)) = getType (Scop (e, scps))
  --getType (Scop ((AssignArrayLit (ArrayLiteral litElems _) _), scps)) = ArrayType (ArrayDeclarationLiteral (checkSameTypes (map (\(ArrayLiteralElem e _) -> getType (Scop (e, scps)))))
  --getType (Scop ((AssignArrayLit (ArrayLiteral [])))) = ArrayType Nothing
  --getType (Scop ((AssignArrayLit (ArrayLiteral pe:pes)))) = undefined--(map (\e -> Scop (e, scps)) pes) 
  getType (Scop ((AssignPair _ _), _)) = undefined
  getType (Scop ((AssignFunctionCall _ _), _)) = undefined
  getType (Scop ((AssignArrayLit e), scps)) = undefined
  getType (Scop (AssignPairElem ((Left e), _),  scps)) = getType (Scop (e, scps))
  getType (Scop (AssignPairElem ((Right e), _), scps)) = getType (Scop (e, scps))

instance Typeable (Scop (Pos StatementOperator)) where
  getType (Scop (o@((StatDecAss typ _ arhs), pos), scp))  = do
                                                          trhs <- getType (Scop (arhs, scp))
                                                          subType o [typ, trhs]
  getType (Scop (o@((StatAss alhs arhs), pos), scp))      = do 
                                                          tlhs <- getType (Scop (alhs, scp))
                                                          trhs <- getType (Scop (arhs, scp))
                                                          subType o [tlhs, trhs]
  getType (Scop (o@((StatFree e), pos), scp))             = do 
                                                          exp <- getType  (Scop (e, scp)) 
                                                          subType o [exp]   
  getType (Scop (o@((StatRead alhs), pos), scp))          = do 
                                                          tlhs <- getType  (Scop (alhs, scp))
                                                          subType o [tlhs]
  getType (Scop (o@((StatReturn e), pos), scp))           = do 
                                                          exp <-  getType  (Scop (e, scp)) 
                                                          subType o [exp] 
  getType (Scop (o@((StatExit e), pos), scp))             = do
                                                          exp <- getType  (Scop (e, scp)) 
                                                          subType o [exp]
  getType (Scop (o@((StatPrint e), pos), scp))            = do 
                                                          exp <- getType  (Scop (e, scp))
                                                          subType o [exp]
  getType (Scop (o@((StatPrintLn e), pos), scp))          = do  
                                                          exp <- getType  (Scop (e, scp))
                                                          subType o [exp]


instance Typeable Function where
  getType (Function t _ _ (sts, scp)) 
    = mapM_ (\e -> checkTypes (Scop (e, [scp])) t) returns >> getType t
    where
      returns = [e | (StatementOperator (StatReturn e, _)) <- sts]

instance Typeable (Scop a) => Typeable (Scop (Pos a)) where
  getType (Scop ((a, _), scps)) = getType (Scop (a, scps))

instance Typeable (Scop Expression) where
  getType (Scop ((IntExp _), _)) = getType IntType 
  getType (Scop ((BoolExp _), _)) = getType BoolType
  getType (Scop ((CharExpr _), _)) = getType CharType
  getType (Scop ((StringExpr _), _)) = getType StringType
  getType (Scop (PairExpr, _)) = getType PairNull
  getType (Scop ((IdentExpr i), scp)) = lookupType i scp
  getType (Scop (ArrayExpr (ArrayElem i es, _), scp)) = mapM_ (\e -> handleArray i e scp) es >> lookupType i scp
  getType (Scop ((UExpr uop e), scp)) = handleOperator uop [Scop (e, scp)]
  getType (Scop ((BExp e bop e'), scp)) = handleOperator bop [Scop (e, scp), Scop (e', scp)]
  getType (Scop ((BracketExp e ), scp)) = getType (Scop (e, scp))

handleOperator :: (WaccTypeable a, Referenceable a) => Pos a -> [Scop (Pos Expression)] -> ErrorList Type
handleOperator op sexprs = mapM getType sexprs >>= subType op

handleArray :: Identifier -> Pos Expression -> [NewScope] -> ErrorList ()
handleArray i e scp = checkTypes (Scop (e, scp)) IntType

checkTypes :: (Typeable r) => Scop (Pos Expression) -> r -> ErrorList ()
checkTypes given@(Scop (e, _)) required = do
  a <- getType given
  b <- getType required
  if (a == b)
    then return ()
    else expError b a e >> return ()

subType :: (WaccTypeable a, Referenceable a) => Pos a -> [Type] -> ErrorList Type
subType op ts = do 
  (t, ts') <- subType' (getWType op) ts [] op (getWType op) 1 (getPos op)
  if ts' == [] then return t else fail "Too many elements when attempting to do a type substitution"

subType' :: Referenceable a => WaccType -> [Type] -> [(String, Type)]-> a -> WaccType -> Int -> Position -> ErrorList (Type, [Type])
subType' (((tw, tw'), (TypeID s)) :+> ws) (t' : ts) tids op opW track pos 
  | tw == t'  = subType' ws ts ((s, tw)  : tids) op opW (succ track) pos
  | tw' == t' = subType' ws ts ((s, tw') : tids) op opW (succ track) pos
  | otherwise = die TypeStage pos ("Type Error: " ++ getName op ++ ": has type (" ++ show opW ++ ") and in particular requires either a " ++ show tw ++ " or a " ++ show tw' ++ " in argument " ++ show track ++ " but was actually given a type " ++ show t') 200
subType' ((ArrayWaccType (TypeID _ :=> RetWT)) ::> ws) ((Pairable (ArrayType t)) : ts) tids op opW track pos = if (ws /= RetWT) then subType' ws ts tids op opW track pos else return (t, ts)
subType' (ArrayWaccType (t :-> RetWT) ::> ws) ((Pairable (ArrayType t')) : ts) tids op opW track pos = if t == t' 
  then
    if (ws /= RetWT) then subType' ws ts tids op opW track pos else return (t, ts)
  else
    if (ws /= RetWT) 
      then subType' ws ts tids op opW track pos >>= \res -> throwTypeError res pos ("Failed to match array types in argument " ++ show track ++ " of operator " ++ getName op ++ " with type " ++ show opW ++ ". We required [" ++ show t' ++ "] but were given [" ++ show t ++ "]")
      else die TypeStage pos ("Type Error: " ++ "Failed to match array types in argument " ++ show track ++ " of operator " ++ getName op ++ " with type " ++ show opW ++ ". We require [" ++ show t' ++ "] but were given [" ++ show t ++ "]") 200
subType' ((ArrayWaccType (t :-> RetWT)) ::> ws) (t' : ts) _ op opW track pos = if t == t' then return (t, ts) else subError ts (Pairable (ArrayType t)) t' op opW track pos
subType' (ArrayWaccType w ::> ws) (t' : ts) _ op opW track pos = die TypeStage pos ("Type Error: " ++ getName op ++ ": has type (" ++ show opW ++ ") and in particular requires an array-type in argument " ++ show track ++ " but was actually given a type " ++ show t') 200
subType' ((TypeID s) :=> RetWT) ts tids op opW _ _ = maybe (fail (getClass op ++ ": " ++ getName op ++ " doesn't have a concrete return type, in fact it has type (" ++ show opW ++ ")")) (\t -> return (t, ts)) (lookup s tids)
subType' ((TypeID s) :=> ws) (t' : ts) tids op opW track pos = maybe (subType' ws ts ((s, t') : tids) op opW (succ track) pos) (subTypeCheck t' ws ts tids op opW track pos) (lookup s tids)
subType' (t :-> ws) (t' : ts) tids op opW track pos = subTypeCheck t ws ts tids op opW track pos t'
subType' (t :-> RetWT) ts _ _ _ _ _ = return (t, ts)
subType' RetWT ts _ _ _ _ _ = fail "hope we don't get here (to reason about this later)"
subType' _ [] _ _ _ _ _= fail "Not enough elements when attempting to do a type substitution"

subTypeCheck :: Referenceable a => Type -> WaccType -> [Type] -> [(String, Type)] -> a -> WaccType -> Int -> Position -> Type -> ErrorList (Type, [Type])
subTypeCheck giv ws ts tids op opW track pos tar 
  = if tar == giv then subType' ws ts tids op opW (succ track) pos else subError ts giv tar op opW track pos

grabPairElem :: (Eq a) => a -> (a, a) -> Maybe a
grabPairElem a (b, c)
  | a == b = Just a
  | a == c = Just c
  | otherwise = Nothing

subError :: Referenceable a => [Type] -> Type -> Type -> a -> WaccType -> Int -> Position -> ErrorList (Type, [Type])
subError ts target given op waccT track pos = throwTypeError (target, ts) pos (getClass op ++ ": " ++ getName op ++ " has type (" ++ show waccT ++ ") but encountered an error when subsituting argument " ++ show track ++ ". The operator requires a type of " ++ show target ++ " but was given a type of " ++ show given) 


lookupType :: Identifier -> [NewScope] -> ErrorList Type
lookupType i@(name, _) ((NewScope hmap) : scps) = maybe (lookupType i scps) return (M.lookup name hmap)
lookupType (name, p) [] = die AnalStage p ("Semantic Error: Variable " ++ name ++ " is used but never defined") 200

throwTypeError :: a -> Position -> String -> ErrorList a
throwTypeError t p str = throwError t (ErrorData FatalLevel TypeStage p ("Type Error: " ++ str) 200)

expError :: Type -> Type -> Pos Expression -> ErrorList Type
expError target given e = 
  throwTypeError target (getPos e) ("Expression: " ++ show e ++ " has type " ++ show target ++ " but needs type " ++ show given)
