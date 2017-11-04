{-# LANGUAGE FlexibleInstances #-}

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

wplus :: BaseType -> BaseType -> (Type, Type) 
wplus a b = (BaseType a, BaseType b)

newtype TypeID = TypeID String
  deriving (Eq)

instance Show TypeID where
  show (TypeID s) = s

data Scop a = Scop (a, [NewScope])

instance Show WaccType where
  show (a :-> b) = show a ++ (" -> ") ++ show b
  show (tid :=> b) = show tid ++ " -> " ++ show b
  show ((p, tid) :+> b) = show tid ++ " elem of " ++ show p ++ " -> " ++ show b
  show ((ArrayWaccType wt) ::> b) = "[" ++ show wt ++ "]" ++ " -> " ++ show b 
  show RetWT = ""

class Typeable a where
  getType :: a -> ErrorList Type

class WaccTypeable a where
  getWType :: a -> WaccType

instance WaccTypeable BinaryOperator where
  getWType (BTimes _)     = BaseType IntType      :-> BaseType IntType      :-> BaseType IntType  :-> RetWT
  getWType (BDivide _)    = BaseType IntType      :-> BaseType IntType      :-> BaseType IntType  :-> RetWT
  getWType (BModulus _)   = BaseType IntType     :-> BaseType IntType     :-> BaseType IntType  :-> RetWT
  getWType (BPlus _ _)    = BaseType IntType      :-> BaseType IntType      :-> BaseType IntType  :-> RetWT
  getWType (BMinus _ _)   = BaseType IntType      :-> BaseType IntType      :-> BaseType IntType  :-> RetWT
  getWType (BMore _)      = ((wplus IntType CharType), (TypeID "a")) :+> TypeID "a" :=> BaseType BoolType :-> RetWT
  getWType (BLess _)      = ((wplus IntType CharType), (TypeID "a")) :+> TypeID "a" :=> BaseType BoolType :-> RetWT
  getWType (BMoreEqual _) = ((wplus IntType CharType), (TypeID "a")) :+> TypeID "a" :=> BaseType BoolType :-> RetWT
  getWType (BLessEqual _) = ((wplus IntType CharType), (TypeID "a")) :+> TypeID "a" :=> BaseType BoolType :-> RetWT
  getWType (BEqual _)     = TypeID "a" :=> TypeID "a" :=> BaseType BoolType :-> RetWT
  getWType (BNotEqual _)  = TypeID "a" :=> TypeID "a" :=> BaseType BoolType :-> RetWT
  getWType (BAnd _)       = BaseType BoolType     :-> BaseType BoolType     :-> BaseType BoolType :-> RetWT
  getWType (BOr _)        = BaseType BoolType     :-> BaseType BoolType     :-> BaseType BoolType :-> RetWT

instance WaccTypeable UnaryOperator where
  getWType (UBang _)    = BaseType BoolType                     :-> BaseType BoolType :-> RetWT
  getWType (UMinus _ _) = BaseType IntType                      :-> BaseType IntType  :-> RetWT
  getWType (ULenght _)  = ArrayWaccType (TypeID "a" :=> RetWT)  ::> BaseType IntType  :-> RetWT
  getWType (UOrd _)     = BaseType CharType                     :-> BaseType IntType  :-> RetWT
  getWType (UChr _)     = BaseType IntType                      :-> BaseType CharType :-> RetWT

instance Typeable BaseType where
  getType a = getType (BaseType a)

instance Typeable Type where
  getType a = return a

instance WaccTypeable Type where
  getWType a = a :-> RetWT

instance Typeable (Scop AssignRhs) where
  getType (Scop ((AssignExp e _), scps)) = getType (Scop ((e, scps)))
  --getType (Scop ((AssignArrayLit (ArrayLiteral litElems _) _), scps)) = ArrayType (ArrayDeclarationLiteral (checkSameTypes (map (\(ArrayLiteralElem e _) -> getType (Scop (e, scps)))))
  getType (Scop ((AssignPair _ _ _), _)) = getType PairType
  getType (Scop ((AssignFunctionCall _ _ _), _)) = undefined
  getType (Scop ((AssignArrayLit (ArrayLiteral litElems _) _), scps)) = undefined
  getType (Scop ((AssignPairElem (PairFst e _) _), scps)) = getType (Scop (e, scps))
  getType (Scop ((AssignPairElem (PairSnd e _) _), scps)) = getType (Scop (e, scps))

instance Typeable Function where
  getType (Function t _ _ (sts, scp) pos) 
    = mapM_ (\e -> checkTypes (Scop (e, [scp])) t) returns >> getType t
    where
      returns = [e | (StatReturn e _) <- sts]

instance Typeable (Scop Expression) where
  getType (Scop ((IntExp _ _), _)) = getType IntType 
  getType (Scop ((BoolExp _ _), _)) = getType BoolType
  getType (Scop ((CharExpr _ _), _)) = getType CharType
  getType (Scop ((StringExpr _ _), _)) = getType StringType
  getType (Scop ((PairExpr _ _), _)) = getType PairType 
  getType (Scop ((IdentExpr i _), scp)) = lookupType i scp
  getType (Scop ((ArrayExpr (ArrayElem i as _) _), scp)) = mapM_ (\a -> handleArray i a scp) as >> lookupType i scp
  getType (Scop ((UExpr uop e _), scp)) = handleOperator uop [Scop (e, scp)]
  getType (Scop ((BExp e bop e' _), scp)) = handleOperator bop [Scop (e, scp), Scop (e', scp)]
  getType (Scop ((BracketExp e _), scp)) = getType (Scop (e, scp))

handleOperator :: (WaccTypeable a, Positionable a, Show a) => a -> [Scop Expression] -> ErrorList Type
handleOperator op sexprs = mapM getType sexprs >>= subType op

handleArray :: Identifier -> ArrayAccess -> [NewScope] -> ErrorList ()
handleArray i (ArrayAccess e _) scp = checkTypes (Scop (e, scp)) IntType

checkTypes :: (Typeable r) => Scop Expression -> r -> ErrorList ()
checkTypes given@(Scop (e, _)) required = do
  a <- getType given
  b <- getType required
  if (a == b)
    then return ()
    else expError b a e >> return ()

subType :: (WaccTypeable a, Positionable a, Show a) => a -> [Type] -> ErrorList Type
subType op ts = subType' (getWType op) ts [] op (getWType op) 0 (getPos op)

subType' :: Show a => WaccType -> [Type] -> [(String, Type)]-> a -> WaccType -> Int -> Position -> ErrorList Type
subType' (((w, w'), (TypeID s)) :+> ws) (t' : ts) _ _ _ _ _= undefined
subType' ((ArrayWaccType w) ::> ws) (t : ts) _ _ _ _ _ = undefined
subType' ((TypeID s) :=> ws) (t' : ts) tids op opW track pos = maybe (subType' ws ts tids op opW (succ track) pos) (\t -> if t == t' then subType' ws ts tids op opW (succ track) pos else subError t t' op opW track pos) (lookup s tids)
subType' (t :-> ws) (t' : ts) tids op opW track pos = if t == t' then subType' ws ts tids op opW (succ track) pos else subError t' t op opW track pos
subType' (t :-> RetWT) [] _ _ _ _ _ = return t
subType' ((TypeID s) :=> ws) [] tids op opW _ _ = maybe (fail ("Operator  " ++ show op ++ " doesn't have a concrete return type, in fact it has type " ++ show opW)) return (lookup s tids)
subType' _ [] _ _ _ _ _= fail "Not enough elements when attempting to do a type substitution"
subType' RetWT _ _ _ _ _ _ = fail "Too many elements when attempting to do a type substitution"

grabPairElem :: (Eq a) => a -> (a, a) -> Maybe a
grabPairElem a (b, c)
  | a == b = Just a
  | a == c = Just c
  | otherwise = Nothing

subError :: Show a => Type -> Type -> a -> WaccType -> Int -> Position -> ErrorList Type
subError target given op waccT track pos = throwTypeError target pos ("Operator: " ++ show op ++ " has type " ++ show waccT ++ " but encountered an error when subsituting argument " ++ show track ++ ". The operator requires a type of " ++ show target ++ " but was given a type of " ++ show given) 


lookupType :: Identifier -> [NewScope] -> ErrorList Type
lookupType i@(Identifier (_, name)) ((NewScope hmap) : scps) = maybe (lookupType i scps) return (M.lookup name hmap)
lookupType (Identifier (p, name)) [] = die AnalStage p ("Semantic Error: Variable " ++ name ++ " is used but never defined") 200

throwTypeError :: Type -> Position -> String -> ErrorList Type
throwTypeError t p str = throwError t (ErrorData FatalLevel TypeStage p ("Type Error: " ++ str) 200)

expError :: Type -> Type -> Expression -> ErrorList Type
expError target given e = 
  throwTypeError target (getExprPos e) ("Expression: " ++ show e ++ " has type " ++ show target ++ " but needs type " ++ show given)

getExprPos :: Expression -> Position
getExprPos (IntExp _ p) = p
getExprPos (BoolExp _ p) = p
getExprPos (CharExpr _ p) = p
getExprPos (StringExpr _ p) = p
getExprPos (PairExpr _ p) = p
getExprPos (IdentExpr _ p) = p
getExprPos (ArrayExpr _ p) = p
getExprPos (UExpr _ _ p) = p
getExprPos (BExp _ _ _ p) = p
getExprPos (BracketExp _ p) = p
