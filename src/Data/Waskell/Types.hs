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
  getType (Scop ((AssignPair _ _), _)) = undefined
  getType (Scop ((AssignFunctionCall _ _), _)) = undefined
  getType (Scop ((AssignArrayLit e), scps)) = undefined
  getType (Scop (AssignPairElem (Left e),  scps)) = getType (Scop (e, scps))
  getType (Scop (AssignPairElem (Right e), scps)) = getType (Scop (e, scps))

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

handleOperator :: (WaccTypeable a, Show a) => Pos a -> [Scop (Pos Expression)] -> ErrorList Type
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

subType :: (WaccTypeable a, Show a) => Pos a -> [Type] -> ErrorList Type
subType op ts = subType' (getWType op) ts [] op (getWType op) 0 (getPos op)

subType' :: Show a => WaccType -> [Type] -> [(String, Type)]-> a -> WaccType -> Int -> Position -> ErrorList Type
subType' (((w, w'), (TypeID s)) :+> ws) (t' : ts) _ _ _ _ _= undefined
subType' ((ArrayWaccType w) ::> ws) (t : ts) _ _ _ _ _ = undefined
subType' ((TypeID s) :=> ws) (t' : ts) tids op opW track pos = maybe (subType' ws ts tids op opW (succ track) pos) 
                                                                     (\t -> if t == t' then subType' ws ts tids op opW (succ track) pos else subError t t' op opW track pos) (lookup s tids)
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
lookupType i@(name, _) ((NewScope hmap) : scps) = maybe (lookupType i scps) return (M.lookup name hmap)
lookupType (name, p) [] = die AnalStage p ("Semantic Error: Variable " ++ name ++ " is used but never defined") 200

throwTypeError :: Type -> Position -> String -> ErrorList Type
throwTypeError t p str = throwError t (ErrorData FatalLevel TypeStage p ("Type Error: " ++ str) 200)

expError :: Type -> Type -> Pos Expression -> ErrorList Type
expError target given e = 
  throwTypeError target (getPos e) ("Expression: " ++ show e ++ " has type " ++ show target ++ " but needs type " ++ show given)
