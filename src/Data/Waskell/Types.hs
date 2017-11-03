{-# LANGUAGE FlexibleInstances #-}

module Data.Waskell.Types where
  
import Data.Waskell.ADT
import Data.Waskell.Error

import qualified Data.HashMap.Lazy as M

data WaccType = Type :-> WaccType | ForAllType :=> WaccType | RetWT Type | RetFA ForAllType
  deriving (Eq)

newtype ForAllType = ForAllType ()
  deriving (Eq)

data Scop a = Scop (a, [NewScope])

instance Show WaccType where
  show (a :-> b) = show a ++ (" -> ") ++ show b
  show (a :=> b) = "Any -> " ++ show b
  show (RetWT a) = show a 
  show (RetFA (ForAllType ())) = "Any"

class Typeable a where
  getType :: a -> ErrorList Type

class WaccTypeable a where
  getWType :: a -> WaccType

instance WaccTypeable BinaryOperator where
  getWType a = undefined

instance WaccTypeable UnaryOperator where
  getWType a = undefined

instance Typeable BaseType where
  getType a = getType (BaseType a)

instance Typeable Type where
  getType a = return a

instance WaccTypeable Type where
  getWType a = RetWT a

instance Typeable (Scop AssignRhs) where
  getType a = undefined

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
subType op ts = subType' (getWType op) ts op (getWType op) 0 (getPos op)

subType' :: Show a => WaccType -> [Type] -> a -> WaccType -> Int -> Position -> ErrorList Type
subType' (ForAllType () :=> ws) (t' : ts) op opW track pos = subType' ws ts op opW (succ track) pos
subType' (RetFA (ForAllType ())) (t' : []) _ _ _ _ = return t'
subType' (t :-> ws) (t' : ts) op opW track pos = if t == t' then subType' ws ts op opW (succ track) pos else subError t' t op opW track pos
subType' (RetWT t) (t' : []) op opW track pos = if t == t' then return t else subError t' t op opW track pos
subType' _ [] _ _ _ _= fail "Not enough elements when attempting to do a type substitution"
subType' (RetWT _) _ _ _ _ _ = fail "Too many elements when attempting to do a type substitution"
subType' (RetFA (ForAllType ())) _ _ _ _ _ = fail "Too many elements when attempting to do a type substitution"

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
