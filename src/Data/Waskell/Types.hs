{-# LANGUAGE FlexibleInstances #-}

module Data.Waskell.Types where
  
import Data.Waskell.ADT
import Data.Waskell.Error

data WaccType = WaccType :=> WaccType | RetWT Type
  deriving (Eq)

data Scop a = Scop (a, [NewScope])

instance Show WaccType where
  show (a :=> b) = show a ++ (" -> ") ++ show b
  show (RetWT a) = show a 

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

handleOperator :: WaccTypeable a => a -> [Scop Expression] -> ErrorList Type
handleOperator op sexprs = mapM getType sexprs >>= subType (getWType op)

handleArray :: Identifier -> ArrayAccess -> [NewScope] -> ErrorList ()
handleArray i (ArrayAccess e _) scp = checkTypes (Scop (e, scp)) IntType

checkTypes :: (Typeable r) => Scop Expression -> r -> ErrorList ()
checkTypes given@(Scop (e, _)) required = do
  a <- getType given
  b <- getType required
  if (a == b)
    then return ()
    else expError b a e >> return ()

subType :: WaccType -> [Type] -> ErrorList Type
subType = undefined

lookupType :: Identifier -> [NewScope] -> ErrorList Type
lookupType = undefined

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
