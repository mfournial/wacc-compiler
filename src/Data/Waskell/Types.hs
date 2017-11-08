{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Waskell.Types where

import Data.Waskell.ADT
import Data.Waskell.Error
import Data.Waskell.Types.WaccType
import Data.Waskell.Types.Util

import Control.Monad

import qualified Data.HashMap.Lazy as M


data Scop a = Scop (a, [NewScope])

class Typeable a where
  getType :: a -> ErrorList Type

instance Typeable BaseType where
  getType a = getType (liftType a)

instance Typeable Pairable where
  getType a = return (Pairable a)

instance Typeable Type where
  getType a = return a


getPairElemTypeL :: Scop Expression -> Position -> ErrorList Type
getPairElemTypeL s pos = do
  x <- getType s
  case x of
    (PairType t _) -> return t
    _ -> die TypeStage pos "fst called on non-pair expression" 200

getPairElemTypeR :: Scop Expression -> Position -> ErrorList Type
getPairElemTypeR s pos = do
  x <- getType s
  case x of
    (PairType _ t) -> return t
    _ -> die TypeStage pos "fst called on non-pair expression" 200

getArrayElemType :: Identifier -> [Pos Expression] -> Position -> [NewScope] -> ErrorList Type
getArrayElemType _ [] _ _ = fail "Parser error (empty array index) passed to semantic checker"
getArrayElemType i (e : exps) pos scp = do
  t <- lookupType i scp
  getType' (e : exps) t pos
  where
    getType' :: [Pos Expression] -> Type -> Position -> ErrorList Type
    getType' [] t  _ = return t
    getType' (e' : es') (Pairable (ArrayType t')) _ = do
      int <- getType (Scop (e', scp))
      _ <- if int /= liftType IntType then throwTypeError int pos "Attempt to index array with non integer expression" else return int
      getType' es' t' pos
    getType' (e' : []) (Pairable (BaseType StringType)) _ = do
      int <- getType (Scop (e', scp))
      _ <- if int /= liftType IntType then throwTypeError int pos "Attempt to index array with non integer expression" else return int
      return (liftType CharType)
    getType' _ _ pos' = die TypeStage pos' ("Attempted to index too far into array") 200

instance Typeable (Scop AssignLhs) where
  getType (Scop (AssignToIdent iden, scp)) =  lookupType iden scp
  getType (Scop (AssignToArrayElem (ArrayElem _ [], _), _)) = fail "Parser error (empty array index) passed to semantic checker"
  getType (Scop (AssignToArrayElem (ArrayElem i (e : exps), pos), scp)) = getArrayElemType i (e : exps) pos scp
  getType (Scop (AssignToPair (Left (e, pos), _),  scps)) = getPairElemTypeL (Scop (e, scps)) pos
  getType (Scop (AssignToPair (Right (e, pos), _),  scps)) = getPairElemTypeR (Scop (e, scps)) pos

instance Typeable (Scop AssignRhs) where
  getType (Scop (AssignExp e, scps)) = getType (Scop (e, scps))
  getType (Scop (AssignArrayLit (ArrayLiteral []), _)) = return (Pairable ArrayNull)
  getType (Scop (AssignArrayLit (ArrayLiteral (p : ps)), scp)) = do
    t <- getType (Scop (p, scp))
    _ <- foldM (checkSame scp) t ps
    return (Pairable (ArrayType t))

  getType (Scop ((AssignPair e e'), scp)) = PairType <$> getType (Scop (e, scp)) <*> getType (Scop (e', scp))

  getType (Scop ((AssignCall i@(sid, pid) exps), scp)) = do
    (Function ret _ ps _) <- lookupFunction i scp
    when (length ps /= length exps) $ throwTypeError () pid ("Function with identifier " ++ sid ++ " called with wrong number of arguments")
    _ <- sequence $ zipWith3 (checker ret) exps ps [1..]
    return ret
    where
      checker :: Type -> Pos Expression -> Parameter -> Int -> ErrorList Type
      checker ret ep@(_, pos) (Param t _) track
        | getType (Scop (ep, scp)) == getType t = return ret
        | otherwise = throwTypeError ret pos ("Type mismatch when attempting to call function " ++ sid ++ " argument " ++ show track ++ " requires a type of " ++ show t ++ " but was given a type of " ++ show (getType (Scop (ep, scp))))



  getType (Scop (AssignPairElem ((Left (e, pos)), _),  scps)) = getPairElemTypeL (Scop (e, scps)) pos
  getType (Scop (AssignPairElem ((Right (e, pos)), _),  scps)) = getPairElemTypeR (Scop (e, scps)) pos


instance {-# OVERLAPPING #-} Typeable (Scop (Pos StatementOperator)) where
  getType (Scop (o@((StatDecAss typ _ arhs), _), scp))  = do
                                                          trhs <- getType (Scop (arhs, scp))
                                                          subType o [typ, trhs]
  getType (Scop (o@((StatAss alhs arhs), _), scp))      = do 
                                                          tlhs <- getType (Scop (alhs, scp))
                                                          trhs <- getType (Scop (arhs, scp))
                                                          subType o [tlhs, trhs]
  --Ideally we would implement recursion on pairs in subType' but it seems like overengineering
  --just for this one example, so we hacked it instead.
  getType (Scop (((StatFree e), pos), scp))             = do 
                                                          expr <- getType  (Scop (e, scp)) 
                                                          case expr of
                                                            (Pairable (ArrayType _)) -> return IOUnit
                                                            (PairType _ _) -> return IOUnit
                                                            _ -> throwTypeError IOUnit pos "Attempting to free non pair/array type"

  getType (Scop (o@((StatRead alhs), _), scp))          = do 
                                                          tlhs <- getType  (Scop (alhs, scp))
                                                          subType o [tlhs]
  getType (Scop (o@((StatReturn e), _), scp))           = do 
                                                          expr <-  getType  (Scop (e, scp)) 
                                                          subType o [expr] 
  getType (Scop (o@((StatExit e), _), scp))             = do
                                                          expr <- getType  (Scop (e, scp)) 
                                                          subType o [expr]
  getType (Scop (o@((StatPrint e), _), scp))            = do 
                                                          expr <- getType  (Scop (e, scp))
                                                          subType o [expr]
  getType (Scop (o@((StatPrintLn e), _), scp))          = do  
                                                          expr <- getType  (Scop (e, scp))
                                                          subType o [expr]
checkSame :: [NewScope] -> Type -> Pos Expression -> ErrorList Type
checkSame scp a' (b, pb) = do
  b' <- getType (Scop (b, scp))
  if (a' == b') then return a' else throwTypeError a' pb ("Array literal declared with multiple element types, in particular we expect a " ++ show a' ++ " but got a " ++ show b')

instance Typeable Function where
  getType (Function t _ _ (sts, scp))
    = mapM_ (\e -> checkTypes (Scop (e, [scp])) t) returns >> getType t
    where
      returns = [e | (StatementOperator (StatReturn e, _)) <- sts]

instance {-# OVERLAPPABLE #-} Typeable (Scop a) => Typeable (Scop (Pos a)) where
  getType (Scop ((a, _), scps)) = getType (Scop (a, scps))

instance Typeable (Scop Expression) where
  getType (Scop ((IntExp _), _)) = getType IntType
  getType (Scop ((BoolExp _), _)) = getType BoolType
  getType (Scop ((CharExpr _), _)) = getType CharType
  getType (Scop ((StringExpr _), _)) = getType StringType
  getType (Scop (PairExpr, _)) = getType PairNull
  getType (Scop ((IdentExpr i), scp)) = lookupType i scp
  getType (Scop (ArrayExpr (ArrayElem i es, pos), scp)) = mapM_ (\e -> handleArray i e scp) es >> getArrayElemType i es pos scp
  getType (Scop ((UExpr uop e), scp)) = handleOperator uop [Scop (e, scp)]
  getType (Scop ((BExp e bop e'), scp)) = handleOperator bop [Scop (e, scp), Scop (e', scp)]
  getType (Scop ((BracketExp e ), scp)) = getType (Scop (e, scp))

handleOperator :: (WaccTypeable a, Referenceable a) => Pos a -> [Scop (Pos Expression)] -> ErrorList Type
handleOperator op sexprs = mapM getType sexprs >>= subType op

handleArray :: Identifier -> Pos Expression -> [NewScope] -> ErrorList ()
handleArray _ e scp = checkTypes (Scop (e, scp)) IntType

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


subType' ((ArrayWaccType (t :-> RetWT)) ::> _) (t' : ts) _ op opW track pos = if t == t' then return (t, ts) else subError ts (Pairable (ArrayType t)) t' op opW track pos


subType' (ArrayWaccType _ ::> _) (t' : _) _ op opW track pos = die TypeStage pos ("Type Error: " ++ getName op ++ ": has type (" ++ show opW ++ ") and in particular requires an array-type in argument " ++ show track ++ " but was actually given a type " ++ show t') 200


subType' ((TypeID s) :=> RetWT) ts tids op opW _ _ = maybe (fail (getClass op ++ ": " ++ getName op ++ " doesn't have a concrete return type, in fact it has type (" ++ show opW ++ ")")) (\t -> return (t, ts)) (lookup s tids)


subType' ((TypeID s) :=> ws) (t' : ts) tids op opW track pos = maybe (subType' ws ts ((s, t') : tids) op opW (succ track) pos) (subTypeCheck t' ws ts tids op opW track pos) (lookup s tids)

subType' (t :-> ws) (t' : ts) tids op opW track pos = subTypeCheck t' ws ts tids op opW track pos t

subType' (t :-> RetWT) ts _ _ _ _ _ = return (t, ts)

subType' RetWT _ _ _ _ _ _ = fail "hope we don't get here (to reason about this later)"

subType' _ [] _ _ _ _ _= fail "Not enough elements when attempting to do a type substitution"

subTypeCheck :: Referenceable a => Type -> WaccType -> [Type] -> [(String, Type)] -> a -> WaccType -> Int -> Position -> Type -> ErrorList (Type, [Type])
subTypeCheck giv ws ts tids op opW track pos tar
  = if tar == giv then subType' ws ts tids op opW (succ track) pos else subError ts tar giv op opW track pos >> subType' ws ts tids op opW (succ track) pos

grabPairElem :: (Eq a) => a -> (a, a) -> Maybe a
grabPairElem a (b, c)
  | a == b = Just a
  | a == c = Just c
  | otherwise = Nothing

subError :: Referenceable a => [Type] -> Type -> Type -> a -> WaccType -> Int -> Position -> ErrorList (Type, [Type])
subError ts target given op waccT track pos = throwTypeError (target, ts) pos (getClass op ++ ": " ++ getName op ++ " has type (" ++ show waccT ++ ") but encountered an error when subsituting argument " ++ show track ++ ". The operator requires a type of " ++ show target ++ " but was given a type of " ++ show given)


lookupType :: Identifier -> [NewScope] -> ErrorList Type
lookupType i@(name, p) ((NewScope hmap) : scps) = maybe (lookupType i scps) (either return (\_ -> die AnalStage p "Attempted to treat function as first class object" 200)) (M.lookup name hmap)
lookupType (name, p) [] = die AnalStage p ("Semantic Error: Variable " ++ name ++ " is used but never defined") 200

lookupFunction :: Identifier -> [NewScope] -> ErrorList Function
lookupFunction i@(name, p) ((NewScope hmap) : scps) = maybe (lookupFunction i scps) (either (\_ -> die AnalStage p "Attempted to treat value as function" 200) return) (M.lookup name hmap)
lookupFunction (name, p) [] = die AnalStage p ("Semantic Error: Variable " ++ name ++ " is used but never defined") 200

throwTypeError :: a -> Position -> String -> ErrorList a
throwTypeError t p str = throwError t (ErrorData FatalLevel TypeStage p ("Type Error: " ++ str) 200)

expError :: Type -> Type -> Pos Expression -> ErrorList Type
expError target given e =
  throwTypeError target (getPos e) ("Expression: " ++ show e ++ " has type " ++ show target ++ " but needs type " ++ show given)
