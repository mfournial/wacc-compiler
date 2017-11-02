module Data.Waskell.Scope where

import Data.Waskell.ADT
import Data.Waskell.Error (ErrorList)
import Waskell.TypeCheck

import qualified Data.HashMap.Lazy as M

genSymbols :: WaccTree -> ErrorList WaccTree
genSymbols (WaccTree (Program fs sb pos)) = do
  fs' <- mapM genSymbolsF fs
  (sts, scp) <- fillScopeBlock sb []
  scp' <- foldr addFuncToScope (pure scp) fs
  return (WaccTree (Program fs' (sts, scp') pos))

addFuncToScope :: Function -> ErrorList NewScope -> ErrorList NewScope
addFuncToScope f@(Function _ i _ _ _) escp = extendScope i <$> getFuncType f <*> escp

fillScopeBlock :: ScopeBlock -> [NewScope] -> ErrorList ScopeBlock
fillScopeBlock (sts, scp) parents = foldr (\x y -> addStmtToScope x y parents) (pure ([], scp)) sts 

addStmtToScope :: Statement -> ErrorList ScopeBlock -> [NewScope] -> ErrorList ScopeBlock

addStmtToScope s@(StatAss (AssignToIdent i _) r _) esb parents = do
  (sts, scp) <- esb
  typ <- getRhsType r (scp : parents)
  return (s : sts, extendScope i typ scp)

addStmtToScope (StatIf c sb sb' p) esb parents = do
  (sts, scp) <- esb
  sbF <- fillScopeBlock sb (scp : parents)
  sbF' <- fillScopeBlock sb' (scp : parents)
  return ((StatIf c sbF sbF' p) : sts, scp)

addStmtToScope (StatWhile c sb p) esb parents = do
  (sts, scp) <- esb
  sb' <- fillScopeBlock sb (scp : parents)
  return ((StatWhile c sb' p) : sts, scp)

addStmtToScope (StatScope sb p) esb parents = do
  (sts, scp) <- esb
  sb' <- fillScopeBlock sb (scp : parents)
  return ((StatScope sb' p) : sts, scp)

addStmtToScope s esb _ = do
  (sts, scp) <- esb
  return (s : sts, scp)

genSymbolsF :: Function -> ErrorList Function
genSymbolsF (Function t i ps sb pos) = do
  sb' <- fillScopeBlock sb [genParamScope ps]
  return (Function t i ps sb' pos)

emptyScope :: NewScope
emptyScope = NewScope M.empty

extendScope :: Identifier -> Type -> NewScope -> NewScope
extendScope (Identifier (_,s)) t (NewScope hmap) = NewScope (M.insert s t hmap)

genParamScope :: [Parameter] -> NewScope
genParamScope = foldr addParamToScope emptyScope

addParamToScope :: Parameter -> NewScope -> NewScope
addParamToScope (Param t i _) = extendScope i t
