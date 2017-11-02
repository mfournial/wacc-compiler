module Data.Waskell.Scope where

import Data.Waskell.ADT
import Data.Waskell.Error (ErrorList)

import qualified Data.HashMap.Lazy as M

genSymbols :: WaccTree -> ErrorList WaccTree
genSymbols (WaccTree (Program fs sb pos)) = do
  fs' <- mapM genSymbolsF fs
  (sts, scp) <- fillScopeBlock sb
  scp' <- foldr addFuncToScope (pure scp) fs
  return (WaccTree (Program fs' (sts, scp') pos))

addFuncToScope :: Function -> ErrorList NewScope -> ErrorList NewScope
addFuncToScope f@(Function _ i _ _ _) escp = extendScope i <$> getFuncType f <*> escp

fillScopeBlock :: ScopeBlock -> ErrorList ScopeBlock
fillScopeBlock (sts, scp) = foldr addStmtToScope (pure ([], scp)) sts 

addStmtToScope :: Statement -> ErrorList ScopeBlock -> ErrorList ScopeBlock

addStmtToScope s@(StatAss (AssignToIdent i _) r _) esb = do
  (sts, scp) <- esb
  typ <- getRhsType r
  return (s : sts, extendScope i typ scp)

addStmtToScope (StatIf c sb sb' p) esb = do
  (sts, scp) <- esb
  sbF <- fillScopeBlock sb
  sbF' <- fillScopeBlock sb'
  return ((StatIf c sbF sbF' p) : sts, scp)

addStmtToScope (StatWhile c sb p) esb = do
  (sts, scp) <- esb
  sb' <- fillScopeBlock sb
  return ((StatWhile c sb' p) : sts, scp)

addStmtToScope (StatScope sb p) esb = do
  (sts, scp) <- esb
  sb' <- fillScopeBlock sb
  return ((StatScope sb' p) : sts, scp)

addStmtToScope s esb = do
  (sts, scp) <- esb
  return (s : sts, scp)

genSymbolsF :: Function -> ErrorList Function
genSymbolsF (Function t i ps sb pos) = do
  sb' <- fillScopeBlock sb
  return (Function t i ps sb' pos)

emptyScope :: NewScope
emptyScope = NewScope M.empty

extendScope :: Identifier -> Type -> NewScope -> NewScope
extendScope (Identifier (_,s)) t (NewScope hmap) = NewScope (M.insert s t hmap)

getRhsType :: AssignRhs -> ErrorList Type
getRhsType = undefined

getFuncType :: Function -> ErrorList Type
getFuncType = undefined

getExprType :: Expression -> ErrorList Type
getExprType = undefined
