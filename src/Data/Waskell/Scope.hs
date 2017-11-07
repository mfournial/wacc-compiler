module Data.Waskell.Scope where

import Data.Waskell.ADT
import Data.Waskell.Error
import Data.Waskell.Types
import Control.Monad

import qualified Data.HashMap.Lazy as M

genSymbols :: WaccTree -> ErrorList WaccTree
genSymbols (WaccTree (Program fsp (sts, empty))) = do
  let (fs, ps) = unzip fsp
  ns <- foldM addFuncToScope emptyScope fs
  fs' <- mapM (\f -> genSymbolsF f ns) fs
  (retsts, retscp) <- fillScopeBlock (sts, empty) [ns]
  return (WaccTree (Program (zip fs' ps) (retsts, retscp)))

addFuncToScope :: NewScope -> Function -> ErrorList NewScope
addFuncToScope scp f@(Function _ i _ _) = extendScope i (Right f) scp

fillScopeBlock :: ScopeBlock -> [NewScope] -> ErrorList ScopeBlock
fillScopeBlock (sts, scp) parents = foldM (\x y -> addStmtToScope x y parents) ([], scp) sts 

addStmtToScope :: ScopeBlock -> Statement -> [NewScope] -> ErrorList ScopeBlock

addStmtToScope (sts, scp) s@(StatementOperator so@((StatDecAss t i r), _)) parents = do
  _ <- getType (Scop (so, scp : parents))
  scp' <- extendScope i (Left t) scp
  return (s : sts, scp')

addStmtToScope (sts, scp) s@(StatementOperator so) parents = do
  _ <- getType (Scop (so, scp : parents))
  return (s : sts, scp)

addStmtToScope (sts, scp) (StatIf (c, p) sb sb') parents = do
  b <- getType (Scop (c, scp : parents))
  _ <- if (b == liftType BoolType) then return b else throwTypeError (liftType BoolType) p ("conditional of if expression should have type bool, but infact has type " ++ show b)
  sbF <- fillScopeBlock sb (scp : parents)
  sbF' <- fillScopeBlock sb' (scp : parents)
  return ((StatIf (c, p) sbF sbF') : sts, scp)

addStmtToScope (sts, scp) (StatWhile (c, p) sb) parents = do
  b <- getType (Scop (c, scp : parents))
  _ <- if (b == liftType BoolType) then return b else throwTypeError (liftType BoolType) p ("conditional of while expression should have type bool, but infact has type " ++ show b)
  sb' <- fillScopeBlock sb (scp : parents)
  return ((StatWhile (c, p) sb') : sts, scp)

addStmtToScope (sts, scp) (StatScope sb) parents = do
  sb' <- fillScopeBlock sb (scp : parents)
  return ((StatScope sb') : sts, scp)

addStmtToScope sb StatSkip _ = return sb

genSymbolsF :: Function -> NewScope -> ErrorList Function
genSymbolsF (Function t i ps (sts, _)) scp = do
  ps' <- genParamScope ps
  sb' <- fillScopeBlock (sts, ps') [scp]
  t' <- getType (Function t i ps sb')
  return (Function t' i ps sb')

emptyScope :: NewScope
emptyScope = NewScope M.empty

extendScope :: Identifier -> Either Type Function -> NewScope -> ErrorList NewScope
extendScope (s,p) t m@(NewScope hmap) 
  | M.member s hmap = throwError m (ErrorData FatalLevel AnalStage p (" attempting to redifine already defined variable or function " ++ s) 200)
  | otherwise = return $ NewScope (M.insert s t hmap)

genParamScope :: [Parameter] -> ErrorList NewScope
genParamScope = foldM addParamToScope emptyScope

addParamToScope :: NewScope -> Parameter -> ErrorList NewScope
addParamToScope ns (Param t i) = extendScope i (Left t) ns

