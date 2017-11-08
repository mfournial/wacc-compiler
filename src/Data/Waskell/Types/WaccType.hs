{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Waskell.Types.WaccType where

import Data.Waskell.ADT
import Data.Waskell.Types.Util

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

newtype ArrayWaccType = ArrayWaccType WaccType
  deriving (Eq)

newtype TypeID = TypeID String
  deriving (Eq)

instance Show TypeID where
  show (TypeID s) = s

class WaccTypeable a where
  getWType :: a -> WaccType


-- Instances

instance WaccTypeable StatementOperator where
  getWType (StatDecAss typ _ _)  = typ :-> typ :-> IOUnit :-> RetWT
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
  getWType BMore      = (wplus IntType CharType, TypeID "a")     :+> TypeID "a"            :=> liftType BoolType :-> RetWT
  getWType BLess      = (wplus IntType CharType, TypeID "a")     :+> TypeID "a"            :=> liftType BoolType :-> RetWT
  getWType BMoreEqual = (wplus IntType CharType, TypeID "a")     :+> TypeID "a"            :=> liftType BoolType :-> RetWT
  getWType BLessEqual = (wplus IntType CharType, TypeID "a")     :+> TypeID "a"            :=> liftType BoolType :-> RetWT
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

instance WaccTypeable Type where
  getWType a = a :-> RetWT

