module Code.Generator.Runtime.Internal(
  label,
  RuntimeComponent(RC),
  RCID(..)
) where

import Code.Instructions
import Data.Maybe(fromJust)

data RuntimeComponent = RC RCID Instructions

instance Eq RuntimeComponent where
  (==) (RC rid _) (RC rid' _) = rid == rid'

data RCID = PrintStr
          | PrintInt
          | PrintChar
          | PrintBool
          | ReadInt
          | ReadChar
          | ThrowRuntimeErr
          | FreePair
          | PrintRef
          | ArrayCheck
          | Checkdbz
          deriving (Eq)

names :: [(RCID, String)]
names = [ (PrintStr, "runtime_print_string")
        , (PrintInt, "runtime_print_int")
        , (PrintChar, "runtime_print_char")
        , (PrintBool, "runtime_print_bool")
        , (PrintRef, "runtime_print_ref")
        , (ReadInt, "runtime_read_int")
        , (ReadChar, "runtime_read_char")
        , (ThrowRuntimeErr, "runtime_throw_err")
        , (FreePair, "runtime_free_pair")
        , (ArrayCheck, "runtime_array_check")
        , (Checkdbz, "runtime_check_division_by_zero")
        ]

label :: RCID -> String
label = fromJust . flip lookup names