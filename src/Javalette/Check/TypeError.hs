{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Trustworthy #-}

module Javalette.Check.TypeError (
    TypeResult (..),
    TypeError (..),
    pattern Ok,
    pattern Err,
)
where

import qualified Data.Text as T
import Javalette.Lang.Abs

-- | A result of the type checker. Is either a computed value or a `TypeError`.
type TypeResult a = Either TypeError a

pattern Ok :: b -> Either a b
pattern Ok a = Right a

pattern Err :: a -> Either a b
pattern Err msg = Left msg

-- | A kind of typing error that can occur in a program.
data TypeError
  = TypeMismatch Type Type
  | TypeMismatchOverloaded Type [Type]
  | ExpectedFnType Type
  | ExpectedArrType Type
  | ExpectedObjType Type
  | ExpectedLValue Expr
  | ArgumentMismatch
  | DuplicateVariable Ident
  | DuplicateFunction Ident
  | DuplicateInstanceVar Ident
  | DuplicateClass Ident
  | UndeclaredVar Ident
  | ClassNotFound Ident
  | MethodNotFound Ident
  | UnknownProperty Ident
  | InvalidAccessor Expr
  | MainNotFound
  | MainArguments
  | MainReturnType
  | NonReturningPath
  | EmptyProgram

instance Show TypeError where
  showsPrec _ (TypeMismatch got expected) =
    showString "Type mismatch in program: Got "
      . showType got
      . showString " but expected "
      . showType expected
      . showString "."
  showsPrec _ (TypeMismatchOverloaded got [expected]) =
    showString "Type mismatch in program: Got "
      . showType got
      . showString " but expected "
      . showType expected
      . showString "."
  showsPrec _ (TypeMismatchOverloaded got expected) =
    showString "Type mismatch in program: Got "
      . showType got
      . showString " but expected one of "
      . showTypes expected
      . showString "."
  showsPrec _ (ExpectedFnType got) =
    showString "Type Mismatch: Expected a function but got " . showType got . showString "."
  showsPrec _ (ExpectedArrType got) =
    showString "Type Mismatch: Expected an array but got " . showType got . showString "."
  showsPrec _ (ExpectedObjType got) =
    showString "Type Mismatch: Expected an object but got " . showType got . showString "."
  showsPrec _ (ExpectedLValue expr) =
    showString "Expected an lvalue but got expression " . shows expr . showString "."
  showsPrec _ ArgumentMismatch =
    showString "Type Mismatch: Function arguments do not match function definition."
  showsPrec _ (DuplicateVariable (Ident id)) =
    showString "Variable declared multiple times: " . shows id . showString "."
  showsPrec _ (DuplicateFunction (Ident id)) =
    showString "Duplicate identifier! Function name used before:" . shows id . showString "."
  showsPrec _ (DuplicateInstanceVar (Ident id)) =
    showString "Duplicate identifier! Instance variable name used before:" . shows id . showString "."
  showsPrec _ (DuplicateClass (Ident id)) =
    showString "Duplicate identifier! Class name used before:" . shows id . showString "."
  showsPrec _ (UndeclaredVar (Ident id)) =
    showString "Use of undeclared variable " . shows id . showString "."
  showsPrec _ (ClassNotFound (Ident id)) =
    showString "Class definition not found for class " . shows id . showString "."
  showsPrec _ (MethodNotFound (Ident id)) =
    showString "Method definition not found for method " . shows id . showString "."
  showsPrec _ (UnknownProperty (Ident id)) =
    showString "Property " . shows id . showString " not found."
  showsPrec _ (InvalidAccessor expr) =
    showString "Accessor is invalid: '" . shows expr . showString "'."
  showsPrec _ MainNotFound = showString "No function 'main' found."
  showsPrec _ MainArguments = showString "Function 'main' must not have arguments."
  showsPrec _ MainReturnType = showString "Function 'main' must return 'int'."
  showsPrec _ NonReturningPath =
    showString "Missing return statement: execution path without return found."
  showsPrec _ EmptyProgram = showString "No function or class definition found."

-- | The textual representation of a type. More readable than the derived
-- 'Show' instance of 'Type'.
showType :: Type -> ShowS
showType Int = showString "int"
showType Double = showString "double"
showType Boolean = showString "boolean"
showType Void = showString "void"
showType String = showString "String"
showType (Array typ) = showType typ . showString "[]" 
showType (Object (Ident id)) = showString $ T.unpack id
showType (Fn typ args) = showString "fn " . showType typ . showString "(" . showTypes args . showString ")"

-- | The textual representation of a list of types. Entries are separated by
-- commas.
showTypes :: [Type] -> ShowS
showTypes [] = showString ""
showTypes [typ] = showType typ
showTypes (typ:types) = showType typ . showString ", " . showTypes types
