{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_HADDOCK prune, ignore-exports, show-extensions #-}

-- | Type errors thrown by the type checker.
module Javalette.Check.TypeError
  ( TypeResult (..),
    TypeError (..),
    pattern Ok,
    pattern Err,
  )
where

import qualified Data.Text as T
import Javalette.Check.TypedAST (FnIdent, Type, TypeIdent, VarIdent)
import qualified Javalette.Lang.Abs as J

-- | A result of the type checker. Is either a computed value or a `TypeError`.
type TypeResult a = Either TypeError a

-- | Successful result.
pattern Ok :: b -> Either a b
pattern Ok a = Right a

-- | Unsuccessful result.
pattern Err :: a -> Either a b
pattern Err msg = Left msg

-- | A kind of typing error that can occur in a program.
data TypeError
  = TypeMismatch Type Type
  | TypeMismatchOverloaded Type [Type]
  | ExpectedFnType Type
  | ExpectedArrType Type
  | ExpectedStrPtrType Type
  | ExpectedEnumType Type
  | ExpectedObjType Type
  | ExpectedComplexType Type
  | ExpectedLValue J.Expr
  | ArgumentMismatch
  | DuplicateDefinition TypeIdent
  | DuplicateVariable VarIdent
  | DuplicateFunction FnIdent
  | DuplicateField VarIdent TypeIdent
  | DuplicateMethod FnIdent TypeIdent
  | DuplicateInstanceVar VarIdent TypeIdent
  | UndeclaredVar VarIdent
  | FunctionNotFound FnIdent
  | TypeNotFound TypeIdent
  | StructNotFound TypeIdent
  | FieldNotFound VarIdent TypeIdent
  | ClassNotFound TypeIdent
  | MethodNotFound FnIdent TypeIdent
  | InvalidAlias TypeIdent
  | UnknownProperty VarIdent
  | InvalidAccessor Type
  | InvalidNullPtr Type
  | MainNotFound
  | MainArguments
  | MainReturnType
  | NonReturningPath
  | EmptyProgram

instance Show TypeError where
  showsPrec _ (TypeMismatch got expected) =
    showString "Type mismatch in program: Got "
      . shows got
      . showString " but expected "
      . shows expected
      . showString "."
  showsPrec _ (TypeMismatchOverloaded got [expected]) =
    showString "Type mismatch in program: Got "
      . shows got
      . showString " but expected "
      . shows expected
      . showString "."
  showsPrec _ (TypeMismatchOverloaded got expected) =
    showString "Type mismatch in program: Got "
      . shows got
      . showString " but expected one of "
      . showTypes expected
      . showString "."
  showsPrec _ (ExpectedFnType got) =
    showString "Type Mismatch: Expected a function but got " . shows got . showString "."
  showsPrec _ (ExpectedArrType got) =
    showString "Type Mismatch: Expected an array but got " . shows got . showString "."
  showsPrec _ (ExpectedStrPtrType got) =
    showString "Type Mismatch: Expected a pointer to a struct but got " . shows got . showString "."
  showsPrec _ (ExpectedEnumType got) =
    showString "Type Mismatch: Expected an enum but got " . shows got . showString "."
  showsPrec _ (ExpectedObjType got) =
    showString "Type Mismatch: Expected an object but got " . shows got . showString "."
  showsPrec _ (ExpectedComplexType got) =
    showString "Type Mismatch: Expected a struct or class but got " . shows got . showString "."
  showsPrec _ (ExpectedLValue expr) =
    showString "Expected an lvalue but got expression " . shows expr . showString "."
  showsPrec _ ArgumentMismatch =
    showString "Type Mismatch: Function arguments do not match function definition."
  showsPrec _ (DuplicateDefinition id) =
    showString "Ambiguous identifier: " . shows id . showString " used more than once."
  showsPrec _ (DuplicateVariable id) =
    showString "Variable declared multiple times: " . shows id . showString "."
  showsPrec _ (DuplicateFunction id) =
    showString "Duplicate identifier. Function name used before:" . shows id . showString "."
  showsPrec _ (DuplicateField fldId strId) =
    showString "Duplicate field: Identifier " . shows fldId . showString " used before in type " . shows strId . showString "."
  showsPrec _ (DuplicateMethod fnId clsId) =
    showString "Duplicate method: Identifier " . shows fnId . showString " used before in class " . shows clsId . showString "."
  showsPrec _ (DuplicateInstanceVar varId clsId) =
    showString "Duplicate instance variable: Identifier " . shows varId . showString " used before in class " . shows clsId . showString "."
  showsPrec _ (UndeclaredVar id) =
    showString "Use of undeclared variable " . shows id . showString "."
  showsPrec _ (FunctionNotFound id) =
    showString "Function definition not found for function " . shows id . showString "."
  showsPrec _ (TypeNotFound id) =
    showString "Declaration for type " . shows id . showString " not found."
  showsPrec _ (StructNotFound id) =
    showString "Struct definition not found for struct " . shows id . showString "."
  showsPrec _ (FieldNotFound id typeId) =
    showString "Definition for field " . shows id . showString " not found in type " . shows typeId . showString "."
  showsPrec _ (ClassNotFound id) =
    showString "Class definition not found for class " . shows id . showString "."
  showsPrec _ (MethodNotFound id clsId) =
    showString "Method definition not found for method " . shows id . showString " in class " . shows clsId . showString "."
  showsPrec _ (InvalidAlias id) =
    showString "Type alias " . shows id . showString " does not alias a known type."
  showsPrec _ (UnknownProperty id) =
    showString "Property " . shows id . showString " not found."
  showsPrec _ (InvalidAccessor typ) =
    showString "Accessor for type " . shows typ . showString " does not exist."
  showsPrec _ (InvalidNullPtr typ) =
    showString "Cannot take null pointer of type " . shows typ . showString "'."
  showsPrec _ MainNotFound = showString "No function 'main' found."
  showsPrec _ MainArguments = showString "Function 'main' must not have arguments."
  showsPrec _ MainReturnType = showString "Function 'main' must return 'int'."
  showsPrec _ NonReturningPath =
    showString "Missing return statement: execution path without return found."
  showsPrec _ EmptyProgram = showString "No function or class definition found."

-- | The textual representation of a list of types. Entries are separated by
-- commas.
showTypes :: [Type] -> ShowS
showTypes [] = showString ""
showTypes [typ] = shows typ
showTypes (typ : types) = shows typ . showString ", " . showTypes types
