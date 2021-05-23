{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The Typed AST for Javalette. Contains type annotations and expression
-- specialisations in comparison to the untyped AST.
module Javalette.Check.TypedAST where

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T

-- | A Typed Javalette AST.
newtype TypedProg = Program [TopDef]
  deriving (Show)

-- | A top-level definition.
data TopDef
  = FnDef Type FnIdent [Param] Blk
  | StrDef TypeIdent [StrField]
  | EnumDef TypeIdent [EnumField]
  | ClsDef TypeIdent [ClsItem] [ClsVar] [ClsMeth]
  deriving (Show)

-- | A function parameter.
data Param = Parameter Type VarIdent
  deriving (Show)

-- | A Struct field.
data StrField = StrFld Type VarIdent
  deriving (Show)

newtype EnumField = EnumFld VarIdent
  deriving (Show)

-- | A class member.
data ClsItem = InstVar Type VarIdent | MethDef Type FnIdent [Param] Blk
  deriving (Show)

-- | An entry in a class descriptor for an instance variable.
data ClsVar = ClsVar TypeIdent Type VarIdent
  deriving (Show)

-- | An entry in a class descriptor for a method
data ClsMeth = ClsMeth TypeIdent FnIdent Type
  deriving (Show)

-- | A statement block.
newtype Blk = Block [Stmt]
  deriving (Show)

-- | A single statement.
data Stmt
  = Empty
  | BStmt Blk
  | Decl Type [DeclItem]
  | Ass LValue TExpr
  | Incr LValue
  | Decr LValue
  | Ret TExpr
  | VRet
  | Cond TExpr Stmt
  | CondElse TExpr Stmt Stmt
  | While TExpr Stmt
  | ForEach Type VarIdent TExpr Stmt
  | SExpr TExpr
  deriving (Show)

-- | A variable declaration. May contain an expression for initialisation.
data DeclItem = NoInit VarIdent | Init VarIdent TExpr
  deriving (Show)

-- | An lvalue, i.e. a special kind of expression that a value can be assigned
-- to.
data LValue
  = VarVal VarIdent Type
  | ArrVal LValue TExpr Type
  | DerefVal LValue VarIdent Type
  deriving (Show)

-- | A type in Javalette.
data Type
  = Int
  | Double
  | Boolean
  | String
  | Void
  | Array Type
  | Struct TypeIdent
  | Enum TypeIdent
  | Object TypeIdent
  | Ptr Type
  | Fn Type [Type]
  | Type Type
  deriving (Eq)

instance Show Type where
  showsPrec _ Int = showString "int"
  showsPrec _ Double = showString "double"
  showsPrec _ Boolean = showString "boolean"
  showsPrec _ String = showString "String"
  showsPrec _ Void = showString "void"
  showsPrec _ (Array typ) = shows typ . showString "[]"
  showsPrec _ (Struct id) = shows id
  showsPrec _ (Enum id) = shows id
  showsPrec _ (Object id) = shows id
  showsPrec _ (Ptr id) = showString "*" . shows id
  showsPrec _ (Fn typ args) = showString "fn " . shows typ . showString "(" . shows args . showString ")"
  showsPrec _ (Type typ) = showString "type " . shows typ

-- | A typed expression.
data TExpr
  = EVar VarIdent Type
  | ELitInt Integer
  | ELitDouble Double
  | ELitTrue
  | ELitFalse
  | EString String
  | ENull TypeIdent Type
  | ECall FnIdent [TExpr] Type
  | ENeg TExpr Type
  | ENot TExpr
  | EMul TExpr MulOp TExpr Type
  | EAdd TExpr AddOp TExpr Type
  | ERel TExpr RelOp TExpr
  | EAnd TExpr TExpr
  | EOr TExpr TExpr
  | EArrAlloc Type [SizeItem] Type
  | EArrLen TExpr
  | EArrIndex TExpr TExpr Type
  | EStrInit TypeIdent
  | EDeref TExpr VarIdent Type
  | EEnum TypeIdent VarIdent
  | EObjInit TypeIdent
  | EMethCall TExpr TypeIdent FnIdent [TExpr] Type Type
  | EType Type
  deriving (Show)

-- | A size specification in the declaration of an array
newtype SizeItem = SizeSpec TExpr
  deriving (Show)

-- | An operation based on addition.
data AddOp = Plus | Minus
  deriving (Show, Eq)

-- | An operation based on multiplication
data MulOp = Times | Div | Mod
  deriving (Show, Eq)

-- | An ordering relation.
data RelOp = LTH | LE | GTH | GE | EQU | NE
  deriving (Show, Eq)

-- | An identifier in Javalette.
class Ident t where
  ident :: t -> Text

-- | An identifier that belongs to a named type.
newtype TypeIdent = TyId Text
  deriving (Eq, Ord, Data.String.IsString)

instance Ident TypeIdent where
  ident (TyId id) = id

instance Show TypeIdent where
  show (TyId id) = T.unpack id

-- | An identifier that belongs to a function or method.
newtype FnIdent = FnId Text
  deriving (Eq, Ord, Data.String.IsString)

instance Ident FnIdent where
  ident (FnId id) = id

instance Show FnIdent where
  show (FnId id) = T.unpack id

-- | An identifier that belongs to a variable.
newtype VarIdent = VarId Text
  deriving (Eq, Ord, Data.String.IsString)

instance Ident VarIdent where
  ident (VarId id) = id

instance Show VarIdent where
  show (VarId id) = T.unpack id

-- | Obtain the type of a typed expression.
getType :: TExpr -> Type
getType (EVar _ typ) = typ
getType ELitInt {} = Int
getType ELitDouble {} = Double
getType ELitTrue = Boolean
getType ELitFalse = Boolean
getType EString {} = String
getType (ENull _ typ) = typ
getType (ECall _ _ typ) = typ
getType (ENeg _ typ) = typ
getType ENot {} = Boolean
getType (EMul _ _ _ typ) = typ
getType (EAdd _ _ _ typ) = typ
getType ERel {} = Boolean
getType EAnd {} = Boolean
getType EOr {} = Boolean
getType (EArrAlloc _ _ typ) = typ
getType EArrLen {} = Int
getType (EArrIndex _ _ typ) = typ
getType (EStrInit id ) = Ptr $ Struct id
getType (EDeref _ _ typ) = typ
getType (EEnum id _) = Enum id
getType (EObjInit id) = Object id
getType (EMethCall _ _ _ _ _ typ) = typ
getType (EType typ) = Type typ

-- | Obtain the type of an lvalue.
getLValType :: LValue -> Type
getLValType (ArrVal _ _ typ) = typ
getLValType (DerefVal _ _ typ) = typ
getLValType (VarVal _ typ) = typ
