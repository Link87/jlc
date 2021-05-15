{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Javalette.Check.TypedAST where

import qualified Data.String
import Data.Text (Text)

newtype TypedProg = Program [TopDef]

data TopDef
  = FnDef Type FnIdent [Arg] Blk TopDef
  | ClsDef ClsIdent [ClsItem] [ClsVar] [ClsMeth] TopDef

data Arg = Argument Type VarIdent

data ClsItem = InstVar Type VarIdent | MethDef Type FnIdent [Arg] Blk

data ClsVar = ClsVar ClsIdent Type VarIdent

data ClsMeth = ClsMeth ClsIdent FnIdent Type

newtype Blk = Block [Stmt]

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

data DeclItem = NoInit VarIdent | Init VarIdent TExpr

data LValue = ArrId LValue TExpr | Id VarIdent

data Type
  = Int
  | Double
  | Boolean
  | Void
  | Array Type
  | Object ClsIdent
  | Struct StructIdent
  | Ptr Type
  | Fn Type [Type]
  | String

data TExpr = TExpr Expr Type

data Expr
  = EVar VarIdent
  | ELitInt Integer
  | ELitDoub Double
  | ELitTrue
  | ELitFalse
  | ENull ClsIdent
  | ECall FnIdent [TExpr]
  | EString String
  | EArrIndex TExpr TExpr
  | ENeg TExpr
  | ENot TExpr
  | EMul TExpr MulOp TExpr
  | EAdd TExpr AddOp TExpr
  | ERel TExpr RelOp TExpr
  | EAnd TExpr TExpr
  | EOr TExpr TExpr
  | EObjInit Type
  | EArrAlloc Type [SizeItem]
  | EMethCall TExpr FnIdent [TExpr]
  | EArrLen TExpr

newtype SizeItem = SizeSpec TExpr

data AddOp = Plus | Minus

data MulOp = Times | Div | Mod

data RelOp = LTH | LE | GTH | GE | EQU | NE

class Ident t where
  ident :: t -> Text

newtype ClsIdent = ClsId Text
  deriving (Data.String.IsString)

instance Ident ClsIdent where
  ident (ClsId id) = id

newtype StructIdent = StructId Text
  deriving (Data.String.IsString)

instance Ident StructIdent where
  ident (StructId id) = id

newtype FnIdent = FnId Text
  deriving (Data.String.IsString)

instance Ident FnIdent where
  ident (FnId id) = id

newtype VarIdent = VarId Text
  deriving (Data.String.IsString)

instance Ident VarIdent where
  ident (VarId id) = id
