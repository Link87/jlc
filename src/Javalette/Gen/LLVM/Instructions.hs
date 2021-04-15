-- {-# LANGUAGE DataKinds #-}
module Javalette.Gen.LLVM.Instructions
  ( Instruction (..),
    Type (..),
    Value (..),
    Arg (..),
    CmpOp (..),
    ElemOffset(..),
  )
where

import Data.Text (Text)
import Javalette.Lang.Abs (Ident (Ident))
import qualified Javalette.Lang.Abs as J

newtype Label = Label Text

data Type
  = Ptr Type
  | Int Int
  | Doub
  | Arr Int Type
  | Void

data Value
  = Loc J.Ident
  | Glob J.Ident
  | IConst Int
  | DConst Double
  | BConst Bool
  | SConst Text
  | VConst

data Arg = Argument Type Value

data CmpOp = Eq | Ne | Sgt | Sge | Slt | Sle

data ElemOffset = Offset Type Int

data Instruction
  = FnDecl Type Ident [Type]
  | FnDef Type Ident [Arg]
  | EndFnDef
  | LabelDef Ident
  | StringDef Ident Type Value
  | Alloca Ident Type
  | Store Type Value Ident
  | Load Ident Type Ident
  | GetElementPtr Ident Type Value [ElemOffset]
  | Call Ident Ident [Arg]
  | VCall Ident [Arg]
  | Return Type Value
  | VReturn
  | ICompare CmpOp Value Value
  | FCompare CmpOp Value Value
  | Branch Ident Label Label
  | Unreachable
  | Add Ident Type Value Value
  | Sub Ident Type Value Value
  | Mul Ident Type Value Value
  | SDiv Ident Type Value Value
  | SRem Ident Type Value Value
  | FAdd Ident Type Value Value
  | FSub Ident Type Value Value
  | FMul Ident Type Value Value
  | FDiv Ident Type Value Value
  | FNeg Ident Type Value
  | Blank
