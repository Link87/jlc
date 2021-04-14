module Javalette.Gen.LLVM.Instructions
  ( Instruction (..),
    Type(..),
    Const(..),
    Value(..),
    Arg(..),
    CmpOp(..),
    ElemOffset
  )
where

import Data.Text (Text)
import Javalette.Lang.Abs (Ident(Ident))
import qualified Javalette.Lang.Abs as J

newtype Label = Label Text

data Type = Ptr Type | Int Int | Doub | Arr Int Type | Void

data Const = IConst Int | DConst Double | VConst

data Value = LitVal Const | RefVal J.Ident

data Arg = Argument Type Value

data CmpOp = Eq | Ne | Sgt | Sge | Slt | Sle

type ElemOffset = (Type, Int)

data Instruction
  = FnDecl Type Ident [Type]
  | FnDef Type Ident [Arg]
  | EndFnDef
  | LabelDef Ident
  | StringDef Ident Text
  | Alloca Ident Type
  | Store Type Value Ident
  | Load Ident Type Ident
  | GetElementPtr Type Ident [ElemOffset]
  | Call Ident Ident [Arg]
  | VCall Ident [Arg]
  | Return Type Value
  | VReturn
  | ICompare CmpOp Ident Ident
  | FCompare CmpOp Ident Ident
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
