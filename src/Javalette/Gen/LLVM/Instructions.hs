module Javalette.Gen.LLVM.Instructions
  ( Instruction (..),
    Type(..),
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

data Arg = Argument Type J.Ident

data Const = IConst Int | DConst Double

data CmpOp = Eq | Ne | Sgt | Sge | Slt | Sle

type ElemOffset = (Type, Int)

data Instruction
  = FnDecl Type Ident [Type]
  | FnDef Type Ident [Arg]
  | EndFnDef
  | LabelDef Ident
  | StringDef Ident Text
  | Alloc Ident Type
  | Store Type Ident Ident
  | CStore Type Const Ident
  | Load Ident Type Ident
  | GetElementPtr Type Ident [ElemOffset]
  | Call Ident [Arg]
  | VCall Ident [Arg]
  | Return Type Ident
  | VReturn
  | ICompare CmpOp Ident Ident
  | FCompare CmpOp Ident Ident
  | Branch Ident Label Label
  | Unreachable
