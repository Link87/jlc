{-# OPTIONS_HADDOCK prune, ignore-exports, show-extensions #-}

module Javalette.Gen.LLVM.Instructions
  ( Instruction (..),
    Type (..),
    Value (..),
    Arg (..),
    RelOp (..),
    FRelOp (..),
    ElemOffset (..),
    PhiElem (..),
  )
where

import Data.Text (Text)
import Javalette.Lang.Abs (Ident (Ident))
import qualified Javalette.Lang.Abs as J

-- | Possible types for values in LLVM.
data Type
  = -- | A pointer to a value of another type.
    Ptr Type
  | -- | An integer value with a certain number of bits.
    Int Int
  | -- | A LLVM @double@ value.
    Doub
  | -- | A LLVM array of a certain length and type.
    Arr Int Type
  | -- | The LLVM @void@ type.
    Void

-- | Possible values that can be handed to LLVM instructions. Can either be
-- constant values or local or global variables.
data Value
  = -- | A local variable.
    Loc J.Ident
  | -- | A global variable.
    Glob J.Ident
  | -- | A constant integer value.
    IConst Int
  | -- | A constant double value.
    DConst Double
  | -- | A constant boolean value.
    BConst Bool
  | -- | A constant string value.
    SConst Text
  | -- | A constant void value. Cannot exist in LLVM.
    VConst

-- | An argument in a function definition or function call.
data Arg = Argument Type Value

-- | Relational operators of the @icmp@ instruction. We only support signed
-- variants of the operators.
data RelOp = Eq | Ne | Sgt | Sge | Slt | Sle

-- | Relational operators of the @fcmp@ instruction. We only support ordered
-- variants of the operators.
data FRelOp = Oeq | One | Ogt | Oge | Olt | Ole

-- | An offset in the @getelementptr@ instruction.
data ElemOffset = Offset Type Int

-- | A predecessor entry in a @phi@ node.
data PhiElem = PhiElem Value Ident

-- | A LLVM instruction.
data Instruction
  = -- | Function declaration: @declare \<ty\> \@\<name\>(\<args\>)@
    FnDecl Type Ident [Type]
  | -- | Function definition: @define \<ty\> \@\<name\>(\<args\>) {@
    FnDef Type Ident [Arg]
  | -- | End of Function definition: @}@
    EndFnDef
  | -- | Label definition: @\<label\>:@
    LabelDef Ident
  | -- | String definition: @\@\<id\> = internal constant [\<len\> x i8] c"\<string\>\\00"@
    StringDef Ident Type Value
  | -- | @alloca@ instruction: @\<result\> = alloca \<ty\>@
    Alloca Ident Type
  | -- | @store@ instruction: @store \<ty\> \<value\>, \<ty\>* \<pointer\>@
    Store Type Value Ident
  | -- | @load@ instruction: @\<result\> = load \<ty\>, \<ty\>* \<pointer\>@
    Load Ident Type Ident
  | -- | @getelementptr@ instruction: @\<result\> = getelementptr \<ty\>, \<ty\>* \<pointer\>{, \<ty\> \<offset\>}*@
    GetElementPtr Ident Type Value [ElemOffset]
  | -- | @call@ instruction (non-void): @\<result\> = call \<ty\> \<fnname\>(\<function args\>)@
    Call Ident Type Ident [Arg]
  | -- | @call@ instruction (void): @call void \<fnname\>(\<function args\>)@
    VCall Ident [Arg]
  | -- | @ret@ instruction (non-void): @ret \<type\> \<value\>@
    Return Type Value
  | -- | @ret@ instruction (void): @ret void@
    VReturn
  | -- | @icmp@ instruction: @\<result\> = icmp \<cond\> \<ty\> \<op1\>, \<op2\>@
    ICompare Ident RelOp Type Value Value
  | -- | @fcmp@ instruction: @\<result\> = fcmp \<cond\> \<ty\> \<op1\>, \<op2\>@
    FCompare Ident FRelOp Type Value Value
  | -- | @br@ instruction (condidtional): @br i1 \<cond\>, label \<iftrue\>, label \<iffalse\>@
    Branch Value Ident Ident
  | -- | @br@ instruction (uncondidtional): @br label \<dest\>@
    UncondBranch Ident
  | -- | @unreachable@ instruction: @unreachable@
    Unreachable
  | -- | @add@ instruction: @\<result\> = add \<ty\> \<op1\>, \<op2\>@
    Add Ident Type Value Value
  | -- | @sub@ instruction: @\<result\> = sub \<ty\> \<op1\>, \<op2\>@
    Sub Ident Type Value Value
  | -- | @mul@ instruction: @\<result\> = mul \<ty\> \<op1\>, \<op2\>@
    Mul Ident Type Value Value
  | -- | @sdiv@ instruction: @\<result\> = sdiv \<ty\> \<op1\>, \<op2\>@
    SDiv Ident Type Value Value
  | -- | @srem@ instruction: @\<result\> = srem \<ty\> \<op1\>, \<op2\>@
    SRem Ident Type Value Value
  | -- | @fadd@ instruction: @\<result\> = fadd \<ty\> \<op1\>, \<op2\>@
    FAdd Ident Type Value Value
  | -- | @fsub@ instruction: @\<result\> = fsub \<ty\> \<op1\>, \<op2\>@
    FSub Ident Type Value Value
  | -- | @fmul@ instruction: @\<result\> = fmul \<ty\> \<op1\>, \<op2\>@
    FMul Ident Type Value Value
  | -- | @fdiv@ instruction: @\<result\> = fdiv \<ty\> \<op1\>, \<op2\>@
    FDiv Ident Type Value Value
  | -- | @fneg@ instruction: @\<result\> = fneg \<ty\> \<op1\>@.
    -- Not supported on old LLVM versions.
    FNeg Ident Type Value
  | -- | @and@ instruction: @\<result\> = and \<ty\> \<op1\>, \<op2\>@
    And Ident Type Value Value
  | -- | @or@ instruction: @\<result\> = or \<ty\> \<op1\>, \<op2\>@
    Or Ident Type Value Value
  | -- | @xor@ instruction: @\<result\> = xor \<ty\> \<op1\>, \<op2\>@
    XOr Ident Type Value Value
  | -- | @phi@ instruction: @\<result\> = phi \<ty\> [ \<val0\>, \<label0\>], ...@
    Phi Ident Type [PhiElem]
  | -- | A blank line. Has no semantics, but improves readability.
    Blank
