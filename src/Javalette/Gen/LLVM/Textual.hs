{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune, ignore-exports, show-extensions #-}

module Javalette.Gen.LLVM.Textual
  ( generateCode,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import Javalette.Gen.LLVM.Instructions (Instruction)
import qualified Javalette.Gen.LLVM.Instructions as L
import qualified Javalette.Lang.Abs as J

-- | Generate textual LLVM code from a list of 'Instruction's.
generateCode :: [Instruction] -> Text
generateCode = TL.toStrict . B.toLazyText . generateCode'
  where
    generateCode' :: [Instruction] -> Builder
    generateCode' = foldr ((\a b -> a <> "\n" <> b) . generateInstructionCode) ""

-- | Assemble the textual LLVM code of a single LLVM instruction.
generateInstructionCode :: Instruction -> Builder
generateInstructionCode (L.FnDecl typ id args) =
  "declare "
    <> typeId typ
    <> B.singleton ' '
    <> llvmGlobIdent id
    <> B.singleton '('
    <> typeList args
    <> B.singleton ')'
generateInstructionCode (L.FnDef typ id args) =
  "define "
    <> typeId typ
    <> B.singleton ' '
    <> llvmGlobIdent id
    <> B.singleton '('
    <> argList args
    <> ") {"
generateInstructionCode L.EndFnDef = B.singleton '}'
generateInstructionCode (L.LabelDef id) = ident id <> B.singleton ':'
generateInstructionCode (L.StringDef id typ (L.SConst text)) =
  llvmGlobIdent id
    <> " = internal constant "
    <> typeId typ
    <> " c\""
    <> B.fromText text
    <> "\""
generateInstructionCode (L.Alloca id typ) =
  indent
    <> llvmLocIdent id
    <> " = alloca "
    <> typeId typ
generateInstructionCode (L.Store typ val id) =
  indent
    <> "store "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val
    <> ", "
    <> typeId (L.Ptr typ)
    <> B.singleton ' '
    <> llvmLocIdent id
generateInstructionCode (L.Load id1 typ id2) =
  indent
    <> llvmLocIdent id1
    <> " = load "
    <> typeId typ
    <> ", "
    <> typeId (L.Ptr typ)
    <> B.singleton ' '
    <> llvmLocIdent id2
generateInstructionCode (L.GetElementPtr id1 typ id2 offsets) =
  indent
    <> llvmLocIdent id1
    <> " = getelementptr "
    <> typeId typ
    <> ", "
    <> typeId (L.Ptr typ)
    <> B.singleton ' '
    <> valueRepr id2
    <> ", "
    <> offsetList offsets
generateInstructionCode (L.Call id1 typ id2 args) =
  indent
    <> llvmLocIdent id1
    <> " = call "
    <> typeId typ
    <> B.singleton ' '
    <> llvmGlobIdent id2
    <> B.singleton '('
    <> argList args
    <> B.singleton ')'
generateInstructionCode (L.VCall id args) =
  indent
    <> "call "
    <> typeId L.Void
    <> B.singleton ' '
    <> llvmGlobIdent id
    <> B.singleton '('
    <> argList args
    <> B.singleton ')'
generateInstructionCode (L.Return typ val) =
  indent
    <> "ret "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val
generateInstructionCode L.VReturn =
  indent
    <> "ret "
    <> typeId L.Void
generateInstructionCode (L.ICompare id relOp typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = icmp "
    <> relOpRepr relOp
    <> B.singleton ' '
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (L.FCompare id fRelOp typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = fcmp "
    <> fRelOpRepr fRelOp
    <> B.singleton ' '
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (L.Branch val lab1 lab2) =
  indent
    <> "br "
    <> typeId (L.Int 1)
    <> B.singleton ' '
    <> valueRepr val
    <> ", label "
    <> llvmLocIdent lab1
    <> ", label "
    <> llvmLocIdent lab2
generateInstructionCode (L.UncondBranch lab) =
  indent
    <> "br label "
    <> llvmLocIdent lab
generateInstructionCode L.Unreachable = indent <> "unreachable"
generateInstructionCode (L.Add id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = add "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (L.Sub id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = sub "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (L.Mul id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = mul "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (L.SDiv id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = sdiv "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (L.SRem id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = srem "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (L.FAdd id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = fadd "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (L.FSub id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = fsub "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (L.FMul id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = fmul "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (L.FDiv id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = fdiv "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (L.FNeg id typ val) =
  indent
    <> llvmLocIdent id
    <> " = fneg "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val
generateInstructionCode (L.And id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = and "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (L.Or id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = or "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (L.XOr id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = xor "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (L.Phi id typ phis) =
  indent
    <> llvmLocIdent id
    <> " = phi "
    <> typeId typ
    <> B.singleton ' '
    <> phiList phis
generateInstructionCode L.Blank = ""

-- | Get the representation of a 'L.Type' in LLVM.
typeId :: L.Type -> Builder
typeId (L.Ptr typ) = typeId typ <> B.singleton '*'
typeId (L.Int prec) = B.singleton 'i' <> showInt prec
typeId L.Doub = "double"
typeId (L.Arr size typ) =
  B.singleton '['
    <> showInt size
    <> " x "
    <> typeId typ
    <> B.singleton ']'
typeId L.Void = "void"

-- | Assemble a representation of a list of 'L.Type's, separated by commas.
typeList :: [L.Type] -> Builder
typeList [] = ""
typeList [typ] = typeId typ
typeList (typ : types) = typeId typ <> ", " <> typeList types

-- | Assemble a representation of an argument list, i.e. @['L.Arg']@. Each
-- argument is a 'L.Value' preceded by a 'L.Type'. Arguments are sepatated by commas.
argList :: [L.Arg] -> Builder
argList [] = ""
argList [L.Argument typ val] = typeId typ <> B.singleton ' ' <> valueRepr val
argList (L.Argument typ val : types) = typeId typ <> B.singleton ' ' <> valueRepr val <> ", " <> argList types

-- | Assemble a representation of a list of offsets of @getelementptr@. Each
-- offset is represented as an integer offset value preceded by a 'L.Type'.
-- Offset entries are sepatated by commas.
offsetList :: [L.ElemOffset] -> Builder
offsetList [] = ""
offsetList [L.Offset typ off] = typeId typ <> B.singleton ' ' <> showInt off
offsetList (L.Offset typ off : offsets) = typeId typ <> B.singleton ' ' <> showInt off <> ", " <> offsetList offsets

-- | Assemble a representation of a list of predecessors of a @phi@ node. Each
-- element is represented as a 'L.Value' followed by a label identifier.
-- Entries are wrapped in brackets and separated by commas.
phiList :: [L.PhiElem] -> Builder
phiList [] = ""
phiList [L.PhiElem val lab] = "[ " <> valueRepr val <> ", " <> llvmLocIdent lab <> " ]"
phiList (L.PhiElem val lab : phis) = "[ " <> valueRepr val <> ", " <> llvmLocIdent lab <> " ], " <> phiList phis

-- | Get the representation of a 'L.Value' in LLVM. Can either be a constant value
-- or a local or global identifier.
valueRepr :: L.Value -> Builder
valueRepr (L.Loc id) = llvmLocIdent id
valueRepr (L.Glob id) = llvmGlobIdent id
valueRepr (L.IConst ival) = showInt ival
valueRepr (L.DConst dval) = showDouble dval
valueRepr (L.BConst True) = "true"
valueRepr (L.BConst False) = "false"
valueRepr L.VConst = error "A void has no value representation!"

-- | Get the representation of a relational operator used by @icmp@ in LLVM.
relOpRepr :: L.RelOp -> Builder
relOpRepr L.Eq = "eq"
relOpRepr L.Ne = "ne"
relOpRepr L.Sgt = "sgt"
relOpRepr L.Sge = "sge"
relOpRepr L.Slt = "slt"
relOpRepr L.Sle = "sle"

-- | Get the representation of a relational operator used by @fcmp@ in LLVM.
fRelOpRepr :: L.FRelOp -> Builder
fRelOpRepr L.Oeq = "oeq"
fRelOpRepr L.One = "one"
fRelOpRepr L.Ogt = "ogt"
fRelOpRepr L.Oge = "oge"
fRelOpRepr L.Olt = "olt"
fRelOpRepr L.Ole = "ole"

-- | Convert an integer value into a 'Builder'.
showInt :: Int -> Builder
showInt = B.fromString . show

-- | Convert a double value into a 'Builder'.
showDouble :: Double -> Builder
showDouble = B.fromString . show

-- | Extract an identifier from an 'L.Ident' and convert it into a 'Builder'.
ident :: L.Ident -> Builder
ident (L.Ident name) = B.fromText name

-- | Precede an 'L.Ident' with a @\@@ to convert it into a global variable name
-- representation for LLVM and convert it into a 'Builder'.
llvmGlobIdent :: L.Ident -> Builder
llvmGlobIdent (L.Ident name) = B.singleton '@' <> B.fromText name

-- | Precede an 'L.Ident' with a @%@ to convert it into a local variable name
-- representation for LLVM and convert it into a 'Builder'.
llvmLocIdent :: L.Ident -> Builder
llvmLocIdent (L.Ident name) = B.singleton '%' <> B.fromText name

-- | Indentation for LLVM instructions. Improves readability.
indent :: Builder
indent = "  "
