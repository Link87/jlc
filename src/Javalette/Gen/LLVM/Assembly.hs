{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune, ignore-exports, show-extensions #-}

module Javalette.Gen.LLVM.Assembly
  ( generateCode,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import Javalette.Gen.LLVM.Instruction

-- | Generate LLVM assembly code from a list of 'Instruction's.
generateCode :: [Instruction] -> Text
generateCode = TL.toStrict . B.toLazyText . generateCode'
  where
    generateCode' :: [Instruction] -> Builder
    generateCode' = foldr ((\a b -> a <> "\n" <> b) . generateInstructionCode) ""

-- | Generate LLVM assembly code for a single LLVM instruction.
generateInstructionCode :: Instruction -> Builder
generateInstructionCode (FnDecl typ id args) =
  "declare "
    <> typeId typ
    <> B.singleton ' '
    <> llvmGlobIdent id
    <> B.singleton '('
    <> typeList args
    <> B.singleton ')'
generateInstructionCode (FnDef typ id args) =
  "define "
    <> typeId typ
    <> B.singleton ' '
    <> llvmGlobIdent id
    <> B.singleton '('
    <> argList args
    <> ") {"
generateInstructionCode EndFnDef = B.singleton '}'
generateInstructionCode (LabelDef id) = ident id <> B.singleton ':'
generateInstructionCode (StringDef id typ (SConst text)) =
  llvmGlobIdent id
    <> " = internal constant "
    <> typeId typ
    <> " c\""
    <> B.fromText text
    <> "\""
generateInstructionCode (Alloca id typ) =
  indent
    <> llvmLocIdent id
    <> " = alloca "
    <> typeId typ
generateInstructionCode (Store typ val id) =
  indent
    <> "store "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val
    <> ", "
    <> typeId (Ptr typ)
    <> B.singleton ' '
    <> llvmLocIdent id
generateInstructionCode (Load id1 typ id2) =
  indent
    <> llvmLocIdent id1
    <> " = load "
    <> typeId typ
    <> ", "
    <> typeId (Ptr typ)
    <> B.singleton ' '
    <> llvmLocIdent id2
generateInstructionCode (GetElementPtr id1 typ id2 offsets) =
  indent
    <> llvmLocIdent id1
    <> " = getelementptr "
    <> typeId typ
    <> ", "
    <> typeId (Ptr typ)
    <> B.singleton ' '
    <> valueRepr id2
    <> ", "
    <> offsetList offsets
generateInstructionCode (Call id1 typ id2 args) =
  indent
    <> llvmLocIdent id1
    <> " = call "
    <> typeId typ
    <> B.singleton ' '
    <> llvmGlobIdent id2
    <> B.singleton '('
    <> argList args
    <> B.singleton ')'
generateInstructionCode (VCall id args) =
  indent
    <> "call "
    <> typeId Void
    <> B.singleton ' '
    <> llvmGlobIdent id
    <> B.singleton '('
    <> argList args
    <> B.singleton ')'
generateInstructionCode (Return typ val) =
  indent
    <> "ret "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val
generateInstructionCode VReturn =
  indent
    <> "ret "
    <> typeId Void
generateInstructionCode (ICompare id relOp typ val1 val2) =
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
generateInstructionCode (FCompare id fRelOp typ val1 val2) =
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
generateInstructionCode (Branch val lab1 lab2) =
  indent
    <> "br "
    <> typeId (Int 1)
    <> B.singleton ' '
    <> valueRepr val
    <> ", label "
    <> llvmLocIdent lab1
    <> ", label "
    <> llvmLocIdent lab2
generateInstructionCode (UncondBranch lab) =
  indent
    <> "br label "
    <> llvmLocIdent lab
generateInstructionCode Unreachable = indent <> "unreachable"
generateInstructionCode (Add id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = add "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (Sub id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = sub "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (Mul id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = mul "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (SDiv id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = sdiv "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (SRem id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = srem "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (FAdd id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = fadd "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (FSub id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = fsub "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (FMul id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = fmul "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (FDiv id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = fdiv "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (FNeg id typ val) =
  indent
    <> llvmLocIdent id
    <> " = fneg "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val
generateInstructionCode (And id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = and "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (Or id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = or "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (XOr id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = xor "
    <> typeId typ
    <> B.singleton ' '
    <> valueRepr val1
    <> ", "
    <> valueRepr val2
generateInstructionCode (Phi id typ phis) =
  indent
    <> llvmLocIdent id
    <> " = phi "
    <> typeId typ
    <> B.singleton ' '
    <> phiList phis
generateInstructionCode Blank = ""

-- | Get the representation of a 'Type' in LLVM assembly.
typeId :: Type -> Builder
typeId (Ptr typ) = typeId typ <> B.singleton '*'
typeId (Int prec) = B.singleton 'i' <> showInt prec
typeId Doub = "double"
typeId (Arr size typ) =
  B.singleton '['
    <> showInt size
    <> " x "
    <> typeId typ
    <> B.singleton ']'
typeId Void = "void"

-- | Generate a representation of a list of 'Type's, separated by commas.
typeList :: [Type] -> Builder
typeList [] = ""
typeList [typ] = typeId typ
typeList (typ : types) = typeId typ <> ", " <> typeList types

-- | Generate a representation of an argument list, i.e. @['Arg']@. Each
-- argument is a 'Value' preceded by a 'Type'. Arguments are sepatated by commas.
argList :: [Arg] -> Builder
argList [] = ""
argList [Argument typ val] = typeId typ <> B.singleton ' ' <> valueRepr val
argList (Argument typ val : types) = typeId typ <> B.singleton ' ' <> valueRepr val <> ", " <> argList types

-- | Generate a representation of a list of offsets of @getelementptr@. Each
-- offset is represented as an integer offset value preceded by a 'Type'.
-- Offset entries are sepatated by commas.
offsetList :: [ElemOffset] -> Builder
offsetList [] = ""
offsetList [Offset typ off] = typeId typ <> B.singleton ' ' <> showInt off
offsetList (Offset typ off : offsets) = typeId typ <> B.singleton ' ' <> showInt off <> ", " <> offsetList offsets

-- | Generate a representation of a list of predecessors of a @phi@ node. Each
-- element is represented as a 'Value' followed by a label identifier.
-- Entries are wrapped in brackets and separated by commas.
phiList :: [PhiElem] -> Builder
phiList [] = ""
phiList [PhiElem val lab] = "[ " <> valueRepr val <> ", " <> llvmLocIdent lab <> " ]"
phiList (PhiElem val lab : phis) = "[ " <> valueRepr val <> ", " <> llvmLocIdent lab <> " ], " <> phiList phis

-- | Get the representation of a 'Value' in LLVM assembly. Can either be a
-- constant value or a local or global identifier.
valueRepr :: Value -> Builder
valueRepr (Loc id) = llvmLocIdent id
valueRepr (Glob id) = llvmGlobIdent id
valueRepr (IConst ival) = showInt ival
valueRepr (DConst dval) = showDouble dval
valueRepr (BConst True) = "true"
valueRepr (BConst False) = "false"
valueRepr VConst = error "A void has no value representation!"

-- | Get the representation of a relational operator used by @icmp@ in LLVM.
relOpRepr :: RelOp -> Builder
relOpRepr Eq = "eq"
relOpRepr Ne = "ne"
relOpRepr Sgt = "sgt"
relOpRepr Sge = "sge"
relOpRepr Slt = "slt"
relOpRepr Sle = "sle"

-- | Get the representation of a relational operator used by @fcmp@ in LLVM.
fRelOpRepr :: FRelOp -> Builder
fRelOpRepr Oeq = "oeq"
fRelOpRepr One = "one"
fRelOpRepr Ogt = "ogt"
fRelOpRepr Oge = "oge"
fRelOpRepr Olt = "olt"
fRelOpRepr Ole = "ole"

-- | Convert an integer value into a 'Builder'.
showInt :: Int -> Builder
showInt = B.fromString . show

-- | Convert a double value into a 'Builder'.
showDouble :: Double -> Builder
showDouble = B.fromString . show

-- | Extract an identifier from an 'Ident' and convert it into a 'Builder'.
ident :: Ident -> Builder
ident (Ident name) = B.fromText name

-- | Precede an 'Ident' with a @\@@ to convert it into a global variable name
-- representation for LLVM and convert it into a 'Builder'.
llvmGlobIdent :: Ident -> Builder
llvmGlobIdent (Ident name) = B.singleton '@' <> B.fromText name

-- | Precede an 'Ident' with a @%@ to convert it into a local variable name
-- representation for LLVM and convert it into a 'Builder'.
llvmLocIdent :: Ident -> Builder
llvmLocIdent (Ident name) = B.singleton '%' <> B.fromText name

-- | Indentation for LLVM instructions. Improves readability.
indent :: Builder
indent = "  "
