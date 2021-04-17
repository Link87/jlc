{-# LANGUAGE OverloadedStrings #-}

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
import Javalette.Lang.Abs (Ident (Ident))
import qualified Javalette.Lang.Abs as J

generateCode :: [Instruction] -> Text
generateCode = TL.toStrict . B.toLazyText . generateCode'
  where
    generateCode' :: [Instruction] -> Builder
    generateCode' = foldr ((\a b -> a <> "\n" <> b) . generateInstructionCode) ""

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

typeList :: [L.Type] -> Builder
typeList [] = ""
typeList [typ] = typeId typ
typeList (typ : types) = typeId typ <> ", " <> typeList types

argList :: [L.Arg] -> Builder
argList [] = ""
argList [L.Argument typ val] = typeId typ <> B.singleton ' ' <> valueRepr val
argList (L.Argument typ val : types) = typeId typ <> B.singleton ' ' <> valueRepr val <> ", " <> argList types

offsetList :: [L.ElemOffset] -> Builder
offsetList [] = ""
offsetList [L.Offset typ off] = typeId typ <> B.singleton ' ' <> showInt off
offsetList (L.Offset typ off : offsets) = typeId typ <> B.singleton ' ' <> showInt off <> ", " <> offsetList offsets

phiList :: [L.PhiElem] -> Builder
phiList [] = ""
phiList [L.PhiElem val lab] = "[ " <> valueRepr val <> ", " <> llvmLocIdent lab <> " ]"
phiList (L.PhiElem val lab : phis) = "[ " <> valueRepr val <> ", " <> llvmLocIdent lab <> " ], " <> phiList phis

valueRepr :: L.Value -> Builder
valueRepr (L.Loc id) = llvmLocIdent id
valueRepr (L.Glob id) = llvmGlobIdent id
valueRepr (L.IConst ival) = showInt ival
valueRepr (L.DConst dval) = showDouble dval
valueRepr (L.BConst True) = "true"
valueRepr (L.BConst False) = "false"
valueRepr L.VConst = error "A void has no value representation!"

relOpRepr :: L.RelOp -> Builder
relOpRepr L.Eq = "eq"
relOpRepr L.Ne = "ne"
relOpRepr L.Sgt = "sgt"
relOpRepr L.Sge = "sge"
relOpRepr L.Slt = "slt"
relOpRepr L.Sle = "sle"

fRelOpRepr :: L.FRelOp -> Builder
fRelOpRepr L.Oeq = "oeq"
fRelOpRepr L.One = "one"
fRelOpRepr L.Ogt = "ogt"
fRelOpRepr L.Oge = "oge"
fRelOpRepr L.Olt = "olt"
fRelOpRepr L.Ole = "ole"

showInt :: Int -> Builder
showInt = B.fromString . show

showDouble :: Double -> Builder
showDouble = B.fromString . show

ident :: Ident -> Builder
ident (Ident name) = B.fromText name

llvmGlobIdent :: Ident -> Builder
llvmGlobIdent (Ident name) = B.singleton '@' <> B.fromText name

llvmLocIdent :: Ident -> Builder
llvmLocIdent (Ident name) = B.singleton '%' <> B.fromText name

indent :: Builder
indent = "  "
