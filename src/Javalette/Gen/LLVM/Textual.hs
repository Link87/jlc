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

-- TODO Add missing instructions
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
generateInstructionCode (L.VCall id args) =
  indent
    <> "call "
    <> typeId L.Void
    <> B.singleton ' '
    <> llvmGlobIdent id
    <> B.singleton '('
    <> argList args
    <> B.singleton ')'
generateInstructionCode (L.Return typ (L.LitVal val)) = indent <> "ret " <> typeId typ <> B.singleton ' ' <> constRepr val
generateInstructionCode L.VReturn = indent <> "ret " <> typeId L.Void
generateInstructionCode (L.Mul id typ val1 val2) =
  indent
    <> llvmLocIdent id
    <> " = mul "
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
generateInstructionCode L.Blank = ""

typeId :: L.Type -> Builder
typeId (L.Ptr typ) = typeId typ <> B.singleton '*'
typeId (L.Int prec) = B.singleton 'i' <> (B.fromString . show) prec
typeId L.Doub = "double"
typeId (L.Arr size typ) =
  B.singleton '['
    <> (B.fromString . show) size
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

valueRepr :: L.Value -> Builder
valueRepr (L.LitVal val) = constRepr val
valueRepr (L.RefVal id) = llvmLocIdent id

constRepr :: L.Const -> Builder
constRepr (L.IConst ival) = B.fromText $ T.pack $ show ival
constRepr (L.DConst dval) = B.fromText $ T.pack $ show dval
constRepr L.VConst = error "A void has no value representation!"

ident :: Ident -> Builder
ident (Ident name) = B.fromText name

llvmGlobIdent :: Ident -> Builder
llvmGlobIdent (Ident name) = B.singleton '@' <> B.fromText name

llvmLocIdent :: Ident -> Builder
llvmLocIdent (Ident name) = B.singleton '%' <> B.fromText name

indent :: Builder
indent = "  "
