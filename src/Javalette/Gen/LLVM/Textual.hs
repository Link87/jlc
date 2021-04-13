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
import Javalette.Lang.Abs (Ident(Ident))
import qualified Javalette.Lang.Abs as J

generateCode :: [Instruction] -> Text
generateCode = TL.toStrict . B.toLazyText . generateCode'
  where
    generateCode' :: [Instruction] -> Builder
    generateCode' = foldr ((\a b -> a <> "\n" <> b) . generateInstructionCode) ""

-- TODO Add missing instructions
generateInstructionCode :: Instruction -> Builder
generateInstructionCode (L.FnDecl typ id args) =
  "@declare "
    <> typeId typ
    <> B.singleton ' '
    <> ident id
    <> " ("
    <> typeList args
    <> B.singleton ')'
generateInstructionCode (L.FnDef typ id args) =
  "@declare "
    <> typeId typ
    <> B.singleton ' '
    <> ident id
    <> " ("
    <> argList args
    <> ") {"
generateInstructionCode L.EndFnDef = B.singleton '}'
generateInstructionCode (L.LabelDef id) = ident id <> B.singleton ':'

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
argList [L.Argument typ id] = typeId typ <> B.singleton ' ' <> ident id 
argList (L.Argument typ id : types) = typeId typ <> B.singleton ' ' <> ident id <> ", " <> argList types

ident :: Ident -> Builder
ident (Ident name) = B.fromText name

llvmGlobIdent :: Ident -> Builder
llvmGlobIdent (Ident name) = B.singleton '@' <> B.fromText name

llvmLocIdent :: Ident -> Builder
llvmLocIdent (Ident name) = B.singleton '%' <> B.fromText name
