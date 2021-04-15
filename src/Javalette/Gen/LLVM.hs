{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Javalette.Gen.LLVM
  ( generateIR,
  )
where

import Control.Monad (void)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State
  ( MonadState (get, put),
    StateT (StateT),
    evalStateT,
    gets,
  )
import Control.Monad.Writer
  ( MonadWriter (tell),
    WriterT (WriterT),
    execWriterT,
  )
import Data.Either (fromRight)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Endo (..), appEndo)
import Data.Text (Text)
import qualified Data.Text as T
import Javalette.Check.TypeCheck (AnnotatedProg)
import Javalette.Gen.LLVM.Instructions (Instruction)
import qualified Javalette.Gen.LLVM.Instructions as L
import Javalette.Gen.LLVM.Textual (generateCode)
import Javalette.Lang.Abs

type Code = Endo ([Instruction], [Instruction])

generateIR :: AnnotatedProg -> Text
generateIR prog = generateCode $ glob ++ code
  where
    (code, glob) = appEndo (runGen $ compile prog) ([], [])

newtype Gen a = MkGen (WriterT Code (StateT Env Identity) a)
  deriving (Functor, Applicative, Monad, MonadState Env, MonadWriter Code)

runGen :: Gen () -> Code
runGen (MkGen gen) = runIdentity (evalStateT (execWriterT gen) emptyEnv)

emit :: Instruction -> Gen ()
emit instr = tell $ Endo (([instr], mempty) <>)

-- | Emit a global definition out-of-order
emitGlob :: Instruction -> Gen ()
emitGlob instr = tell $ Endo ((mempty, [instr]) <>)

compile :: AnnotatedProg -> Gen ()
compile prog = do
  emitGlob $ L.FnDecl L.Void "printInt" [L.Int 32]
  emitGlob $ L.FnDecl L.Void "printDouble" [L.Doub]
  emitGlob $ L.FnDecl L.Void "printString" [L.Ptr $ L.Int 8]
  emitGlob $ L.FnDecl (L.Int 32) "readInt" []
  emitGlob $ L.FnDecl L.Doub "readDouble" []
  emitGlob L.Blank
  compileProg prog

compileProg :: AnnotatedProg -> Gen ()
compileProg (Program []) = return ()
compileProg (Program (fn : rest)) = do
  compileFn fn
  compileProg $ Program rest

compileFn :: TopDef -> Gen ()
compileFn (FnDef typ jlId args (Block blk)) = do
  llvmArgs <- compileFnArgs args
  emit L.Blank
  emit $ L.FnDef (toLLVMType typ) jlId llvmArgs
  emit $ L.LabelDef $ Ident "entry"
  newTop
  compileStmts blk
  discardTop
  emit L.EndFnDef
  where
    compileFnArgs :: [Arg] -> Gen [L.Arg]
    compileFnArgs [] = return []
    compileFnArgs (Argument typ jlId : rest) = do
      llvmId <- newVarName
      pushVar jlId llvmId
      args <- compileFnArgs rest
      return $ L.Argument (toLLVMType typ) (L.Loc llvmId) : args

compileStmts :: [Stmt] -> Gen ()
compileStmts [] = return ()
compileStmts (stmt : stmts) = do
  compileStmt stmt
  compileStmts stmts

compileStmt :: Stmt -> Gen ()
compileStmt Empty = return ()
compileStmt (BStmt (Block _)) = undefined
compileStmt (Decl typ items) = do
  compileItems items typ
compileStmt (Ass jlId expr@(ETyped _ typ)) = do
  val <- compileExpr expr
  llvmId <- lookupVar jlId
  emit $ L.Store (toLLVMType typ) val llvmId
compileStmt (Incr jlId) = undefined
compileStmt (Decr jlId) = undefined
compileStmt (Ret expr@(ETyped _ typ)) = do
  val <- compileExpr expr
  emit $ L.Return (toLLVMType typ) val
compileStmt VRet = emit L.VReturn
compileStmt (Cond expr stmt) = undefined
compileStmt (CondElse expr stmt smt) = undefined
compileStmt (While expr stmt) = undefined
compileStmt (SExp expr) = void $ compileExpr expr

compileItems :: [Item] -> Type -> Gen ()
compileItems [] _ = return ()
compileItems (item : items) typ =
  case item of
    NoInit jlId -> do
      newVarInstr jlId typ
      compileItems items typ
    Init jlId expr -> do
      llvmId <- newVarInstr jlId typ
      val <- compileExpr expr
      emit $ L.Store (toLLVMType typ) val llvmId
      compileItems items typ

newVarInstr :: Ident -> Type -> Gen Ident
newVarInstr jlId typ = do
  llvmId <- newVarName
  pushVar jlId llvmId
  emit $ L.Alloca llvmId (toLLVMType typ)
  return llvmId

compileExprs :: [Expr] -> Gen [L.Value]
compileExprs [] = return []
compileExprs (expr : exprs) = do
  val <- compileExpr expr
  vals <- compileExprs exprs
  return $ val : vals

compileExpr :: Expr -> Gen L.Value
compileExpr (ETyped (EVar jlId) typ) = do
  llvmId <- lookupVar jlId
  tempId <- newVarName
  emit $ L.Load tempId (toLLVMType typ) llvmId
  return $ L.Loc tempId
compileExpr (ETyped (ELitInt ival) Int) = return $ L.IConst (fromInteger ival)
compileExpr (ETyped (ELitDoub dval) Doub) = undefined
compileExpr (ETyped ELitTrue Bool) = undefined
compileExpr (ETyped ELitFalse Bool) = undefined
compileExpr (ETyped (EApp jlId exprs) Void) = do
  vals <- compileExprs exprs
  emit $ L.VCall jlId (zipArgs vals (getTypes exprs))
  return L.VConst
  where
    zipArgs :: [L.Value] -> [Type] -> [L.Arg]
    zipArgs [] [] = []
    zipArgs (val : vals) (typ : types) = L.Argument (toLLVMType typ) val : zipArgs vals types
compileExpr (ETyped (EApp jlId exprs) typ) = undefined
compileExpr (ETyped (EString sval) String) = do
  let text = T.pack (sval ++ "\\00")
  let typ = L.Arr (length sval + 1) (L.Int 8)
  llvmGlobId <- newGlobVarName
  emitGlob $ L.StringDef llvmGlobId typ (L.SConst text)
  llvmLocId <- newVarName
  emit $ L.GetElementPtr llvmLocId typ (L.Glob llvmGlobId) [L.Offset (L.Int 32) 0, L.Offset (L.Int 32) 0]
  return $ L.Loc llvmLocId
compileExpr (ETyped (ENeg expr) Doub) = do
  val <- compileExpr expr
  tempId <- newVarName
  emit $ L.FNeg tempId (toLLVMType Doub) val
  return $ L.Loc tempId
compileExpr (ETyped (ENeg expr) Int) = do
  val <- compileExpr expr
  tempId <- newVarName
  emit $ L.Mul tempId (toLLVMType Int) val (L.IConst (-1))
  return $ L.Loc tempId
compileExpr (ETyped (ENot expr) Bool) = undefined
compileExpr (ETyped (EMul expr1 op expr2) typ) = undefined
compileExpr (ETyped (EAdd expr1 op expr2) typ) = undefined
compileExpr (ETyped (ERel expr1 op expr2) Bool) = undefined
compileExpr (ETyped (EAnd expr1 expr2) Bool) = undefined
compileExpr (ETyped (EOr expr1 expr2) Bool) = undefined
compileExpr _ = error "No (matching) type annotation found!"

toLLVMType :: Type -> L.Type
toLLVMType Int = L.Int 32
toLLVMType Doub = L.Doub
toLLVMType Bool = L.Int 1
toLLVMType Void = L.Void
toLLVMType String = L.Ptr $ L.Int 8

getTypes :: [Expr] -> [Type]
getTypes [] = []
getTypes (expr : exprs) =
  case expr of
    ETyped _ typ -> typ : getTypes exprs
    _ -> error "Not a typed expression!"

-- * Environment

data Env = Env
  { vars :: [Map Ident Ident],
    nextVar :: Int,
    nextGlobVar :: Int,
    nextLabel :: Int
  }

emptyEnv :: Env
emptyEnv =
  Env
    { vars = [Map.empty],
      nextVar = 0,
      nextGlobVar = 0,
      nextLabel = 0
    }

-- | Discard the top context of the context stack.
discardTop :: Gen ()
discardTop = do
  env <- get
  case vars env of
    top : rest -> put env {vars = rest}
    _ -> error "Stack already empty!"

-- | Add an empty context on top of the context stack.
newTop :: Gen ()
newTop = do
  env <- get
  put env {vars = Map.empty : vars env}

newVarName :: Gen Ident
newVarName = do
  env <- get
  let num = nextVar env
  put env {nextVar = num + 1}
  let llvmId = Ident $ "t" <> T.pack (show num)
  return llvmId

newGlobVarName :: Gen Ident
newGlobVarName = do
  env <- get
  let num = nextGlobVar env
  put env {nextGlobVar = num + 1}
  let llvmGlobId = Ident $ "g" <> T.pack (show num)
  return llvmGlobId

pushVar :: Ident -> Ident -> Gen ()
pushVar jlId llvmId = do
  env <- get
  put env {vars = Map.insert jlId llvmId (head (vars env)) : tail (vars env)}
  return ()

lookupVar :: Ident -> Gen Ident
lookupVar jlId = gets (stackLookup jlId . vars)
  where
    stackLookup :: Ident -> [Map Ident Ident] -> Ident
    stackLookup _ [] = error "Variable not found!"
    stackLookup jlId (map : stack) = case Map.lookup jlId map of
      Just llvmId -> llvmId
      Nothing -> stackLookup jlId stack
