{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune, ignore-exports, show-extensions #-}

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
    modify,
  )
import Control.Monad.Writer
  ( MonadWriter (tell),
    WriterT (WriterT),
    execWriterT,
  )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Endo (..), appEndo)
import Data.Text (Text)
import qualified Data.Text as T
import Javalette.Check.TypeCheck (AnnotatedProg)
import Javalette.Gen.LLVM.Instruction (Instruction)
import qualified Javalette.Gen.LLVM.Instruction as L
import Javalette.Gen.LLVM.Assembly (generateCode)
import Javalette.Lang.Abs

-- * Main function

-- | Generate LLVM assembly from a type-annotated AST.
--
-- This uses two passes: First, the AST is converted in a list of
-- instuctions. Then, the list of instructions is converted into assembly code
-- using 'generateCode'.
generateIR :: AnnotatedProg -> Text
generateIR prog = generateCode $ glob ++ code
  where
    (code, glob) = appEndo (runGen $ compile prog) ([], [])

-- * Code compilation prerequisites

-- | Code collected in the 'WriterT' monad.
--
-- Contains a list of 'Instruction's
-- derived from the AST in-order as well as a list of instructions that are
-- global statements and emitted separately and out-of-order. Since left-nested
-- applications of @'(++)'@ are slow, we make use of 'Endo' to concatenate
-- instructions right-nested at the end.
type Code = Endo ([Instruction], [Instruction])

-- | The monad used for generating code. Saves the current state in a 'StateT'
-- monad and the emitted code in a 'WriterT' monad.
newtype Gen a = MkGen (WriterT Code (StateT Env Identity) a)
  deriving (Functor, Applicative, Monad, MonadState Env, MonadWriter Code)

-- | Extract emitted 'Code' from the 'Gen' monad.
runGen :: Gen () -> Code
runGen (MkGen gen) = runIdentity (evalStateT (execWriterT gen) emptyEnv)

-- | Append an instruction to the end of the code.
emit :: Instruction -> Gen ()
emit instr = tell $ Endo (([instr], mempty) <>)

-- | Emit an instruction as a global statement out-of-order. Global instructions
-- are prepended to normally emitted code.
emitGlob :: Instruction -> Gen ()
emitGlob instr = tell $ Endo ((mempty, [instr]) <>)

-- * Compilation functions

-- | Compile an type-annotated AST into a list of instructions. Emits global
-- statements for runtime function declarations.
compile :: AnnotatedProg -> Gen ()
compile prog = do
  emitGlob $ L.FnDecl L.Void "printInt" [L.Int 32]
  emitGlob $ L.FnDecl L.Void "printDouble" [L.Doub]
  emitGlob $ L.FnDecl L.Void "printString" [L.Ptr $ L.Int 8]
  emitGlob $ L.FnDecl (L.Int 32) "readInt" []
  emitGlob $ L.FnDecl L.Doub "readDouble" []
  emitGlob $ L.FnDecl (L.Ptr $ L.Int 8) "calloc" [L.Int 32, L.Int 32]
  emitGlob L.Blank
  compileProg prog

-- | Compile an type-annotated AST into a list of instructions.
compileProg :: AnnotatedProg -> Gen ()
compileProg (Program []) = return ()
compileProg (Program (fn : rest)) = do
  compileFn fn
  compileProg $ Program rest

-- | Compile a function definition into a list of instructions.
compileFn :: TopDef -> Gen ()
compileFn (FnDef typ jlId args (Block blk)) = do
  llvmArgs <- generateFnArgList args
  emit L.Blank
  emit $ L.FnDef (toLLVMType typ) (toLLVMIdent jlId) llvmArgs
  labelInstr "entry"
  compileFnArgVars args llvmArgs
  newVarTop
  compileStmts blk
  if typ == Void
    then emit L.VReturn
    else emit L.Unreachable
  discardVarTop
  emit L.EndFnDef
  where
    generateFnArgList :: [Arg] -> Gen [L.Arg]
    generateFnArgList [] = return []
    generateFnArgList (Argument typ jlId : rest) = do
      llvmArgId <- newVarName
      args <- generateFnArgList rest
      return $ L.Argument (toLLVMType typ) (L.Loc llvmArgId) : args
    compileFnArgVars :: [Arg] -> [L.Arg] -> Gen ()
    compileFnArgVars [] [] = return ()
    compileFnArgVars (Argument typ jlId : rest1) (L.Argument _ llvmArgId : rest2) = do
      llvmStackId <- newVarInstr jlId typ
      emit $ L.Store (toLLVMType typ) llvmArgId llvmStackId
      compileFnArgVars rest1 rest2

-- | Compile a list of statements into a list of instructions.
compileStmts :: [Stmt] -> Gen ()
compileStmts [] = return ()
compileStmts (stmt : stmts) = do
  compileStmt stmt
  compileStmts stmts

-- | Compile a function definition into a list of instructions.
compileStmt :: Stmt -> Gen ()
compileStmt Empty = return ()
compileStmt (BStmt (Block stmts)) = do
  newVarTop
  compileStmts stmts
  discardVarTop
compileStmt (Decl typ items) = do
  compileItems items typ
compileStmt (Ass jlId expr@(ETyped _ typ)) = do
  val <- compileExpr expr
  llvmId <- lookupVar jlId
  emit $ L.Store (toLLVMType typ) val llvmId
compileStmt (Incr jlId) = do
  llvmPtrId <- lookupVar jlId
  llvmValId <- newVarName
  emit $ L.Load llvmValId (toLLVMType Int) llvmPtrId
  llvmResId <- newVarName
  emit $ L.Add llvmResId (toLLVMType Int) (L.Loc llvmValId) (L.IConst 1)
  emit $ L.Store (toLLVMType Int) (L.Loc llvmResId) llvmPtrId
compileStmt (Decr jlId) = do
  llvmPtrId <- lookupVar jlId
  llvmValId <- newVarName
  emit $ L.Load llvmValId (toLLVMType Int) llvmPtrId
  llvmResId <- newVarName
  emit $ L.Sub llvmResId (toLLVMType Int) (L.Loc llvmValId) (L.IConst 1)
  emit $ L.Store (toLLVMType Int) (L.Loc llvmResId) llvmPtrId
compileStmt (Ret expr@(ETyped _ typ)) = do
  val <- compileExpr expr
  emit $ L.Return (toLLVMType typ) val
compileStmt VRet = emit L.VReturn
compileStmt (Cond expr stmt) = do
  ifLabId <- newLabName
  endLabId <- newLabName
  val <- compileExpr expr
  emit $ L.Branch val ifLabId endLabId
  labelInstr ifLabId
  compileStmt stmt
  emit $ L.UncondBranch endLabId
  labelInstr endLabId
compileStmt (CondElse expr stmt1 stmt2) = do
  ifLabId <- newLabName
  elseLabId <- newLabName
  endLabId <- newLabName
  val <- compileExpr expr
  emit $ L.Branch val ifLabId elseLabId
  labelInstr ifLabId
  compileStmt stmt1
  emit $ L.UncondBranch endLabId
  labelInstr elseLabId
  compileStmt stmt2
  emit $ L.UncondBranch endLabId
  labelInstr endLabId
compileStmt (While expr stmt) = do
  topLabId <- newLabName
  loopLabId <- newLabName
  endLabId <- newLabName
  emit $ L.UncondBranch topLabId
  labelInstr topLabId
  val <- compileExpr expr
  emit $ L.Branch val loopLabId endLabId
  labelInstr loopLabId
  compileStmt stmt
  emit $ L.UncondBranch topLabId
  labelInstr endLabId
compileStmt (SExp expr) = void $ compileExpr expr

-- | Compile items of a variable declaration into a list of instructions.
compileItems :: [Item] -> Type -> Gen ()
compileItems [] _ = return ()
compileItems (item : items) typ =
  case item of
    NoInit jlId -> do
      newVarInstr jlId typ
      compileItems items typ
    Init jlId expr -> do
      val <- compileExpr expr
      llvmId <- newVarInstr jlId typ
      emit $ L.Store (toLLVMType typ) val llvmId
      compileItems items typ

-- | Compile a list of expressions into a list of instructions.
compileExprs :: [Expr] -> Gen [L.Value]
compileExprs [] = return []
compileExprs (expr : exprs) = do
  val <- compileExpr expr
  vals <- compileExprs exprs
  return $ val : vals

-- | Compile an expression into a list of instructions.
compileExpr :: Expr -> Gen L.Value
compileExpr (ETyped (EVar jlId) typ) = loadVarInstr jlId typ
compileExpr (ETyped (ELitInt ival) Int) = return $ L.IConst (fromInteger ival)
compileExpr (ETyped (ELitDoub dval) Doub) = return $ L.DConst dval
compileExpr (ETyped ELitTrue Bool) = return $ L.BConst True
compileExpr (ETyped ELitFalse Bool) = return $ L.BConst False
compileExpr (ETyped (EApp jlId exprs) Void) = do
  vals <- compileExprs exprs
  emit $ L.VCall (toLLVMIdent jlId) (zipArgs vals (getTypes exprs))
  return L.VConst
compileExpr (ETyped (EApp jlId exprs) typ) = do
  vals <- compileExprs exprs
  llvmId <- newVarName
  emit $ L.Call llvmId (toLLVMType typ) (toLLVMIdent jlId) (zipArgs vals (getTypes exprs))
  return $ L.Loc llvmId
compileExpr (ETyped (EString sval) String) = do
  let text = T.pack (sval ++ "\\00")
  let typ = L.Arr (length sval + 1) (L.Int 8)
  llvmGlobId <- newGlobVarName
  emitGlob $ L.StringDef llvmGlobId typ (L.TConst text)
  llvmLocId <- newVarName
  emit $ L.GetElementPtr llvmLocId typ (L.Glob llvmGlobId) [L.Offset (L.Int 32) 0, L.Offset (L.Int 32) 0]
  return $ L.Loc llvmLocId
compileExpr (ETyped (ENeg expr) Doub) = do
  val <- compileExpr expr
  tempId <- newVarName
  emit $ L.FMul tempId (toLLVMType Doub) val (L.DConst (-1))
  return $ L.Loc tempId
compileExpr (ETyped (ENeg expr) Int) = do
  val <- compileExpr expr
  tempId <- newVarName
  emit $ L.Mul tempId (toLLVMType Int) val (L.IConst (-1))
  return $ L.Loc tempId
compileExpr (ETyped (ENot expr) Bool) = do
  val <- compileExpr expr
  llvmId <- newVarName
  emit $ L.XOr llvmId (toLLVMType Bool) val (L.BConst True)
  return $ L.Loc llvmId
compileExpr (ETyped (EMul expr1 op expr2) typ) = do
  val1 <- compileExpr expr1
  val2 <- compileExpr expr2
  llvmId <- newVarName
  case (typ, op) of
    (Int, Times) -> emit $ L.Mul llvmId (toLLVMType Int) val1 val2
    (Int, Div) -> emit $ L.SDiv llvmId (toLLVMType Int) val1 val2
    (Int, Mod) -> emit $ L.SRem llvmId (toLLVMType Int) val1 val2
    (Doub, Times) -> emit $ L.FMul llvmId (toLLVMType Doub) val1 val2
    (Doub, Div) -> emit $ L.FDiv llvmId (toLLVMType Doub) val1 val2
  return $ L.Loc llvmId
compileExpr (ETyped (EAdd expr1 op expr2) typ) = do
  val1 <- compileExpr expr1
  val2 <- compileExpr expr2
  llvmId <- newVarName
  case (typ, op) of
    (Int, Plus) -> emit $ L.Add llvmId (toLLVMType Int) val1 val2
    (Int, Minus) -> emit $ L.Sub llvmId (toLLVMType Int) val1 val2
    (Doub, Plus) -> emit $ L.FAdd llvmId (toLLVMType Doub) val1 val2
    (Doub, Minus) -> emit $ L.FSub llvmId (toLLVMType Doub) val1 val2
  return $ L.Loc llvmId
compileExpr (ETyped (ERel expr1@(ETyped _ typ) op expr2) Bool) = do
  val1 <- compileExpr expr1
  val2 <- compileExpr expr2
  llvmId <- newVarName
  case typ of
    Int -> emit $ L.ICompare llvmId (toLLVMRelOp op) (toLLVMType Int) val1 val2
    Bool -> emit $ L.ICompare llvmId (toLLVMRelOp op) (toLLVMType Bool) val1 val2
    Doub -> emit $ L.FCompare llvmId (toLLVMFRelOp op) (toLLVMType Doub) val1 val2
  return $ L.Loc llvmId
compileExpr (ETyped (EAnd expr1 expr2) Bool) = do
  trueLabId <- newLabName
  endLabId <- newLabName
  val1 <- compileExpr expr1
  beginLabId <- getCurrentLabel
  emit $ L.Branch val1 trueLabId endLabId
  labelInstr trueLabId
  val2 <- compileExpr expr2
  resLabId <- getCurrentLabel
  emit $ L.UncondBranch endLabId
  labelInstr endLabId
  llvmId <- newVarName
  emit $ L.Phi llvmId (toLLVMType Bool) [L.PhiElem val2 resLabId, L.PhiElem (L.BConst False) beginLabId]
  return $ L.Loc llvmId
compileExpr (ETyped (EOr expr1 expr2) Bool) = do
  falseLabId <- newLabName
  endLabId <- newLabName
  val1 <- compileExpr expr1
  beginLabId <- getCurrentLabel
  emit $ L.Branch val1 endLabId falseLabId
  labelInstr falseLabId
  val2 <- compileExpr expr2
  resLabId <- getCurrentLabel
  emit $ L.UncondBranch endLabId
  labelInstr endLabId
  llvmId <- newVarName
  emit $ L.Phi llvmId (toLLVMType Bool) [L.PhiElem (L.BConst True) beginLabId, L.PhiElem val2 resLabId]
  return $ L.Loc llvmId
compileExpr (ETyped (ENewArr typ expr) (Array _)) = do
  let llvmType = toLLVMType typ
  let llvmStructType = L.Struct [L.Int 32, L.Ptr llvmType]
  len <- compileExpr expr
  llvmPtrId <- newVarName
  llvmLenId <- newVarName
  llvmMemId <- newVarName
  llvmMemExtId <- newVarName
  llvmStructId <- newVarName
  llvmSLenId <- newVarName
  llvmSPtrId <- newVarName
  llvmId <- newVarName
  emit $ L.GetElementPtr llvmPtrId llvmType L.NullPtr [L.Offset (L.Int 32) 1]
  emit $ L.PtrToInt llvmLenId (L.Ptr llvmType) (L.Loc llvmPtrId) (L.Int 32)
  emit $ L.Call llvmMemId (L.Ptr $ L.Int 8) "calloc" [L.Argument (L.Int 32) len, L.Argument (L.Int 32) (L.Loc llvmLenId)]
  emit $ L.Bitcast llvmMemExtId (L.Ptr $ L.Int 8) (L.Loc llvmMemId) (L.Ptr $ L.Int 32)
  emit $ L.Alloca llvmStructId llvmStructType
  emit $ L.GetElementPtr llvmSLenId llvmStructType (L.Loc llvmStructId) [L.Offset (L.Int 32) 0, L.Offset (L.Int 32) 0]
  emit $ L.Store (L.Int 32) len llvmSLenId
  emit $ L.GetElementPtr llvmSPtrId llvmStructType (L.Loc llvmStructId) [L.Offset (L.Int 32) 0, L.Offset (L.Int 32) 1]
  emit $ L.Store (L.Ptr llvmType) (L.Loc llvmMemExtId) llvmSPtrId
  emit $ L.Load llvmId llvmStructType llvmStructId
  return $ L.Loc llvmId
compileExpr (ETyped (EArrLen expr) Int) = do
  emit L.Blank
  return $ L.IConst 0
compileExpr _ = error "No (matching) type annotation found!"

-- * Shared functions for instruction generation

-- | Emit an instruction to allocate stack space for a local variable. Returns
-- the pointer to the allocated address. A new unique variable name for LLVM is
-- generated.
newVarInstr :: Ident -> Type -> Gen L.Ident
newVarInstr jlId typ = do
  llvmId <- newVarName
  pushVar jlId llvmId
  emit $ L.Alloca llvmId (toLLVMType typ)
  return llvmId

-- | Emit an instruction to load a variable by its name in the AST. A new unique
-- variable name for LLVM is generated. The variable has to be pushed to the
-- stack beforehand.
loadVarInstr :: Ident -> Type -> Gen L.Value
loadVarInstr jlId typ = do
  llvmId <- lookupVar jlId
  tempId <- newVarName
  emit $ L.Load tempId (toLLVMType typ) llvmId
  return $ L.Loc tempId

-- | Emit a label instruction and update the currently set label. Don't emit
-- 'L.LabelDef's manually.
labelInstr :: L.Ident -> Gen ()
labelInstr llvmLabId = do
  setCurrentLabel llvmLabId
  emit $ L.LabelDef llvmLabId

-- * Conversion functions

-- | Convert a 'Type' from the AST into an LLVM 'L.Type'
toLLVMType :: Type -> L.Type
toLLVMType Int = L.Int 32
toLLVMType Doub = L.Doub
toLLVMType Bool = L.Int 1
toLLVMType Void = L.Void
toLLVMType String = L.Ptr $ L.Int 8
toLLVMType (Array typ) = L.Struct [L.Int 32, L.Ptr (toLLVMType typ)]

-- | Convert a relational operator from the AST into an LLVM relational
-- operator.
toLLVMRelOp :: RelOp -> L.RelOp
toLLVMRelOp LTH = L.Slt
toLLVMRelOp LE = L.Sle
toLLVMRelOp GTH = L.Sgt
toLLVMRelOp GE = L.Sge
toLLVMRelOp EQU = L.Eq
toLLVMRelOp NE = L.Ne

-- | Convert a relational operator from the AST into an LLVM relational operator
-- for floating point numbers.
toLLVMFRelOp :: RelOp -> L.FRelOp
toLLVMFRelOp LTH = L.Olt
toLLVMFRelOp LE = L.Ole
toLLVMFRelOp GTH = L.Ogt
toLLVMFRelOp GE = L.Oge
toLLVMFRelOp EQU = L.Oeq
toLLVMFRelOp NE = L.One

-- | Convert an 'Ident' from the AST into an 'L.Ident' for LLVM.
toLLVMIdent :: Ident -> L.Ident
toLLVMIdent (Ident name) = L.Ident name

-- * Utility functions

-- | Extract the types from a list of typed expressions.
getTypes :: [Expr] -> [Type]
getTypes [] = []
getTypes (expr : exprs) =
  case expr of
    ETyped _ typ -> typ : getTypes exprs
    _ -> error "Not a typed expression!"

-- | Zip a list of 'L.Value's and a list of 'Type's together to create a list of
-- LLVM function arguments.
zipArgs :: [L.Value] -> [Type] -> [L.Arg]
zipArgs [] [] = []
zipArgs (val : vals) (typ : types) = L.Argument (toLLVMType typ) val : zipArgs vals types

-- * Environment handling

-- | An environment to save data used during compilation.
data Env = Env
  { vars :: [Map Ident L.Ident],
    nextVar :: Int,
    nextGlobVar :: Int,
    nextLabel :: Int,
    curLabel :: L.Ident
  }

-- | Create an empty environment.
emptyEnv :: Env
emptyEnv =
  Env
    { vars = [Map.empty],
      nextVar = 0,
      nextGlobVar = 0,
      nextLabel = 0,
      curLabel = ""
    }

-- | Add an empty entry on top of the variable stack.
newVarTop :: Gen ()
newVarTop = modify (\env -> env {vars = Map.empty : vars env})

-- | Discard the top entry of the variable stack in the environment.
discardVarTop :: Gen ()
discardVarTop = do
  env <- get
  case vars env of
    top : rest -> put env {vars = rest}
    _ -> error "Variable stack already empty!"

-- | Register a new local variable in the environment. The variable is pushed
-- into the top entry of the variable stack.
pushVar :: Ident -> L.Ident -> Gen ()
pushVar jlId llvmId = modify (\env -> env {vars = Map.insert jlId llvmId (head (vars env)) : tail (vars env)})

-- | Search for a local variable in the environment's variable stack. The
-- variable is assumed to exist, otherwise an error is thrown.
lookupVar :: Ident -> Gen L.Ident
lookupVar jlId = gets (stackLookup jlId . vars)
  where
    stackLookup :: Ident -> [Map Ident L.Ident] -> L.Ident
    stackLookup _ [] = error "Variable not found!"
    stackLookup jlId (map : stack) = case Map.lookup jlId map of
      Just llvmId -> llvmId
      Nothing -> stackLookup jlId stack

-- | Generate a new unique local variable name.
newVarName :: Gen L.Ident
newVarName = do
  env <- get
  let num = nextVar env
  put env {nextVar = num + 1}
  let llvmId = L.Ident $ "t" <> T.pack (show num)
  return llvmId

-- | Generate a new unique global variable name.
newGlobVarName :: Gen L.Ident
newGlobVarName = do
  env <- get
  let num = nextGlobVar env
  put env {nextGlobVar = num + 1}
  let llvmGlobId = L.Ident $ "g" <> T.pack (show num)
  return llvmGlobId

-- | Generate a new unique label name.
newLabName :: Gen L.Ident
newLabName = do
  env <- get
  let num = nextLabel env
  put env {nextLabel = num + 1}
  let llvmLabelId = L.Ident $ "lab" <> T.pack (show num)
  return llvmLabelId

-- | Get the label under which instructions are currently emitted.
getCurrentLabel :: Gen L.Ident
getCurrentLabel = gets curLabel

-- | Set the label under which instructions are currently emitted.
setCurrentLabel :: L.Ident -> Gen ()
setCurrentLabel llvmLabId = modify (\env -> env {curLabel = llvmLabId})
