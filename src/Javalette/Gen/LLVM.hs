{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
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
import Data.Maybe (fromJust)
import Data.Monoid (Endo (..), appEndo)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import Javalette.Check.TypeCheck (AnnotatedProg)
import Javalette.Gen.LLVM.Assembly (generateCode)
import Javalette.Gen.LLVM.Instruction (Instruction)
import qualified Javalette.Gen.LLVM.Instruction as L
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
  emitGlob $ L.FnDecl L.Void "printDouble" [L.Double]
  emitGlob $ L.FnDecl L.Void "printString" [L.Ptr $ L.Int 8]
  emitGlob $ L.FnDecl (L.Int 32) "readInt" []
  emitGlob $ L.FnDecl L.Double "readDouble" []
  emitGlob $ L.FnDecl (L.Ptr $ L.Int 8) "_calloc" [L.Int 32, L.Int 32]
  emitGlob L.Blank
  compileClassDescrs prog
  compileProg prog

compileClassDescrs :: AnnotatedProg -> Gen ()
compileClassDescrs (Program []) = return ()
compileClassDescrs (Program (FnDef {} : rest)) = compileClassDescrs $ Program rest
compileClassDescrs (Program (ClsDescr jlId _ ivars meths : rest)) = do
  llvmClsTypeId <- getClsTypeName jlId
  llvmClsDescrTypeId <- getClsDescrTypeName jlId
  llvmDescrId <- getClsDescrName jlId
  llvmVarTypes <- registerClsVars ivars 0
  fnPtrs <- registerClsMeths meths jlId 0
  emitGlob $ L.TypeDef llvmClsTypeId (L.Struct $ L.Ptr (L.Int 8) : llvmVarTypes)
  let llvmClsDescrType = L.Struct (map (\(L.FnPtr typ _) -> typ) fnPtrs)
  emitGlob $ L.TypeDef llvmClsDescrTypeId llvmClsDescrType
  emitGlob $ L.ClsDescr llvmDescrId (L.Named llvmClsDescrTypeId) fnPtrs
  compileClassDescrs $ Program rest
  where
    registerClsVars :: [ClsVar] -> Int -> Gen [L.Type]
    registerClsVars [] _ = return []
    registerClsVars (ClsVar cls typ id : meths) index = do
      addClsVar cls id index
      llvmType <- toLLVMType typ
      llvmTypes <- registerClsVars meths (index + 1)
      return $ llvmType : llvmTypes
    registerClsMeths :: [ClsMeth] -> Ident -> Int -> Gen [L.FnPtr]
    registerClsMeths [] _ _ = return []
    registerClsMeths (ClsMeth ownerCls id typ : meths) curCls index = do
      llvmType <- toLLVMType typ
      addClsMeth curCls id index typ ownerCls
      fnPtrs <- registerClsMeths meths curCls (index + 1)
      return $ L.FnPtr (L.Ptr llvmType) (L.Glob $ toFQNIdent ownerCls id) : fnPtrs

-- | Compile an type-annotated AST into a list of instructions.
compileProg :: AnnotatedProg -> Gen ()
compileProg (Program tds) = mapM_ compileTopDef tds

-- | Compile a function or class definition into a list of instructions.
compileTopDef :: TopDef -> Gen ()
compileTopDef (FnDef typ jlId params (Block stmts)) = do
  compileFn typ (toLLVMIdent jlId) params stmts
compileTopDef (ClsDescr jlId items _ _) = do
  compileClsMeths items jlId

compileClsMeths :: [ClsItem] -> Ident -> Gen ()
compileClsMeths [] _ = return ()
compileClsMeths (InstVar _ _ : rest) cls = compileClsMeths rest cls
compileClsMeths (MethDef typ id params (Block stmts) : rest) cls = do
  compileFn typ (toFQNIdent cls id) params stmts
  compileClsMeths rest cls

compileFn :: Type -> L.Ident -> [Arg] -> [Stmt] -> Gen ()
compileFn typ id params stmts = do
  llvmType <- toLLVMType typ
  llvmParams <- generateFnParamList params
  emit L.Blank
  if id == "main"
    then emit $ L.FnDefExt [] llvmType id llvmParams
    else emit $ L.FnDef llvmType id llvmParams
  labelInstr "entry"
  compileFnArgVars params llvmParams
  newVarTop
  compileStmts stmts
  if typ == Void
    then emit L.VReturn
    else emit L.Unreachable
  discardVarTop
  emit L.EndFnDef
  where
    generateFnParamList :: [Arg] -> Gen [L.Param]
    generateFnParamList [] = return []
    generateFnParamList (Argument typ jlId : rest) = do
      llvmType <- toLLVMType typ
      llvmArgId <- newVarName
      args <- generateFnParamList rest
      return $ L.Parameter llvmType llvmArgId : args
    compileFnArgVars :: [Arg] -> [L.Param] -> Gen ()
    compileFnArgVars [] [] = return ()
    compileFnArgVars (Argument typ jlId : rest1) (L.Parameter _ llvmArgId : rest2) = do
      llvmType <- toLLVMType typ
      llvmStackId <- newVarInstr jlId typ
      emit $ L.Store llvmType (L.Loc llvmArgId) llvmStackId
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
  compileDeclItems items typ
compileStmt (Ass (ETyped (ELValue lval) typ) expr) = do
  llvmType <- toLLVMType typ
  val <- compileExpr expr
  casted <- polymorphicCastInstr (getType expr) typ val
  llvmId <- lookupLVal lval typ
  emit $ L.Store llvmType casted llvmId
compileStmt (Incr (ELValue lval)) = do
  llvmIntType <- toLLVMType Int
  llvmAddrId <- lookupLVal lval Int
  llvmValId <- newVarName
  emit $ L.Load llvmValId llvmIntType llvmAddrId
  llvmResId <- newVarName
  emit $ L.Add llvmResId llvmIntType (L.Loc llvmValId) (L.IConst 1)
  emit $ L.Store llvmIntType (L.Loc llvmResId) llvmAddrId
compileStmt (Decr (ELValue lval)) = do
  llvmIntType <- toLLVMType Int
  llvmAddrId <- lookupLVal lval Int
  llvmValId <- newVarName
  emit $ L.Load llvmValId llvmIntType llvmAddrId
  llvmResId <- newVarName
  emit $ L.Sub llvmResId llvmIntType (L.Loc llvmValId) (L.IConst 1)
  emit $ L.Store llvmIntType (L.Loc llvmResId) llvmAddrId
compileStmt (Ret expr@(ETyped _ typ)) = do
  llvmType <- toLLVMType typ
  val <- compileExpr expr
  emit $ L.Return llvmType val
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
compileStmt (ForEach typ jlId expr stmt) = do
  llvmType <- toLLVMType typ -- same as toLLVMType $ Array typ
  llvmArrType <- toLLVMType $ getType expr -- same as toLLVMType $ Array typ
  topLabId <- newLabName
  loopLabId <- newLabName
  endLabId <- newLabName
  val <- compileExpr expr
  llvmVarAddrId <- newVarInstr jlId typ
  llvmIndexAddrId <- newVarName
  llvmArrLenId <- newVarName
  llvmIndexId <- newVarName
  llvmCmpId <- newVarName
  llvmArrAddrId <- newVarName
  llvmValueAddrId <- newVarName
  llvmVarId <- newVarName
  llvmIncrIndexId <- newVarName
  emit $ L.Alloca llvmIndexAddrId (L.Int 32)
  emit $ L.Store (L.Int 32) (L.IConst 0) llvmIndexAddrId
  emit $ L.ExtractValue llvmArrLenId llvmArrType val [0]
  emit $ L.UncondBranch topLabId
  labelInstr topLabId
  emit $ L.Load llvmIndexId (L.Int 32) llvmIndexAddrId
  emit $ L.ICompare llvmCmpId L.Slt (L.Int 32) (L.Loc llvmIndexId) (L.Loc llvmArrLenId)
  emit $ L.ExtractValue llvmArrAddrId llvmArrType val [1]
  emit $ L.GetElementPtr llvmValueAddrId llvmType (L.Loc llvmArrAddrId) [L.VarOffset (L.Int 32) (L.Loc llvmIndexId)]
  emit $ L.Load llvmVarId llvmType llvmValueAddrId
  emit $ L.Store llvmType (L.Loc llvmVarId) llvmVarAddrId
  emit $ L.Add llvmIncrIndexId (L.Int 32) (L.Loc llvmIndexId) (L.IConst 1)
  emit $ L.Store (L.Int 32) (L.Loc llvmIncrIndexId) llvmIndexAddrId
  emit $ L.Branch (L.Loc llvmCmpId) loopLabId endLabId
  labelInstr loopLabId
  compileStmt stmt
  emit $ L.UncondBranch topLabId
  labelInstr endLabId
compileStmt (SExpr expr) = void $ compileExpr expr

-- | Compile items of a variable declaration into a list of instructions.
compileDeclItems :: [DeclItem] -> Type -> Gen ()
compileDeclItems [] _ = return ()
compileDeclItems (item : items) typ =
  case item of
    NoInit jlId -> do
      newVarInstr jlId typ
      compileDeclItems items typ
    Init jlId expr -> do
      llvmType <- toLLVMType typ
      val <- compileExpr expr
      casted <- polymorphicCastInstr (getType expr) typ val
      llvmId <- newVarInstr jlId typ
      emit $ L.Store llvmType casted llvmId
      compileDeclItems items typ

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
compileExpr (ETyped (ELitDoub dval) Double) = return $ L.DConst dval
compileExpr (ETyped ELitTrue Boolean) = return $ L.BConst True
compileExpr (ETyped ELitFalse Boolean) = return $ L.BConst False
compileExpr (ETyped (ENull _) (Object _)) = return L.NullPtr
compileExpr (ETyped (ECall jlId exprs) Void) = do
  vals <- compileExprs exprs
  args <- zipArgs vals (getTypes exprs)
  emit $ L.VCall (L.Glob $ toLLVMIdent jlId) args
  return L.None
compileExpr (ETyped (ECall jlId exprs) typ) = do
  llvmType <- toLLVMType typ
  vals <- compileExprs exprs
  llvmId <- newVarName
  args <- zipArgs vals (getTypes exprs)
  emit $ L.Call llvmId llvmType (L.Glob $ toLLVMIdent jlId) args
  return $ L.Loc llvmId
compileExpr (ETyped (EString sval) String) = do
  let text = T.pack (sval ++ "\\00")
  let typ = L.Array (length sval + 1) (L.Int 8)
  llvmGlobId <- newStrName
  emitGlob $ L.StringDef llvmGlobId typ (L.SConst text)
  llvmLocId <- newVarName
  emit $ L.GetElementPtr llvmLocId typ (L.Glob llvmGlobId) [L.Offset (L.Int 32) 0, L.Offset (L.Int 32) 0]
  return $ L.Loc llvmLocId
compileExpr (ETyped (EArrIndex expr1 expr2) typ) = do
  llvmType <- toLLVMType typ
  llvmArrType <- toLLVMType $ Array typ
  llvmArrVal <- compileExpr expr1
  llvmIndexVal <- compileExpr expr2
  llvmArrAddrId <- newVarName
  llvmElemAddrId <- newVarName
  llvmId <- newVarName
  emit $ L.ExtractValue llvmArrAddrId llvmArrType llvmArrVal [1]
  emit $ L.GetElementPtr llvmElemAddrId llvmType (L.Loc llvmArrAddrId) [L.VarOffset (L.Int 32) llvmIndexVal]
  emit $ L.Load llvmId llvmType llvmElemAddrId
  return $ L.Loc llvmId
compileExpr (ETyped (ENeg expr) Double) = do
  llvmDoubleType <- toLLVMType Double
  val <- compileExpr expr
  tempId <- newVarName
  emit $ L.FMul tempId llvmDoubleType val (L.DConst (-1))
  return $ L.Loc tempId
compileExpr (ETyped (ENeg expr) Int) = do
  llvmIntType <- toLLVMType Int
  val <- compileExpr expr
  tempId <- newVarName
  emit $ L.Mul tempId llvmIntType val (L.IConst (-1))
  return $ L.Loc tempId
compileExpr (ETyped (ENot expr) Boolean) = do
  llvmBooleanType <- toLLVMType Boolean
  val <- compileExpr expr
  llvmId <- newVarName
  emit $ L.XOr llvmId llvmBooleanType val (L.BConst True)
  return $ L.Loc llvmId
compileExpr (ETyped (EMul expr1 op expr2) typ) = do
  llvmIntType <- toLLVMType Int
  llvmDoubleType <- toLLVMType Double
  val1 <- compileExpr expr1
  val2 <- compileExpr expr2
  llvmId <- newVarName
  case (typ, op) of
    (Int, Times) -> emit $ L.Mul llvmId llvmIntType val1 val2
    (Int, Div) -> emit $ L.SDiv llvmId llvmIntType val1 val2
    (Int, Mod) -> emit $ L.SRem llvmId llvmIntType val1 val2
    (Double, Times) -> emit $ L.FMul llvmId llvmDoubleType val1 val2
    (Double, Div) -> emit $ L.FDiv llvmId llvmDoubleType val1 val2
  return $ L.Loc llvmId
compileExpr (ETyped (EAdd expr1 op expr2) typ) = do
  llvmIntType <- toLLVMType Int
  llvmDoubleType <- toLLVMType Double
  val1 <- compileExpr expr1
  val2 <- compileExpr expr2
  llvmId <- newVarName
  case (typ, op) of
    (Int, Plus) -> emit $ L.Add llvmId llvmIntType val1 val2
    (Int, Minus) -> emit $ L.Sub llvmId llvmIntType val1 val2
    (Double, Plus) -> emit $ L.FAdd llvmId llvmDoubleType val1 val2
    (Double, Minus) -> emit $ L.FSub llvmId llvmDoubleType val1 val2
  return $ L.Loc llvmId
compileExpr (ETyped (ERel expr1@(ETyped _ typ) op expr2) Boolean) = do
  llvmIntType <- toLLVMType Int
  llvmBooleanType <- toLLVMType Boolean
  llvmDoubleType <- toLLVMType Double
  val1 <- compileExpr expr1
  val2 <- compileExpr expr2
  llvmId <- newVarName
  case typ of
    Int -> emit $ L.ICompare llvmId (toLLVMRelOp op) llvmIntType val1 val2
    Boolean -> emit $ L.ICompare llvmId (toLLVMRelOp op) llvmBooleanType val1 val2
    Double -> emit $ L.FCompare llvmId (toLLVMFRelOp op) llvmDoubleType val1 val2
  return $ L.Loc llvmId
compileExpr (ETyped (EAnd expr1 expr2) Boolean) = do
  llvmBooleanType <- toLLVMType Boolean
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
  emit $ L.Phi llvmId llvmBooleanType [L.PhiElem val2 resLabId, L.PhiElem (L.BConst False) beginLabId]
  return $ L.Loc llvmId
compileExpr (ETyped (EOr expr1 expr2) Boolean) = do
  llvmBooleanType <- toLLVMType Boolean
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
  emit $ L.Phi llvmId llvmBooleanType [L.PhiElem (L.BConst True) beginLabId, L.PhiElem val2 resLabId]
  return $ L.Loc llvmId
compileExpr (ETyped (EObjInit jlId) typ@(Object cls)) = do
  llvmClsDescrId <- getClsDescrName cls
  llvmClsTypeId <- getClsTypeName cls
  llvmClsDescrTypeId <- getClsDescrTypeName cls
  llvmRefType <- toLLVMType typ
  let llvmClsType = L.Named llvmClsTypeId
  llvmNullId <- newVarName
  llvmLenId <- newVarName
  llvmMemId <- newVarName
  llvmId <- newVarName
  llvmClsDescrDstAddrId <- newVarName
  llvmClsDescrSrcAddrId <- newVarName
  emit $ L.GetElementPtr llvmNullId llvmClsType L.NullPtr [L.Offset (L.Int 32) 1]
  emit $ L.PtrToInt llvmLenId llvmRefType (L.Loc llvmNullId) (L.Int 32)
  emit $ L.Call llvmMemId (L.Ptr $ L.Int 8) (L.Glob "_calloc") [L.Argument (L.Int 32) (L.IConst 1), L.Argument (L.Int 32) (L.Loc llvmLenId)]
  emit $ L.Bitcast llvmId (L.Ptr $ L.Int 8) (L.Loc llvmMemId) llvmRefType
  emit $ L.Bitcast llvmClsDescrSrcAddrId (L.Ptr $ L.Named llvmClsDescrTypeId) (L.Glob llvmClsDescrId) (L.Ptr $ L.Int 8)
  emit $ L.GetElementPtr llvmClsDescrDstAddrId llvmClsType (L.Loc llvmId) [L.Offset (L.Int 32) 0, L.Offset (L.Int 32) 0]
  emit $ L.Store (L.Ptr $ L.Int 8) (L.Loc llvmClsDescrSrcAddrId) llvmClsDescrDstAddrId
  return $ L.Loc llvmId
compileExpr (ETyped (EArrAlloc _ indices) typ) = compileIndexItems indices typ
compileExpr (ETyped (EMethCall expr jlId exprs) typ) = do
  let (Object cls) = getType expr
  llvmClsType <- toLLVMType $ Object cls
  llvmClsTypeId <- getClsTypeName cls
  llvmClsDescrTypeId <- getClsDescrTypeName cls
  (llvmMethIndex, fnType, ownerCls) <- getClsMeth cls jlId
  llvmOwnerClsType <- toLLVMType $ Object ownerCls
  llvmFnType <- toLLVMType fnType
  llvmRetType <- toLLVMType typ
  obj <- compileExpr expr
  vals <- compileExprs exprs
  args <- zipArgs vals (getTypes exprs)
  llvmCastObjId <- polymorphicCastInstr (Object cls) (Object ownerCls) obj
  llvmClsDescrAddrPtrId <- newVarName
  llvmClsDescrAddrId <- newVarName
  llvmClsDescrId <- newVarName
  llvmMethPtrAddrId <- newVarName
  llvmMethPtrId <- newVarName
  emit $ L.GetElementPtr llvmClsDescrAddrPtrId (L.Named llvmClsTypeId) obj [L.Offset (L.Int 32) 0, L.Offset (L.Int 32) 0]
  emit $ L.Load llvmClsDescrAddrId (L.Ptr $ L.Int 8) llvmClsDescrAddrPtrId
  emit $ L.Bitcast llvmClsDescrId (L.Ptr $ L.Int 8) (L.Loc llvmClsDescrAddrId) (L.Ptr $ L.Named llvmClsDescrTypeId)
  emit $ L.GetElementPtr llvmMethPtrAddrId (L.Named llvmClsDescrTypeId) (L.Loc llvmClsDescrId) [L.Offset (L.Int 32) 0, L.Offset (L.Int 32) llvmMethIndex]
  emit $ L.Load llvmMethPtrId (L.Ptr llvmFnType) llvmMethPtrAddrId
  case typ of
    Void -> do
      emit $ L.VCall (L.Loc llvmMethPtrId) (L.Argument llvmOwnerClsType llvmCastObjId : args)
      return L.None
    _ -> do
      llvmId <- newVarName
      emit $ L.Call llvmId llvmRetType (L.Loc llvmMethPtrId) (L.Argument llvmOwnerClsType llvmCastObjId : args)
      return $ L.Loc llvmId
compileExpr (ETyped (EArrLen expr) Int) = do
  llvmArrType <- toLLVMType $ getType expr
  val <- compileExpr expr
  llvmId <- newVarName
  emit $ L.ExtractValue llvmId llvmArrType val [0]
  return $ L.Loc llvmId
compileExpr expr = error $ "No (matching) type annotation found! Expression is: " ++ show expr

-- | Compile a list of dimension size specifications for an array allocation
-- into a list of instructions. For multiple dimensions, loops over the
-- allocated array to allocate all sub-arrays and initialise the parent array
-- elements with the corresponding sub-array pointers.
compileIndexItems :: [SizeItem] -> Type -> Gen L.Value
compileIndexItems [item] (Array typ) = compileSizeItem item typ
compileIndexItems (item : rest) (Array typ) = do
  llvmArrType <- toLLVMType $ Array typ
  llvmType <- toLLVMType typ
  arr <- compileSizeItem item typ
  topLabId <- newLabName
  loopLabId <- newLabName
  endLabId <- newLabName
  llvmIndexAddrId <- newVarName
  llvmArrLenId <- newVarName
  llvmIndexId <- newVarName
  llvmCmpId <- newVarName
  llvmArrAddrId <- newVarName
  llvmValueAddrId <- newVarName
  llvmVarId <- newVarName
  llvmIncrIndexId <- newVarName
  emit $ L.Alloca llvmIndexAddrId (L.Int 32)
  emit $ L.Store (L.Int 32) (L.IConst 0) llvmIndexAddrId
  emit $ L.ExtractValue llvmArrLenId llvmArrType arr [0]
  emit $ L.UncondBranch topLabId
  labelInstr topLabId
  emit $ L.Load llvmIndexId (L.Int 32) llvmIndexAddrId
  emit $ L.ICompare llvmCmpId L.Slt (L.Int 32) (L.Loc llvmIndexId) (L.Loc llvmArrLenId)
  emit $ L.Add llvmIncrIndexId (L.Int 32) (L.Loc llvmIndexId) (L.IConst 1)
  emit $ L.Store (L.Int 32) (L.Loc llvmIncrIndexId) llvmIndexAddrId
  emit $ L.Branch (L.Loc llvmCmpId) loopLabId endLabId
  labelInstr loopLabId
  subArr <- compileIndexItems rest typ
  emit $ L.ExtractValue llvmArrAddrId llvmArrType arr [1]
  emit $ L.GetElementPtr llvmValueAddrId llvmType (L.Loc llvmArrAddrId) [L.VarOffset (L.Int 32) (L.Loc llvmIndexId)]
  emit $ L.Store llvmType subArr llvmValueAddrId
  emit $ L.UncondBranch topLabId
  labelInstr endLabId
  return arr

-- | Compile a single array dimension size specification into a list of
-- instructions. Allocates an array of the given size and type.
compileSizeItem :: SizeItem -> Type -> Gen L.Value
compileSizeItem (SizeSpec expr) typ = do
  llvmType <- toLLVMType typ
  let llvmArrType = L.Struct [L.Int 32, L.Ptr llvmType]
  len <- compileExpr expr
  llvmNullId <- newVarName
  llvmLenId <- newVarName
  llvmMemId <- newVarName
  llvmMemExtId <- newVarName
  llvmLenEntryId <- newVarName
  llvmId <- newVarName
  emit $ L.GetElementPtr llvmNullId llvmType L.NullPtr [L.Offset (L.Int 32) 1]
  emit $ L.PtrToInt llvmLenId (L.Ptr llvmType) (L.Loc llvmNullId) (L.Int 32)
  emit $ L.Call llvmMemId (L.Ptr $ L.Int 8) (L.Glob "_calloc") [L.Argument (L.Int 32) len, L.Argument (L.Int 32) (L.Loc llvmLenId)]
  emit $ L.Bitcast llvmMemExtId (L.Ptr $ L.Int 8) (L.Loc llvmMemId) (L.Ptr llvmType)
  emit $ L.InsertValue llvmLenEntryId llvmArrType (L.CConst [(L.Int 32, L.Undef), (L.Ptr llvmType, L.Undef)]) (L.Int 32) len [0]
  emit $ L.InsertValue llvmId llvmArrType (L.Loc llvmLenEntryId) (L.Ptr llvmType) (L.Loc llvmMemExtId) [1]
  return $ L.Loc llvmId

-- * Shared functions for instruction generation

-- | Emit an instruction to allocate stack space for a local variable. Returns
-- the pointer to the allocated address. A new unique variable name for LLVM is
-- generated.
newVarInstr :: Ident -> Type -> Gen L.Ident
newVarInstr jlId typ = do
  llvmType <- toLLVMType typ
  llvmId <- newVarName
  pushVar jlId llvmId
  emit $ L.Alloca llvmId llvmType
  return llvmId

-- | Emit an instruction to load a variable by its name in the AST. A new unique
-- variable name for LLVM is generated. The variable has to be pushed to the
-- stack beforehand.
loadVarInstr :: Ident -> Type -> Gen L.Value
loadVarInstr jlId typ = do
  llvmType <- toLLVMType typ
  llvmId <- lookupVar jlId
  tempId <- newVarName
  emit $ L.Load tempId llvmType llvmId
  return $ L.Loc tempId

-- | Look up the address of an lvalue. If a plain variable, the variable stack
-- is searched as usual. If indexed, appropriate instructions are emitted.
lookupLVal :: LValue -> Type -> Gen L.Ident
lookupLVal (ArrId lval expr) typ = do
  llvmType <- toLLVMType typ
  llvmArrType <- toLLVMType $ Array typ
  val <- compileExpr expr
  llvmBaseId <- lookupLVal lval (Array typ)
  llvmArrId <- newVarName
  llvmArrAddrId <- newVarName
  llvmId <- newVarName
  emit $ L.Load llvmArrId llvmArrType llvmBaseId
  emit $ L.ExtractValue llvmArrAddrId llvmArrType (L.Loc llvmArrId) [1]
  emit $ L.GetElementPtr llvmId llvmType (L.Loc llvmArrAddrId) [L.VarOffset (L.Int 32) val]
  return llvmId
lookupLVal (Id jlId) _ = lookupVar jlId

-- | Emit a label instruction and update the currently set label. Don't emit
-- 'L.LabelDef's manually.
labelInstr :: L.Ident -> Gen ()
labelInstr llvmLabId = do
  setCurrentLabel llvmLabId
  emit $ L.LabelDef llvmLabId

polymorphicCastInstr :: Type -> Type -> L.Value -> Gen L.Value
polymorphicCastInstr from into val =
  case (from, into) of
    (Object _, Object _) -> do
      llvmFromType <- toLLVMType from
      llvmIntoType <- toLLVMType into
      llvmId <- newVarName
      emit $ L.Bitcast llvmId llvmFromType val llvmIntoType
      return $ L.Loc llvmId
    _ -> return val

-- * Conversion functions

-- | Convert a 'Type' from the AST into an LLVM 'L.Type'
toLLVMType :: Type -> Gen L.Type
toLLVMType Int = return $ L.Int 32
toLLVMType Double = return L.Double
toLLVMType Boolean = return $ L.Int 1
toLLVMType Void = return L.Void
toLLVMType String = return $ L.Ptr $ L.Int 8
toLLVMType (Array typ) = do
  inner <- toLLVMType typ
  return $ L.Struct [L.Int 32, L.Ptr inner]
toLLVMType (Fn ret params) = do
  llvmRet <- toLLVMType ret
  llvmParams <- mapM toLLVMType params
  return $ L.Fn llvmRet llvmParams
toLLVMType (Object jlId) = do
  llvmId <- getClsTypeName jlId
  return $ L.Ptr $ L.Named llvmId

-- clsVars <- getClsVars jlId
-- llvmVars <- mapM (toLLVMType . fst) clsVars
-- return $ L.Ptr $ L.Struct llvmVars

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

toFQNIdent :: Ident -> Ident -> L.Ident
toFQNIdent (Ident cls) (Ident fn) = L.Ident $ T.concat [cls, ".", fn]

-- * Utility functions

-- | Extract the type of a single typed expression.
getType :: Expr -> Type
getType (ETyped _ typ) = typ
getType _ = error "Not a typed expression!"

-- | Extract the types from a list of typed expressions.
getTypes :: [Expr] -> [Type]
getTypes = map getType

-- | Get the identifier used in the lvalue.
unwrapLVal :: LValue -> Ident
unwrapLVal (ArrId lval _) = unwrapLVal lval
unwrapLVal (Id id) = id

-- | Zip a list of 'L.Value's and a list of 'Type's together to create a list of
-- LLVM function arguments.
zipArgs :: [L.Value] -> [Type] -> Gen [L.Arg]
zipArgs [] [] = return []
zipArgs (val : vals) (typ : types) = do
  llvmType <- toLLVMType typ
  args <- zipArgs vals types
  return $ L.Argument llvmType val : args

-- * Environment handling

-- | An environment to save data used during compilation.
data Env = Env
  { vars :: [Map Ident L.Ident],
    -- clsDescrs :: Map Ident L.Ident, -- ^ Map class name to class descriptor

    -- | Map method (and class) name to index in class descriptor
    -- clsTypes :: Map Ident L.Ident, -- ^ Map class name to type name
    clsMeths :: Map (Ident, Ident) (Int, Type, Ident),
    -- | Map variable (and class) name to index in type
    clsVarInds :: Map (Ident, Ident) Int,
    nextVar :: Int,
    nextGlobVar :: Int,
    nextStrConst :: Int,
    nextLabel :: Int,
    curLabel :: L.Ident
  }

-- | Create an empty environment.
emptyEnv :: Env
emptyEnv =
  Env
    { vars = [Map.empty],
      -- clsDescrs = Map.empty,
      clsMeths = Map.empty,
      clsVarInds = Map.empty,
      nextVar = 0,
      nextGlobVar = 0,
      nextStrConst = 0,
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
lookupVar id = gets (stackLookup id . vars)
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
  return $ L.Ident $ (T.append "t" . T.pack) (show num)

-- | Generate a new unique global variable name.
newGlobVarName :: Gen L.Ident
newGlobVarName = do
  env <- get
  let num = nextGlobVar env
  put env {nextGlobVar = num + 1}
  return $ L.Ident $ (T.append "_g" . T.pack) (show num)

-- | Generate a new unique global variable name for strings.
newStrName :: Gen L.Ident
newStrName = do
  env <- get
  let num = nextStrConst env
  put env {nextStrConst = num + 1}
  return $ L.Ident $ (T.append "_s" . T.pack) (show num)

-- | Generate a new unique label name.
newLabName :: Gen L.Ident
newLabName = do
  env <- get
  let num = nextLabel env
  put env {nextLabel = num + 1}
  return $ L.Ident $ (T.append "lab" . T.pack) (show num)

-- | Get the unique name of a class descriptor.
getClsDescrName :: Ident -> Gen L.Ident
getClsDescrName (Ident name) = return $ L.Ident $ T.append "_cd." name

-- | Get the unique name of a class descriptor.
getClsTypeName :: Ident -> Gen L.Ident
getClsTypeName (Ident name) = return $ L.Ident $ T.append "_ty." name

getClsDescrTypeName :: Ident -> Gen L.Ident
getClsDescrTypeName (Ident name) = return $ L.Ident $ T.append "_ty.cd." name

-- | Get the label under which instructions are currently emitted.
getCurrentLabel :: Gen L.Ident
getCurrentLabel = gets curLabel

-- | Set the label under which instructions are currently emitted.
setCurrentLabel :: L.Ident -> Gen ()
setCurrentLabel llvmLabId = modify (\env -> env {curLabel = llvmLabId})

-- addClsDescr :: Ident -> L.Ident -> Gen ()
-- addClsDescr jlId llvmId = modify (\env -> env {clsDescrs = Map.insert jlId llvmId (clsDescrs env)})

addClsMeth :: Ident -> Ident -> Int -> Type -> Ident -> Gen ()
addClsMeth clsId jlId index typ ownerId = modify (\env -> env {clsMeths = Map.insert (clsId, jlId) (index, typ, ownerId) (clsMeths env)})

getClsMeth :: Ident -> Ident -> Gen (Int, Type, Ident)
getClsMeth clsId jlId = gets $ fromJust . Map.lookup (clsId, jlId) . clsMeths

-- addClsType :: Ident -> Ident -> Gen ()
-- addClsType jlId tyId = modify (\env -> env {clsTypes = Map.insert jlId tyId (clsTypes env)})

addClsVar :: Ident -> Ident -> Int -> Gen ()
addClsVar clsId jlId index = modify (\env -> env {clsVarInds = Map.insert (clsId, jlId) index (clsVarInds env)})

-- getClsVars :: Ident -> Gen [(Type, Ident)]
-- getClsVars cls = do
--   ctx <- get
--   traceShowM $ clsVars ctx
--   gets $ fromJust . Map.lookup cls . clsVars . traceShow cls

-- addClsVars :: Ident -> (Type, Ident) -> Gen ()
-- addClsVars cls entry = do
--   ctx <- get
--   case Map.lookup cls (clsVars ctx) of
--     Just entries -> modify (\env -> env {clsVars = Map.insert cls (entry : entries) (clsVars ctx)})
--     Nothing -> modify (\env -> env {clsVars = Map.insert cls [entry] (clsVars ctx)})
