{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_HADDOCK prune, ignore-exports, show-extensions #-}

-- | Compile a Javalette AST into LLVM IR.
--
-- In a first pass, an LLVM AST is created as a list of 'Instruction's. These
-- instructions are converted into LLVM assembly code in the second pass.
module Javalette.Gen.LLVM
  ( generateIR,
  )
where

import Control.Monad (liftM2, void, zipWithM_)
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
import Data.Foldable (asum)
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
import Javalette.Check.TypedAST

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
-- statements for runtime function declarations and class descriptors.
compile :: AnnotatedProg -> Gen ()
compile prog = do
  emitGlob $ L.FnDecl L.Void "printInt" [L.Int 32]
  emitGlob $ L.FnDecl L.Void "printDouble" [L.Double]
  emitGlob $ L.FnDecl L.Void "printString" [L.Ptr $ L.Int 8]
  emitGlob $ L.FnDecl (L.Int 32) "readInt" []
  emitGlob $ L.FnDecl L.Double "readDouble" []
  emitGlob $ L.FnDecl (L.Ptr $ L.Int 8) "_calloc" [L.Int 32, L.Int 32]
  emitGlob L.Blank
  compileDscs prog
  compileProg prog

-- | Read struct and class 'TopDef's and emit the corresponding LLVM types
-- and constants into the global namespace. Registers the index of instance
-- variables as well as the index, type, and owner class of methods with the
-- compilation environment.
compileDscs :: AnnotatedProg -> Gen ()
compileDscs (Program []) = return ()
compileDscs (Program (FnDef {} : rest)) = compileDscs $ Program rest
compileDscs (Program (StrDef strId flds : rest)) = do
  llvmStrTypeId <- getStrTypeName strId
  llvmFldTypes <- registerStrFlds flds strId 0
  emitGlob $ L.TypeDef llvmStrTypeId (L.Struct llvmFldTypes)
  compileDscs $ Program rest
  where
    registerStrFlds :: [StrItem] -> TypeIdent -> Int -> Gen [L.Type]
    registerStrFlds [] _ _ = return []
    registerStrFlds (StrFld typ jlId : flds) strId index = do
      addStrFld strId jlId index
      llvmType <- toLLVMType typ
      llvmTypes <- registerStrFlds flds strId (index + 1)
      return $ llvmType : llvmTypes
compileDscs (Program (ClsDef clsId _ vars meths : rest)) = do
  llvmClsTypeId <- getClsTypeName clsId
  llvmDscTypeId <- getDcsTypeName clsId
  llvmDscId <- getDcsName clsId
  llvmVarTypes <- registerClsVars vars 1
  fnPtrs <- registerClsMeths meths clsId 0
  emitGlob $ L.TypeDef llvmClsTypeId (L.Struct $ L.Ptr (L.Int 8) : llvmVarTypes)
  let llvmDscType = L.Struct (map (\(L.FnPtr typ _) -> typ) fnPtrs)
  emitGlob $ L.TypeDef llvmDscTypeId llvmDscType
  emitGlob $ L.ClsDescr llvmDscId (L.Named llvmDscTypeId) fnPtrs
  compileDscs $ Program rest
  where
    registerClsVars :: [ClsVar] -> Int -> Gen [L.Type]
    registerClsVars [] _ = return []
    registerClsVars (ClsVar clsId typ jlId : vars) index = do
      addClsVar clsId jlId index
      llvmType <- toLLVMType typ
      llvmTypes <- registerClsVars vars (index + 1)
      return $ llvmType : llvmTypes
    registerClsMeths :: [ClsMeth] -> TypeIdent -> Int -> Gen [L.FnPtr]
    registerClsMeths [] _ _ = return []
    registerClsMeths (ClsMeth ownerCls jlId typ : meths) curCls index = do
      llvmType <- toLLVMType typ
      addClsMeth curCls jlId index
      fnPtrs <- registerClsMeths meths curCls (index + 1)
      return $ L.FnPtr (L.Ptr llvmType) (L.Glob $ toFQNIdent ownerCls jlId) : fnPtrs

-- | Compile an type-annotated AST into a list of instructions.
compileProg :: AnnotatedProg -> Gen ()
compileProg (Program tds) = mapM_ compileTopDef tds

-- | Compile a function or methods in a class definition into a list of
-- instructions.
compileTopDef :: TopDef -> Gen ()
compileTopDef (FnDef typ jlId params (Block stmts)) = do
  compileFn typ (toLLVMIdent jlId) params stmts
compileTopDef StrDef {} = return ()
compileTopDef (ClsDef clsId items _ _) = do
  enterCls clsId
  mapM_
    ( \case
        MethDef typ id params (Block stmts) ->
          compileFn typ (toFQNIdent clsId id) params stmts
        _ -> return ()
    )
    items
  leaveCls

-- | Compile a function or method into a list of instructions.
compileFn :: Type -> L.Ident -> [Param] -> [Stmt] -> Gen ()
compileFn typ id params stmts = do
  llvmType <- toLLVMType typ
  llvmParams <- mapM generateFnParam params
  emit L.Blank
  if id == "main"
    then emit $ L.FnDefExt [] llvmType id llvmParams
    else emit $ L.FnDef llvmType id llvmParams
  labelInstr "entry"
  zipWithM_ compileFnArgVar params (map (\(L.Parameter _ llvmArgId) -> llvmArgId) llvmParams)
  newVarTop
  compileStmts stmts
  if typ == Void
    then emit L.VReturn
    else emit L.Unreachable
  discardVarTop
  emit L.EndFnDef
  where
    -- Convert a list of Javalette function arguments into a list of LLVM
    -- function params by converting the type and generating a name.
    generateFnParam :: Param -> Gen L.Param
    generateFnParam (Parameter typ jlId) = liftM2 L.Parameter (toLLVMType typ) newVarName
    -- Emit instructions for allocating stack space and storing the value of
    -- function parameters.
    compileFnArgVar :: Param -> L.Ident -> Gen ()
    compileFnArgVar (Parameter typ jlId) llvmArgId = do
      llvmType <- toLLVMType typ
      llvmStackId <- newVarInstr jlId typ
      emit $ L.Store llvmType (L.Loc llvmArgId) llvmStackId

-- | Compile a list of statements into a list of instructions.
compileStmts :: [Stmt] -> Gen ()
compileStmts = mapM_ compileStmt

-- | Compile a function definition into a list of instructions.
compileStmt :: Stmt -> Gen ()
compileStmt Empty = return ()
compileStmt (BStmt (Block stmts)) = do
  newVarTop
  compileStmts stmts
  discardVarTop
compileStmt (Decl typ items) = do
  compileDeclItems items typ
compileStmt (Ass lval expr) = do
  let typ = getLValType lval
  llvmType <- toLLVMType typ
  val <- compileExpr expr
  casted <- polymorphicCastInstr (getType expr) typ val
  llvmId <- lookupLVal lval
  emit $ L.Store llvmType casted llvmId
compileStmt (Incr lval) = do
  llvmIntType <- toLLVMType Int
  llvmAddrId <- lookupLVal lval
  llvmValId <- instr $ \id -> L.Load id llvmIntType llvmAddrId
  llvmResId <- instr $ \id -> L.Add id llvmIntType (L.Loc llvmValId) (L.IConst 1)
  emit $ L.Store llvmIntType (L.Loc llvmResId) llvmAddrId
compileStmt (Decr lval) = do
  llvmIntType <- toLLVMType Int
  llvmAddrId <- lookupLVal lval
  llvmValId <- instr $ \id -> L.Load id llvmIntType llvmAddrId
  llvmResId <- instr $ \id -> L.Sub id llvmIntType (L.Loc llvmValId) (L.IConst 1)
  emit $ L.Store llvmIntType (L.Loc llvmResId) llvmAddrId
compileStmt (Ret expr) = do
  llvmType <- toLLVMType $ getType expr
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
  llvmType <- toLLVMType typ
  llvmArrType <- toLLVMType $ getType expr
  topLabId <- newLabName
  val <- compileExpr expr
  loopLabId <- newLabName
  endLabId <- newLabName
  llvmVPtrId <- newVarInstr jlId typ
  llvmIPtrId <- instr $ \id -> L.Alloca id (L.Int 32)
  emit $ L.Store (L.Int 32) (L.IConst 0) llvmIPtrId
  llvmLValId <- instr $ \id -> L.ExtractValue id llvmArrType val [0]
  emit $ L.UncondBranch topLabId
  labelInstr topLabId
  llvmIValId <- instr $ \id -> L.Load id (L.Int 32) llvmIPtrId
  llvmCResId <- instr $ \id -> L.ICompare id L.Slt (L.Int 32) (L.Loc llvmIValId) (L.Loc llvmLValId)
  llvmAPtrId <- instr $ \id -> L.ExtractValue id llvmArrType val [1]
  llvmRPtrId <- instr $ \id -> L.GetElementPtr id llvmType (L.Loc llvmAPtrId) [L.VarOffset (L.Int 32) (L.Loc llvmIValId)]
  llvmRValId <- instr $ \id -> L.Load id llvmType llvmRPtrId
  emit $ L.Store llvmType (L.Loc llvmRValId) llvmVPtrId
  llvmIndIncId <- instr $ \id -> L.Add id (L.Int 32) (L.Loc llvmIValId) (L.IConst 1)
  emit $ L.Store (L.Int 32) (L.Loc llvmIndIncId) llvmIPtrId
  emit $ L.Branch (L.Loc llvmCResId) loopLabId endLabId
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
compileExprs :: [TExpr] -> Gen [L.Value]
compileExprs = mapM compileExpr

-- | Compile an expression into a list of instructions.
compileExpr :: TExpr -> Gen L.Value
compileExpr (EVar jlId typ) = loadVarInstr jlId typ
compileExpr (ELitInt ival) = return $ L.IConst (fromInteger ival)
compileExpr (ELitDouble dval) = return $ L.DConst dval
compileExpr ELitTrue = return $ L.BConst True
compileExpr ELitFalse = return $ L.BConst False
compileExpr (EString sval) = do
  let text = T.pack (sval ++ "\\00")
  let typ = L.Array (length sval + 1) (L.Int 8)
  llvmGlobId <- newStrName
  emitGlob $ L.StringDef llvmGlobId typ (L.SConst text)
  L.Loc <$> instr (\id -> L.GetElementPtr id typ (L.Glob llvmGlobId) [L.Offset (L.Int 32) 0, L.Offset (L.Int 32) 0])
compileExpr (ENull _ _) = return L.NullPtr
compileExpr (ECall jlId exprs Void) = do
  vals <- compileExprs exprs
  args <- zipArgs vals (map getType exprs)
  emit $ L.VCall (L.Glob $ toLLVMIdent jlId) args
  return L.None
compileExpr (ECall jlId exprs typ) = do
  llvmType <- toLLVMType typ
  vals <- compileExprs exprs
  args <- zipArgs vals (map getType exprs)
  L.Loc <$> instr (\id -> L.Call id llvmType (L.Glob $ toLLVMIdent jlId) args)
compileExpr (ENeg expr Double) = do
  llvmDoubleType <- toLLVMType Double
  val <- compileExpr expr
  L.Loc <$> instr (\id -> L.FMul id llvmDoubleType val (L.DConst (-1)))
compileExpr (ENeg expr Int) = do
  llvmIntType <- toLLVMType Int
  val <- compileExpr expr
  L.Loc <$> instr (\id -> L.Mul id llvmIntType val (L.IConst (-1)))
compileExpr (ENot expr) = do
  llvmBooleanType <- toLLVMType Boolean
  val <- compileExpr expr
  L.Loc <$> instr (\id -> L.XOr id llvmBooleanType val (L.BConst True))
compileExpr (EMul expr1 op expr2 typ) = do
  llvmType <- toLLVMType typ
  val1 <- compileExpr expr1
  val2 <- compileExpr expr2
  case (typ, op) of
    (Int, Times) -> L.Loc <$> instr (\id -> L.Mul id llvmType val1 val2)
    (Int, Div) -> L.Loc <$> instr (\id -> L.SDiv id llvmType val1 val2)
    (Int, Mod) -> L.Loc <$> instr (\id -> L.SRem id llvmType val1 val2)
    (Double, Times) -> L.Loc <$> instr (\id -> L.FMul id llvmType val1 val2)
    (Double, Div) -> L.Loc <$> instr (\id -> L.FDiv id llvmType val1 val2)
compileExpr (EAdd expr1 op expr2 typ) = do
  llvmType <- toLLVMType typ
  val1 <- compileExpr expr1
  val2 <- compileExpr expr2
  case (typ, op) of
    (Int, Plus) -> L.Loc <$> instr (\id -> L.Add id llvmType val1 val2)
    (Int, Minus) -> L.Loc <$> instr (\id -> L.Sub id llvmType val1 val2)
    (Double, Plus) -> L.Loc <$> instr (\id -> L.FAdd id llvmType val1 val2)
    (Double, Minus) -> L.Loc <$> instr (\id -> L.FSub id llvmType val1 val2)
compileExpr (ERel expr1 op expr2) = do
  let llvmRelOp = toLLVMRelOp op
  let typ1 = getType expr1
  llvmType <- toLLVMType typ1
  val1 <- compileExpr expr1
  val2 <- compileExpr expr2
  case typ1 of
    Int -> L.Loc <$> instr (\id -> L.ICompare id llvmRelOp llvmType val1 val2)
    Boolean -> L.Loc <$> instr (\id -> L.ICompare id llvmRelOp llvmType val1 val2)
    Double -> L.Loc <$> instr (\id -> L.FCompare id (toLLVMFRelOp op) llvmType val1 val2)
    Ptr (Struct _) -> do
      llvmStr1Id <- instr $ \id -> L.PtrToInt id llvmType val1 (L.Int 32)
      llvmStr2Id <- instr $ \id -> L.PtrToInt id llvmType val2 (L.Int 32)
      L.Loc <$> instr (\id -> L.ICompare id llvmRelOp (L.Int 32) (L.Loc llvmStr1Id) (L.Loc llvmStr2Id))
    Object _ -> do
      llvmType2 <- toLLVMType $ getType expr2
      llvmObj1Id <- instr $ \id -> L.PtrToInt id llvmType val1 (L.Int 32)
      llvmObj2Id <- instr $ \id -> L.PtrToInt id llvmType2 val2 (L.Int 32)
      L.Loc <$> instr (\id -> L.ICompare id llvmRelOp (L.Int 32) (L.Loc llvmObj1Id) (L.Loc llvmObj2Id))
compileExpr (EAnd expr1 expr2) = do
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
  L.Loc <$> instr (\id -> L.Phi id llvmBooleanType [L.PhiElem val2 resLabId, L.PhiElem (L.BConst False) beginLabId])
compileExpr (EOr expr1 expr2) = do
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
  L.Loc <$> instr (\id -> L.Phi id llvmBooleanType [L.PhiElem (L.BConst True) beginLabId, L.PhiElem val2 resLabId])
compileExpr (EArrAlloc _ indices typ) = compileIndexItems indices typ
compileExpr (EArrLen expr) = do
  llvmArrType <- toLLVMType $ getType expr
  val <- compileExpr expr
  L.Loc <$> instr (\id -> L.ExtractValue id llvmArrType val [0])
compileExpr (EArrIndex expr1 expr2 typ) = do
  llvmType <- toLLVMType typ
  llvmArrType <- toLLVMType $ Array typ
  llvmArrVal <- compileExpr expr1
  llvmIndVal <- compileExpr expr2
  llvmArrId <- instr $ \id -> L.ExtractValue id llvmArrType llvmArrVal [1]
  llvmElmId <- instr $ \id -> L.GetElementPtr id llvmType (L.Loc llvmArrId) [L.VarOffset (L.Int 32) llvmIndVal]
  L.Loc <$> instr (\id -> L.Load id llvmType llvmElmId)
compileExpr (EStrInit typ@(Struct strId)) = do
  llvmStrType <- toLLVMType typ
  let llvmPtrType = L.Ptr llvmStrType
  llvmNulId <- instr $ \id -> L.GetElementPtr id llvmStrType L.NullPtr [L.Offset (L.Int 32) 1]
  llvmLenId <- instr $ \id -> L.PtrToInt id llvmPtrType (L.Loc llvmNulId) (L.Int 32)
  llvmMemId <- instr $ \id -> L.Call id (L.Ptr $ L.Int 8) (L.Glob "_calloc") [L.Argument (L.Int 32) (L.IConst 1), L.Argument (L.Int 32) (L.Loc llvmLenId)]
  L.Loc <$> instr (\id -> L.Bitcast id (L.Ptr $ L.Int 8) (L.Loc llvmMemId) llvmPtrType)
compileExpr (EDeref expr jlId typ) = do
  let (Ptr (Struct strId)) = getType expr
  llvmStrType <- toLLVMType $ Struct strId
  llvmFldType <- toLLVMType typ
  fldInd <- getStrFld strId jlId
  llvmStrVal <- compileExpr expr
  llvmPtrId <- instr $ \id -> L.GetElementPtr id llvmStrType llvmStrVal [L.Offset (L.Int 32) 0, L.VarOffset (L.Int 32) (L.IConst fldInd)]
  L.Loc <$> instr (\id -> L.Load id llvmFldType llvmPtrId)
compileExpr (EObjInit typ@(Object clsId)) = do
  llvmClsType <- L.Named <$> getClsTypeName clsId
  llvmDscType <- L.Named <$> getDcsTypeName clsId
  llvmRefType <- toLLVMType typ
  llvmDscId <- getDcsName clsId
  llvmNulId <- instr $ \id -> L.GetElementPtr id llvmClsType L.NullPtr [L.Offset (L.Int 32) 1]
  llvmLenId <- instr $ \id -> L.PtrToInt id llvmRefType (L.Loc llvmNulId) (L.Int 32)
  llvmMemId <- instr $ \id -> L.Call id (L.Ptr $ L.Int 8) (L.Glob "_calloc") [L.Argument (L.Int 32) (L.IConst 1), L.Argument (L.Int 32) (L.Loc llvmLenId)]
  llvmObjId <- instr $ \id -> L.Bitcast id (L.Ptr $ L.Int 8) (L.Loc llvmMemId) llvmRefType
  llvmSrcId <- instr $ \id -> L.Bitcast id (L.Ptr llvmDscType) (L.Glob llvmDscId) (L.Ptr $ L.Int 8)
  llvmDstId <- instr $ \id -> L.GetElementPtr id llvmClsType (L.Loc llvmObjId) [L.Offset (L.Int 32) 0, L.Offset (L.Int 32) 0]
  emit $ L.Store (L.Ptr $ L.Int 8) (L.Loc llvmSrcId) llvmDstId
  return $ L.Loc llvmObjId
compileExpr (EMethCall expr ownerCls jlId exprs fnType typ) = do
  let (Object clsId) = getType expr
  llvmClsType <- L.Named <$> getClsTypeName clsId
  llvmDscType <- L.Named <$> getDcsTypeName clsId
  metInd <- getClsMeth clsId jlId
  llvmOwnType <- toLLVMType $ Object ownerCls
  llvmFunType <- toLLVMType fnType
  llvmRetType <- toLLVMType typ
  obj <- compileExpr expr
  vals <- compileExprs exprs
  args <- zipArgs vals (map getType exprs)
  llvmObjCId <- polymorphicCastInstr (Object clsId) (Object ownerCls) obj
  llvmDscAId <- instr $ \id -> L.GetElementPtr id llvmClsType obj [L.Offset (L.Int 32) 0, L.Offset (L.Int 32) 0]
  llvmDscPId <- instr $ \id -> L.Load id (L.Ptr $ L.Int 8) llvmDscAId
  llvmDscCId <- instr $ \id -> L.Bitcast id (L.Ptr $ L.Int 8) (L.Loc llvmDscPId) (L.Ptr llvmDscType)
  llvmMetAId <- instr $ \id -> L.GetElementPtr id llvmDscType (L.Loc llvmDscCId) [L.Offset (L.Int 32) 0, L.Offset (L.Int 32) metInd]
  llvmMetPId <- instr $ \id -> L.Load id (L.Ptr llvmFunType) llvmMetAId
  case typ of
    Void -> do
      emit $ L.VCall (L.Loc llvmMetPId) (L.Argument llvmOwnType llvmObjCId : args)
      return L.None
    _ -> L.Loc <$> instr (\id -> L.Call id llvmRetType (L.Loc llvmMetPId) (L.Argument llvmOwnType llvmObjCId : args))
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
  llvmPtrId <- instr $ \id -> L.Alloca id (L.Int 32)
  emit $ L.Store (L.Int 32) (L.IConst 0) llvmPtrId
  llvmLenId <- instr $ \id -> L.ExtractValue id llvmArrType arr [0]
  emit $ L.UncondBranch topLabId
  labelInstr topLabId
  llvmIndId <- instr $ \id -> L.Load id (L.Int 32) llvmPtrId
  llvmCmpId <- instr $ \id -> L.ICompare id L.Slt (L.Int 32) (L.Loc llvmIndId) (L.Loc llvmLenId)
  llvmIppId <- instr $ \id -> L.Add id (L.Int 32) (L.Loc llvmIndId) (L.IConst 1)
  emit $ L.Store (L.Int 32) (L.Loc llvmIppId) llvmPtrId
  emit $ L.Branch (L.Loc llvmCmpId) loopLabId endLabId
  labelInstr loopLabId
  subArr <- compileIndexItems rest typ
  llvmArrId <- instr $ \id -> L.ExtractValue id llvmArrType arr [1]
  llvmValId <- instr $ \id -> L.GetElementPtr id llvmType (L.Loc llvmArrId) [L.VarOffset (L.Int 32) (L.Loc llvmIndId)]
  emit $ L.Store llvmType subArr llvmValId
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
  llvmNulId <- instr $ \id -> L.GetElementPtr id llvmType L.NullPtr [L.Offset (L.Int 32) 1]
  llvmLenId <- instr $ \id -> L.PtrToInt id (L.Ptr llvmType) (L.Loc llvmNulId) (L.Int 32)
  llvmAllId <- instr $ \id -> L.Call id (L.Ptr $ L.Int 8) (L.Glob "_calloc") [L.Argument (L.Int 32) len, L.Argument (L.Int 32) (L.Loc llvmLenId)]
  llvmMemId <- instr $ \id -> L.Bitcast id (L.Ptr $ L.Int 8) (L.Loc llvmAllId) (L.Ptr llvmType)
  llvmArrId <- instr $ \id -> L.InsertValue id llvmArrType (L.CConst [(L.Int 32, L.Undef), (L.Ptr llvmType, L.Undef)]) (L.Int 32) len [0]
  L.Loc <$> instr (\id -> L.InsertValue id llvmArrType (L.Loc llvmArrId) (L.Ptr llvmType) (L.Loc llvmMemId) [1])

-- * Shared functions for instruction generation

-- | Emit an instruction that returns a value. A new variable name is generated
-- and returned.
instr :: (L.Ident -> Instruction) -> Gen L.Ident
instr cmd = do
  llvmId <- newVarName
  emit $ cmd llvmId
  return llvmId

-- | Emit an instruction to allocate stack space for a local variable. Returns
-- the pointer to the allocated address. A new unique variable name for LLVM is
-- generated.
newVarInstr :: VarIdent -> Type -> Gen L.Ident
newVarInstr jlId typ = do
  llvmType <- toLLVMType typ
  llvmId <- newVarName
  pushVar jlId llvmId
  emit $ L.Alloca llvmId llvmType
  return llvmId

-- | Emit an instruction to load a variable by its name in the AST. A new unique
-- variable name for LLVM is generated. The variable has to be pushed to the
-- stack beforehand.
loadVarInstr :: VarIdent -> Type -> Gen L.Value
loadVarInstr jlId typ = do
  llvmType <- toLLVMType typ
  res <- lookupVar jlId
  llvmId <- case res of
    Just llvmId -> return llvmId
    Nothing -> instVarInstr jlId typ
  L.Loc <$> instr (\id -> L.Load id llvmType llvmId)

-- | Look up the address of an lvalue. If a plain variable, the variable stack
-- is searched as usual. If indexed, appropriate instructions are emitted.
lookupLVal :: LValue -> Gen L.Ident
lookupLVal (VarVal jlId typ) = do
  res <- lookupVar jlId
  case res of
    Just llvmId -> return llvmId
    Nothing -> instVarInstr jlId typ
lookupLVal (ArrVal lval expr typ) = do
  llvmType <- toLLVMType typ
  llvmArrType <- toLLVMType $ Array typ
  llvmIndVal <- compileExpr expr
  llvmVarId <- lookupLVal lval
  llvmArrId <- instr $ \id -> L.Load id llvmArrType llvmVarId
  llvmPtrId <- instr $ \id -> L.ExtractValue id llvmArrType (L.Loc llvmArrId) [1]
  instr $ \id -> L.GetElementPtr id llvmType (L.Loc llvmPtrId) [L.VarOffset (L.Int 32) llvmIndVal]
lookupLVal (DerefVal lval jlId typ) = do
  let ptrType@(Ptr (Struct strId)) = getLValType lval
  llvmStrType <- toLLVMType $ Struct strId
  llvmPtrType <- toLLVMType ptrType
  llvmFldType <- toLLVMType typ
  fldInd <- getStrFld strId jlId
  llvmPtrId <- lookupLVal lval
  llvmStrId <- instr $ \id -> L.Load id llvmPtrType llvmPtrId
  instr $ \id -> L.GetElementPtr id llvmStrType (L.Loc llvmStrId) [L.Offset (L.Int 32) 0, L.VarOffset (L.Int 32) (L.IConst fldInd)]



-- | Emit a label instruction and update the currently set label. Don't emit
-- 'L.LabelDef's manually.
labelInstr :: L.Ident -> Gen ()
labelInstr llvmLabId = do
  setCurrentLabel llvmLabId
  emit $ L.LabelDef llvmLabId

-- | Emit instructions to get the address of a class variable for a subsequent
-- 'Load'. Uses the @self@ variable. Undefined behaviuor, if @self@ does not
-- point to the class object.
instVarInstr :: VarIdent -> Type -> Gen L.Ident
instVarInstr jlId typ = do
  curClsId <- fromJust <$> getCurCls
  llvmType <- toLLVMType typ
  llvmClsType <- L.Named <$> getClsTypeName curClsId
  varInd <- getClsVar curClsId jlId
  llvmSelfId <- fromJust <$> lookupVar "self"
  llvmVarId <- instr $ \id -> L.Load id (L.Ptr llvmClsType) llvmSelfId
  instr $ \id -> L.GetElementPtr id llvmClsType (L.Loc llvmVarId) [L.Offset (L.Int 32) 0, L.VarOffset (L.Int 32) (L.IConst varInd)]

-- | If applicable and necessary, emit an instruction to cast an object to
-- another class type. Undefined behaviour if both types are class types but
-- the first type is not a subtype of the second type.
polymorphicCastInstr :: Type -> Type -> L.Value -> Gen L.Value
polymorphicCastInstr from into val =
  case (from, into) of
    (Object cls1, Object cls2) ->
      if cls1 /= cls2
        then do
          llvmFromType <- toLLVMType from
          llvmIntoType <- toLLVMType into
          llvmId <- newVarName
          L.Loc <$> instr (\id -> L.Bitcast id llvmFromType val llvmIntoType)
        else return val
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
toLLVMType (Object jlId) = L.Ptr . L.Named <$> getClsTypeName jlId
toLLVMType (Struct jlId) = L.Named <$> getStrTypeName jlId
toLLVMType (Ptr typ) = L.Ptr <$> toLLVMType typ
toLLVMType (Fn ret params) = liftM2 L.Fn (toLLVMType ret) (mapM toLLVMType params)

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
toLLVMIdent :: Ident i => i -> L.Ident
toLLVMIdent = L.Ident . ident

-- | Assemble the fully qualified name (FQN) of a method.
--
-- The FQN has the form @Class.method@.
toFQNIdent :: TypeIdent -> FnIdent -> L.Ident
toFQNIdent (TyId name) (FnId fn) = L.Ident $ T.concat [name, ".", fn]

-- * Utility functions

-- | Get the identifier used in the lvalue.
unwrapLVal :: LValue -> VarIdent
unwrapLVal (ArrVal lval _ _) = unwrapLVal lval
unwrapLVal (VarVal id typ) = id

-- | Zip a list of 'L.Value's and a list of 'Type's together to create a list of
-- LLVM function arguments.
zipArgs :: [L.Value] -> [Type] -> Gen [L.Arg]
zipArgs args types = do
  llvmTypes <- mapM toLLVMType types
  return $ zipWith L.Argument llvmTypes args

-- * Environment handling

-- | An environment to save data used during compilation.
data Env = Env
  { vars :: [Map VarIdent L.Ident],
    strFldInds :: Map (TypeIdent, VarIdent) Int,
    -- | Map a method and its class name to the corresponding index in the class
    -- descriptor.
    clsMethInds :: Map (TypeIdent, FnIdent) Int,
    -- | Map variable and its class name to the corresponding index in the class
    -- type.
    clsVarInds :: Map (TypeIdent, VarIdent) Int,
    -- | Number used in the next variable name.
    nextVar :: Int,
    -- | Number used in the next global variable name.
    nextGlobVar :: Int,
    -- | Number used in the next global string constant name.
    nextStrConst :: Int,
    -- | Number used in the next label name.
    nextLabel :: Int,
    -- | Name of the current label.
    curLabel :: L.Ident,
    -- | Name of the current class.
    curCls :: Maybe TypeIdent
  }

-- | Create an empty environment.
emptyEnv :: Env
emptyEnv =
  Env
    { vars = [Map.empty],
      strFldInds = Map.empty,
      clsMethInds = Map.empty,
      clsVarInds = Map.empty,
      nextVar = 0,
      nextGlobVar = 0,
      nextStrConst = 0,
      nextLabel = 0,
      curLabel = "",
      curCls = Nothing
    }

-- | Add an empty entry on top of the variable stack.
newVarTop :: Gen ()
newVarTop = modify (\env -> env {vars = Map.empty : vars env})

-- | Discard the top entry of the variable stack in the environment.
discardVarTop :: Gen ()
discardVarTop = modify (\env -> env {vars = tail $ vars env})

-- | Register a new local variable in the environment. The variable is pushed
-- into the top entry of the variable stack.
pushVar :: VarIdent -> L.Ident -> Gen ()
pushVar jlId llvmId = modify (\env -> env {vars = Map.insert jlId llvmId (head (vars env)) : tail (vars env)})

-- | Search for a local variable in the environment's variable stack. The
-- variable is assumed to exist, otherwise an error is thrown.
lookupVar :: VarIdent -> Gen (Maybe L.Ident)
lookupVar jlId = gets (asum . map (Map.lookup jlId) . vars)

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

-- | Get the type name of a struct.
getStrTypeName :: TypeIdent -> Gen L.Ident
getStrTypeName (TyId name) = return $ L.Ident $ T.append "_S." name

-- | Get the type name of a class.
getClsTypeName :: TypeIdent -> Gen L.Ident
getClsTypeName (TyId name) = return $ L.Ident $ T.append "_C." name

-- | Get the unique name of a class descriptor.
getDcsName :: TypeIdent -> Gen L.Ident
getDcsName (TyId name) = return $ L.Ident $ T.append "_CD." name

-- | Get type name of a class descriptor.
getDcsTypeName :: TypeIdent -> Gen L.Ident
getDcsTypeName (TyId name) = return $ L.Ident $ T.append "_DT." name

-- | Get the label under which instructions are currently emitted.
getCurrentLabel :: Gen L.Ident
getCurrentLabel = gets curLabel

-- | Set the label under which instructions are currently emitted.
setCurrentLabel :: L.Ident -> Gen ()
setCurrentLabel llvmLabId = modify (\env -> env {curLabel = llvmLabId})

-- | Add the details of a struct field to the compilation environment.
-- The index in the LLVM class struct is saved.
addStrFld :: TypeIdent -> VarIdent -> Int -> Gen ()
addStrFld strId jlId index = modify (\env -> env {strFldInds = Map.insert (strId, jlId) index (strFldInds env)})

-- | Get the details of a struct field from the compilation environment.
getStrFld :: TypeIdent -> VarIdent -> Gen Int
getStrFld strId jlId = gets $ fromJust . Map.lookup (strId, jlId) . strFldInds

-- | The name of the class that is currently compiled.
getCurCls :: Gen (Maybe TypeIdent)
getCurCls = gets curCls

-- | Set the name of the class that is curently compiled.
enterCls :: TypeIdent -> Gen ()
enterCls clsId = modify (\env -> env {curCls = Just clsId})

-- | Unset the name of the class that is currently compiled.
leaveCls :: Gen ()
leaveCls = modify (\env -> env {curCls = Nothing})

-- | Add details of a method to the compilation environment. Index in class
-- descriptor, function type and owner class name are saved.
addClsMeth :: TypeIdent -> FnIdent -> Int -> Gen ()
addClsMeth clsId jlId index = modify (\env -> env {clsMethInds = Map.insert (clsId, jlId) index (clsMethInds env)})

-- | Get the details of a method from the class environment.
getClsMeth :: TypeIdent -> FnIdent -> Gen Int
getClsMeth clsId jlId = gets $ fromJust . Map.lookup (clsId, jlId) . clsMethInds

-- | Add the details of an instance variable to the compilation environment.
-- The index in the LLVM class struct is saved.
addClsVar :: TypeIdent -> VarIdent -> Int -> Gen ()
addClsVar clsId jlId index = modify (\env -> env {clsVarInds = Map.insert (clsId, jlId) index (clsVarInds env)})

-- | Get the details of an instance variable from the compilation environment.
getClsVar :: TypeIdent -> VarIdent -> Gen Int
getClsVar clsId jlId = gets $ fromJust . Map.lookup (clsId, jlId) . clsVarInds
