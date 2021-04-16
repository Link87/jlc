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
  llvmArgs <- convertFnArgs args
  emit L.Blank
  emit $ L.FnDef (toLLVMType typ) jlId llvmArgs
  emit $ L.LabelDef $ Ident "entry"
  argsToStack args llvmArgs
  newTop
  compileStmts blk
  if typ == Void
    then emit L.VReturn
    else emit L.Unreachable 
  discardTop
  emit L.EndFnDef
  where
    convertFnArgs :: [Arg] -> Gen [L.Arg]
    convertFnArgs [] = return []
    convertFnArgs (Argument typ jlId : rest) = do
      llvmId <- newVarName
      args <- convertFnArgs rest
      return $ L.Argument (toLLVMType typ) (L.Loc llvmId) : args
    argsToStack :: [Arg] -> [L.Arg] -> Gen ()
    argsToStack [] [] = return ()
    argsToStack (Argument _ jlId : rest1) (L.Argument typ llvmArgId : rest2) = do
      llvmStackId <- newVarName
      pushVar jlId llvmStackId
      emit $ L.Alloca llvmStackId typ
      emit $ L.Store typ llvmArgId llvmStackId
      argsToStack rest1 rest2

compileStmts :: [Stmt] -> Gen ()
compileStmts [] = return ()
compileStmts (stmt : stmts) = do
  compileStmt stmt
  compileStmts stmts

compileStmt :: Stmt -> Gen ()
compileStmt Empty = return ()
compileStmt (BStmt (Block stmts)) = do
  newTop
  compileStmts stmts
  discardTop
compileStmt (Decl typ items) = do
  compileItems items typ
compileStmt (Ass jlId expr@(ETyped _ typ)) = do
  val <- compileExpr expr
  llvmId <- lookupVar jlId
  emit $ L.Store (toLLVMType typ) val llvmId
compileStmt (Incr jlId) = do
  val <- loadVarInstr jlId Int
  llvmId <- newVarName
  emit $ L.Add llvmId (toLLVMType Int) val (L.IConst 1)
compileStmt (Decr jlId) = do
  val <- loadVarInstr jlId Int
  llvmId <- newVarName
  emit $ L.Sub llvmId (toLLVMType Int) val (L.IConst 1)
compileStmt (Ret expr@(ETyped _ typ)) = do
  val <- compileExpr expr
  emit $ L.Return (toLLVMType typ) val
compileStmt VRet = do
  emit L.VReturn
compileStmt (Cond expr stmt) = do
  ifLabId <- newLabName
  endLabId <- newLabName
  val <- compileExpr expr
  emit $ L.Branch val ifLabId endLabId
  emit $ L.LabelDef ifLabId
  compileStmt stmt
  emit $ L.UncondBranch endLabId
  emit $ L.LabelDef endLabId
compileStmt (CondElse expr stmt1 stmt2) = do
  ifLabId <- newLabName
  elseLabId <- newLabName
  endLabId <- newLabName
  val <- compileExpr expr
  emit $ L.Branch val ifLabId elseLabId
  emit $ L.LabelDef ifLabId
  compileStmt stmt1
  emit $ L.UncondBranch endLabId
  emit $ L.LabelDef elseLabId
  compileStmt stmt2
  emit $ L.UncondBranch endLabId
  emit $ L.LabelDef endLabId
compileStmt (While expr stmt) = do
  topLabId <- newLabName
  loopLabId <- newLabName
  endLabId <- newLabName
  emit $ L.UncondBranch topLabId
  emit $ L.LabelDef topLabId
  val <- compileExpr expr
  emit $ L.Branch val loopLabId endLabId
  emit $ L.LabelDef loopLabId
  compileStmt stmt
  emit $ L.UncondBranch topLabId
  emit $ L.LabelDef endLabId
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
  where
    newVarInstr :: Ident -> Type -> Gen Ident
    newVarInstr jlId typ = do
      llvmId <- newVarName
      pushVar jlId llvmId
      emit $ L.Alloca llvmId (toLLVMType typ)
      return llvmId

loadVarInstr :: Ident -> Type -> Gen L.Value
loadVarInstr jlId typ = do
  llvmId <- lookupVar jlId
  tempId <- newVarName
  emit $ L.Load tempId (toLLVMType typ) llvmId
  return $ L.Loc tempId

compileExprs :: [Expr] -> Gen [L.Value]
compileExprs [] = return []
compileExprs (expr : exprs) = do
  val <- compileExpr expr
  vals <- compileExprs exprs
  return $ val : vals

compileExpr :: Expr -> Gen L.Value
compileExpr (ETyped (EVar jlId) typ) = loadVarInstr jlId typ
compileExpr (ETyped (ELitInt ival) Int) = return $ L.IConst (fromInteger ival)
compileExpr (ETyped (ELitDoub dval) Doub) = return $ L.DConst dval
compileExpr (ETyped ELitTrue Bool) = return $ L.BConst True
compileExpr (ETyped ELitFalse Bool) = return $ L.BConst False
compileExpr (ETyped (EApp jlId exprs) Void) = do
  vals <- compileExprs exprs
  emit $ L.VCall jlId (zipArgs vals (getTypes exprs))
  return L.VConst
compileExpr (ETyped (EApp jlId exprs) typ) = do
  llvmId <- newVarName
  vals <- compileExprs exprs
  emit $ L.Call llvmId (toLLVMType typ) jlId (zipArgs vals (getTypes exprs))
  return $ L.Loc llvmId
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
  val1 <- compileExpr expr1
  val2 <- compileExpr expr2
  llvmId <- newVarName
  emit $ L.And llvmId (toLLVMType Bool) val1 val2
  return $ L.Loc llvmId
compileExpr (ETyped (EOr expr1 expr2) Bool) = do
  val1 <- compileExpr expr1
  val2 <- compileExpr expr2
  llvmId <- newVarName
  emit $ L.Or llvmId (toLLVMType Bool) val1 val2
  return $ L.Loc llvmId
compileExpr _ = error "No (matching) type annotation found!"

toLLVMType :: Type -> L.Type
toLLVMType Int = L.Int 32
toLLVMType Doub = L.Doub
toLLVMType Bool = L.Int 1
toLLVMType Void = L.Void
toLLVMType String = L.Ptr $ L.Int 8

toLLVMRelOp :: RelOp -> L.RelOp
toLLVMRelOp LTH = L.Slt
toLLVMRelOp LE = L.Sle
toLLVMRelOp GTH = L.Sgt
toLLVMRelOp GE = L.Sge
toLLVMRelOp EQU = L.Eq
toLLVMRelOp NE = L.Ne

toLLVMFRelOp :: RelOp -> L.FRelOp
toLLVMFRelOp LTH = L.Olt
toLLVMFRelOp LE = L.Ole
toLLVMFRelOp GTH = L.Ogt
toLLVMFRelOp GE = L.Oge
toLLVMFRelOp EQU = L.Oeq
toLLVMFRelOp NE = L.One

getTypes :: [Expr] -> [Type]
getTypes [] = []
getTypes (expr : exprs) =
  case expr of
    ETyped _ typ -> typ : getTypes exprs
    _ -> error "Not a typed expression!"
  
zipArgs :: [L.Value] -> [Type] -> [L.Arg]
zipArgs [] [] = []
zipArgs (val : vals) (typ : types) = L.Argument (toLLVMType typ) val : zipArgs vals types

-- * Environment

data Env = Env
  { vars :: [Map Ident Ident],
    -- returns :: [ReturnState],
    nextVar :: Int,
    nextGlobVar :: Int,
    nextLabel :: Int
  }

emptyEnv :: Env
emptyEnv =
  Env
    { vars = [Map.empty],
      -- returns = [],
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
    _ -> error "Variable stack already empty!"
  -- case returns env of
  --   top : rest -> put env {returns = rest}
  --   _ -> error "Return state stack already empty!"

-- | Add an empty context on top of the context stack.
newTop :: Gen ()
newTop = do
  env <- get
  put env {vars = Map.empty : vars env}
  --put env {vars = Map.empty : vars env, returns = NoReturn : returns env}

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

newLabName :: Gen Ident
newLabName = do
  env <- get
  let num = nextLabel env
  put env {nextLabel = num + 1}
  let llvmLabelId = Ident $ "lab" <> T.pack (show num)
  return llvmLabelId

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

-- getReturnState :: Gen ReturnState
-- getReturnState = gets (head . returns)

-- setReturnState :: ReturnState -> Gen ()
-- setReturnState state = do
--   env <- get
--   case returns env of
--     ret : rest -> put $ env {returns = ret <> state : rest}
--     [] -> error "Empty return state list!"

-- overrideReturnState :: ReturnState -> Gen ()
-- overrideReturnState state = do
--   env <- get
--   case returns env of
--     ret : rest -> put $ env {returns = state : rest}
--     [] -> error "Empty return state list!"
