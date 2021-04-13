{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Javalette.Gen.LLVM
  ( generateIR,
  )
where

import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State
  ( MonadState (get, put),
    StateT (StateT),
    evalStateT,
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

type Code = Endo [Instruction]

generateIR :: AnnotatedProg -> Text
generateIR prog = generateCode $ appEndo (runGen $ compile prog) []

newtype Gen a = MkGen (WriterT Code (StateT Env Identity) a)
  deriving (Functor, Applicative, Monad, MonadState Env, MonadWriter Code)

runGen :: Gen () -> Code
runGen (MkGen gen) = runIdentity (evalStateT (execWriterT gen) emptyEnv)

emit :: Instruction -> Gen ()
emit instr = tell $ Endo ([instr] <>)

compile :: AnnotatedProg -> Gen ()
compile prog = do
  emit $ L.FnDecl L.Void "printInt" [L.Int 32]
  emit $ L.FnDecl L.Void "printDouble" [L.Doub]
  emit $ L.FnDecl L.Void "printString" [L.Ptr $ L.Int 8]
  emit $ L.FnDecl (L.Int 32) "readInt" []
  emit $ L.FnDecl L.Doub "readInt" []
  compileProg prog

compileProg :: AnnotatedProg -> Gen ()
compileProg (Program []) = return ()
compileProg (Program (fn : rest)) = do
  compileFn fn
  compileProg $ Program rest

compileFn :: TopDef -> Gen ()
compileFn (FnDef typ id args (Block blk)) = do
  llvmArgs <- compileFnArgs args
  emit $ L.FnDef (toLLVMType typ) id llvmArgs
  emit $ L.LabelDef $ Ident "entry"
  compileStmts blk
  emit L.EndFnDef
  where
    compileFnArgs :: [Arg] -> Gen [L.Arg]
    compileFnArgs [] = return []
    compileFnArgs (Argument typ jlId : rest) = do
      llvmId <- newVarName
      pushVar jlId llvmId
      args <- compileFnArgs rest
      return $ L.Argument (toLLVMType typ) llvmId : args

compileStmts :: [Stmt] -> Gen ()
compileStmts [] = return ()
compileStmts (stmt : stmts) = do
  compileStmt stmt
  compileStmts stmts

compileStmt :: Stmt -> Gen ()
compileStmt Empty = return ()
compileStmt (BStmt (Block _)) = undefined
compileStmt (Decl typ items) = undefined
compileStmt (Ass id expr) = undefined
compileStmt (Incr id) = undefined
compileStmt (Decr id) = undefined
compileStmt (Ret (ETyped expr typ)) = do
  id <- compileExpr expr
  emit $ L.Return (toLLVMType typ) id
compileStmt VRet = emit L.VReturn
compileStmt (Cond expr stmt) = undefined
compileStmt (CondElse expr stmt smt) = undefined
compileStmt (While expr stmt) = undefined
compileStmt (SExp expr) = undefined

compileExpr :: Expr -> Gen Ident
compileExpr = undefined

toLLVMType :: Type -> L.Type
toLLVMType Int = L.Int 32
toLLVMType Doub = L.Doub
toLLVMType Bool = L.Int 1
toLLVMType Void = L.Void

data Env = Env
  { vars :: [Map Ident Ident],
    nextVar :: Int,
    nextLabel :: Int
  }

emptyEnv :: Env
emptyEnv = Env [] 0 0

newVarName :: Gen Ident
newVarName = do
  env <- get
  let num = nextVar env
  put env {nextVar = num + 1}
  let id = Ident $ "t" <> T.pack (show num)
  return id

pushVar :: Ident -> Ident -> Gen ()
pushVar jlId llvmId = do
  env <- get
  put env {vars = Map.insert jlId llvmId (head (vars env)) : tail (vars env)}
  return ()
