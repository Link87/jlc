{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module TypeChecker
  ( TypeError (..),
    typecheck,
  )
where

import AbsJavalette
import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import PrintJavalette (printTree)

import Debug.Trace

pattern Ok a = Right a

pattern Err msg = Left msg

type AnnotatedProg = Prog

data TypeError
  = TypeMismatch Type Type
  | TypeMismatchOverloaded Type [Type]
  | ExpectedFnType Type
  | ArgumentMismatch
  | DuplicateVariable Ident
  | MainArguments
  | MainReturnType
  | MainNotFound
  | NoFunctionFound
  | DuplicateFunction
  | UndeclaredVar Ident

instance Show TypeError where
  showsPrec _ (TypeMismatch got expected) =
    showString "Type mismatch in program: Got "
      . shows got
      . showString " but expected "
      . shows expected
      . showString "."
  showsPrec _ (TypeMismatchOverloaded got [expected]) =
    showString "Type mismatch in program: Got "
      . shows got
      . showString " but expected "
      . shows expected
      . showString "."
  showsPrec _ (TypeMismatchOverloaded got expected) =
    showString "Type mismatch in program: Got "
      . shows got
      . showString " but expected one of "
      . shows expected
      . showString "."
  showsPrec _ (ExpectedFnType got) =
    showString "Type Mismatch: Expected a function but got " . shows got . showString "."
  showsPrec _ ArgumentMismatch =
    showString "Type Mismatch: Function arguments do not match function definition."
  showsPrec _ (DuplicateVariable (Ident id)) =
    showString "Variable declared multiple times: " . shows id . showString "."
  showsPrec _ MainArguments = showString "Function 'main' must not have arguments."
  showsPrec _ MainReturnType = showString "Function 'main' must return 'int'."
  showsPrec _ MainNotFound = showString "No function 'main' found."
  showsPrec _ NoFunctionFound = showString "No function definition found."
  showsPrec _ DuplicateFunction = showString "Duplicate function identifier!"
  showsPrec _ (UndeclaredVar (Ident id)) =
    showString $ "Use of undeclared variable " ++ id ++ "."

type TypeResult a = Either TypeError a

-- | The local context.
type Context = Map Ident Type

-- | The context stack.
type ContextStack = [Context]

-- | An entry in the local context.
type ContextEntry = (Ident, Type)

newtype Chk a = MkChk (StateT ContextStack (ExceptT TypeError Identity) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState ContextStack,
      MonadError TypeError
    )

-- | An empty context.
emptyContext :: Context
emptyContext = Map.empty

-- | Context that contains the prelude functions.
preludeContext :: Context
preludeContext =
  Map.insert (Ident "printInt") (Fun Void [Int]) $
    Map.insert (Ident "printDouble ") (Fun Void [Doub]) $
      Map.insert (Ident "printString") (Fun Void []) $ -- TODO
        Map.insert (Ident "readInt") (Fun Int []) $
          Map.insert (Ident "readDouble") (Fun Doub []) emptyContext

-- | Insert an element into the top of the stack
extendContext :: ContextEntry -> Chk ()
extendContext (id, typ) = do
  ctx <- get
  case ctx of
    map : ctx -> case Map.lookup id map of
      Nothing -> put $ Map.insert id typ map : ctx
      Just _ -> throwError $ DuplicateVariable id

-- | Discard the top context of the context stack.
discardTop :: Chk ()
discardTop = do
  ctx <- get
  case ctx of
    map : ctx -> put ctx
    _ -> error "Stack already empty!"

-- | Add an empty context on top of the context stack.
newTop :: Chk ()
newTop = do
  ctx <- get
  put $ emptyContext : ctx

-- Run the type checker on a program.
typecheck :: Prog -> TypeResult AnnotatedProg
typecheck prog = do
  -- First pass: save fns and check for main function
  ctx <- saveFns prog
  case Map.lookup (Ident "main") ctx of
    Just (Fun Int []) -> Ok ctx
    Just (Fun Int _) -> Err MainArguments
    Just (Fun _ _) -> Err MainReturnType
    Nothing -> Err MainNotFound
  -- Second pass: check types of fns
  runChk ctx $ checkTypes prog

-- | Run function of 'Chk' monad.
runChk :: Context -> Chk AnnotatedProg -> TypeResult AnnotatedProg
runChk ctx (MkChk rd) = runIdentity $ runExceptT $ evalStateT rd [ctx]

-- | Save the top-level functins in the context.
saveFns :: Prog -> TypeResult Context
saveFns (Program []) = Err NoFunctionFound
saveFns (Program [fn]) = do
  (id, typ) <- saveSignature fn
  return $ Map.insert id typ preludeContext
saveFns (Program (fn : fns)) = do
  (id, typ) <- saveSignature fn
  context <- saveFns $ Program fns
  if Map.notMember id context
    then Ok (Map.insert id typ context)
    else Err DuplicateFunction

-- | Get the 'ContextEntry' of the function.
saveSignature :: TopDef -> TypeResult ContextEntry
saveSignature (FnDef typ id args _blk) = Ok (id, Fun typ (argTypes args))
  where
    argTypes :: [Arg] -> [Type]
    argTypes [] = []
    argTypes (Argument typ _id : as) = typ : argTypes as

-- | Typecheck the program. Functions have to be present in context.
checkTypes :: Prog -> Chk AnnotatedProg
checkTypes (Program []) = return $ Program []
checkTypes (Program (fn : rest)) = do
  checked <- checkFnBody fn
  (Program other) <- checkTypes (Program rest)
  return $ Program $ checked : other

checkFnBody :: TopDef -> Chk TopDef
checkFnBody def@(FnDef typ id args (Block stmts)) = do
  newTop
  saveArgs args
  checkStmts stmts
  discardTop
  return def
  where
    saveArgs :: [Arg] -> Chk ()
    saveArgs (arg:args) = case arg of
      Argument typ id -> do
        extendContext (id, typ)
        saveArgs args
    saveArgs [] = return ()

lookupVar :: Ident -> Chk Type
lookupVar id = do
  stack <- get
  case stackLookup id stack of
    Just typ -> return typ
    Nothing -> throwError $ UndeclaredVar id
  where
    stackLookup :: Ident -> ContextStack -> Maybe Type
    stackLookup id (ctx : stack) = case Map.lookup id ctx of
      Just typ -> Just typ
      Nothing -> stackLookup id stack
    stackLookup _id [] = Nothing
    Ident name = id

checkStmts :: [Stmt] -> Chk Type
checkStmts stmts = case stmts of
  [stmt] -> checkStmt stmt
  stmt : rest -> do
    checkStmt stmt
    checkStmts rest

checkStmt :: Stmt -> Chk Type
checkStmt Empty = return Void
checkStmt (BStmt (Block stmts)) = do
  newTop
  typ <- checkStmts stmts
  discardTop
  return typ
checkStmt (Decl typ items) = checkItems items typ
checkStmt (Ass id expr) = do
  typ <- lookupVar id
  checkExpr expr typ
checkStmt (Incr id) = checkVar id Int
checkStmt (Decr id) = checkVar id Int
checkStmt (Ret expr) = undefined -- TODO return??
checkStmt VRet = return Void -- TODO is that valid?
checkStmt (Cond expr stmt) = do
  checkExpr expr Bool
  checkStmt stmt
checkStmt (CondElse expr stmt1 stmt2) = do
  checkExpr expr Bool
  checkStmt stmt1
  checkStmt stmt2
checkStmt (While expr stmt) = do
  checkExpr expr Bool
  checkStmt stmt
checkStmt (SExp expr) = checkExpr expr Void

checkExpr :: Expr -> Type -> Chk Type
checkExpr expr typ = do
  inferred <- inferExpr expr
  if inferred == typ
    then return typ
    else throwError $ TypeMismatch inferred typ

checkVar :: Ident -> Type -> Chk Type
checkVar id typ = do
  saved <- lookupVar id
  if saved == typ
    then return typ
    else throwError $ TypeMismatch typ saved

checkItems :: [Item] -> Type -> Chk Type
checkItems (item : items) typ = case item of
  NoInit id -> do
    extendContext (id, typ)
    checkItems items typ
    return typ
  Init id expr -> do
    inferred <- inferExpr expr
    if inferred == typ
      then do
        extendContext (id, typ)
        checkItems items typ
        return typ
      else throwError $ TypeMismatch inferred typ

inferExpr :: Expr -> Chk Type
inferExpr (EVar id) = lookupVar id
inferExpr (ELitInt int) = return Int
inferExpr (ELitDoub doub) = return Doub
inferExpr ELitTrue = return Bool
inferExpr ELitFalse = return Bool
inferExpr (EApp id exprs) = do
  typ <- lookupVar id
  result <- checkFn exprs typ
  case result of
    (Fun ret _) -> return ret
    _ -> error "Assertion failed: not a function."
inferExpr (EString str) = undefined -- TODO what here? maybe string type?
inferExpr (Neg expr) = inferUn expr [Int, Doub]
inferExpr (Not expr) = inferUn expr [Bool]
inferExpr (EMul expr1 Mod expr2) = inferBin expr1 expr2 [Int]
inferExpr (EMul expr1 _ expr2) = inferBin expr1 expr2 [Int, Doub]
inferExpr (EAdd expr1 _op expr2) = inferBin expr1 expr2 [Int, Doub]
inferExpr (ERel expr1 op expr2) = inferBin expr1 expr2 [Int, Doub]
inferExpr (EAnd expr1 expr2) = inferBin expr1 expr2 [Bool]
inferExpr (EOr expr1 expr2) = inferBin expr1 expr2 [Bool]

-- | Infer the type for an unary expression. The inferred type has to be
-- one of the given types. Otherwise, a 'TypeError' is emitted.
inferUn :: Expr -> [Type] -> Chk Type
inferUn expr types = do
  typ <- inferExpr expr
  if typ `elem` types
    then return typ
    else throwError $ TypeMismatchOverloaded typ types

-- | Infer the type for a binary expression. Both subexpressions have to match
-- in their types and the inferred type has to be one of the given types.
-- Otherwise, a 'TypeError' is emitted.
inferBin :: Expr -> Expr -> [Type] -> Chk Type
inferBin expr1 expr2 types = do
  typ <- inferExpr expr1
  if typ `elem` types
    then checkExpr expr2 typ
    else throwError $ TypeMismatchOverloaded typ types

-- | Checks if the function argument expressions match the function type.
checkFn :: [Expr] -> Type -> Chk Type
checkFn exprs fntype@(Fun ret types) = case (exprs, types) of
  ([], []) -> return fntype
  (expr : exprs, typ : types) -> do
    inferred <- inferExpr expr
    if inferred == typ
      then checkFn exprs (Fun ret types) >> return fntype
      else throwError $ TypeMismatch typ inferred
  (_, _) -> throwError ArgumentMismatch
checkFn _exprs typ = throwError $ ExpectedFnType typ
