{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_HADDOCK prune, ignore-exports, show-extensions #-}

module Javalette.Check.TypeCheck
  ( TypeError (..),
    AnnotatedProg,
    check,
  )
where

import Control.Monad (when)
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    runExceptT,
  )
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State
  ( MonadState (get, put),
    StateT (StateT),
    evalStateT,
  )
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Javalette.Check.Return (ReturnState (..), both)
import Javalette.Lang.Abs
import Javalette.Lang.Print (printTree)

-- * Type Check

-- | An annotated program. Nothing special type-wise.
type AnnotatedProg = Prog

-- | A result of the type checker. Is either a computed value or a `TypeError`.
type TypeResult a = Either TypeError a

pattern Ok :: b -> Either a b
pattern Ok a = Right a

pattern Err :: a -> Either a b
pattern Err msg = Left msg

-- | Run the type checker on a program.
check :: Prog -> TypeResult AnnotatedProg
check prog = do
  -- First pass: save fns and check for main function
  ctx <- saveFns prog
  case lookupContextEntry (Ident "main") ctx of
    Just (Fn Int []) -> Ok ctx
    Just (Fn Int _) -> Err MainArguments
    Just (Fn _ _) -> Err MainReturnType
    Nothing -> Err MainNotFound
  -- Second pass: check types of fns
  runChk ctx $ checkProgram prog

-- * Type checking functions (first pass)

-- | Save the top-level functins in the context. Main function for the first
-- pass.
saveFns :: Prog -> TypeResult Context
saveFns (Program []) = Err NoFunctionFound
saveFns (Program [fn]) = do
  (id, typ) <- saveSignature fn
  return $ insertContextEntry (id, typ) preludeContext
saveFns (Program (fn : fns)) = do
  (id, typ) <- saveSignature fn
  context <- saveFns $ Program fns
  if not $ memberContextEntry id context
    then Ok (insertContextEntry (id, typ) context)
    else Err DuplicateFunction

-- | Get the 'ContextEntry' of the function.
saveSignature :: TopDef -> TypeResult ContextEntry
saveSignature (FnDef typ id args _blk) = Ok (id, Fn typ (argTypes args))
  where
    argTypes :: [Arg] -> [Type]
    argTypes [] = []
    argTypes (Argument typ _id : as) = typ : argTypes as

-- * Type checking functions (second pass)

-- | The monad used by the type checker. Keeps track of the context using the
-- @State@ monad and any errors using the @Except@ monad.
newtype Chk a = MkChk (StateT ContextStack (ExceptT TypeError Identity) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState ContextStack,
      MonadError TypeError
    )

-- | Run function of 'Chk' monad.
runChk :: Context -> Chk AnnotatedProg -> TypeResult AnnotatedProg
runChk ctx (MkChk rd) = runIdentity $ runExceptT $ evalStateT rd [ctx]

-- | Typecheck the program. Main function for the second pass.
-- Functions have to be present in context.
checkProgram :: Prog -> Chk AnnotatedProg
checkProgram (Program []) = return $ Program []
checkProgram (Program (fn : rest)) = do
  checked <- checkFnBody fn
  (Program other) <- checkProgram (Program rest)
  return $ Program $ checked : other

-- | Typecheck a function's body. Returns the input augmented with type
-- annotations if succesful or throws a `TypeError` otherwise.
checkFnBody :: TopDef -> Chk TopDef
checkFnBody (FnDef typ id args (Block stmts)) = do
  newTop
  saveArgs args
  when (typ == Void) $ setReturnState Return
  annotated <- checkStmts stmts typ
  checkReturnState
  discardTop
  return $ FnDef typ id args (Block annotated)
  where
    saveArgs :: [Arg] -> Chk ()
    saveArgs (arg : args) = case arg of
      Argument typ id -> do
        extendContext (id, typ)
        saveArgs args
    saveArgs [] = return ()

-- | Typecheck a list of statements. Returns the input augmented with type
-- annotations if succesful or throws a `TypeError` otherwise.
checkStmts :: [Stmt] -> Type -> Chk [Stmt]
checkStmts stmts ret = case stmts of
  [] -> return []
  stmt : rest -> do
    -- traceM $ show stmt
    annotated <- checkStmt stmt ret
    next <- checkStmts rest ret
    return $ annotated : next

-- | Typecheck a single statement. Returns the input augmented with type
-- annotations if succesful or throws a `TypeError` otherwise.
checkStmt :: Stmt -> Type -> Chk Stmt
checkStmt Empty _ = return Empty
checkStmt (BStmt (Block stmts)) ret = do
  newTop
  annotated <- checkStmts stmts ret
  state <- getReturnState
  discardTop
  setReturnState state
  return (BStmt (Block annotated))
checkStmt (Decl typ items) _ = do
  annotated <- checkItems items typ
  return $ Decl typ annotated
checkStmt (Ass expr1 expr2) _ = do
  lval <- coerceLVal expr1
  typ <- lookupVar lval
  annotated <- checkExpr expr2 typ
  return $ Ass (ETyped (ELValue lval) typ) annotated
checkStmt (Incr expr) _ = do
  lval <- coerceLVal expr
  checkVar lval Int
  return $ Incr (ELValue lval)
checkStmt (Decr expr) _ = do
  lval <- coerceLVal expr
  checkVar lval Int
  return $ Decr (ELValue lval)
checkStmt (Ret expr) ret = do
  annotated <- checkExpr expr ret
  setReturnState Return
  return $ Ret annotated
checkStmt VRet ret =
  if ret == Void
    then setReturnState Return >> return VRet
    else throwError $ TypeMismatch ret Void
checkStmt (Cond expr stmt) ret = do
  annotatedExpr <- checkExpr expr Boolean
  state <- getReturnState
  annnotatedStmt <- checkStmt stmt ret
  case expr of
    ELitTrue -> return $ Cond annotatedExpr annnotatedStmt
    _ -> do
      overrideReturnState state
      return $ Cond annotatedExpr annnotatedStmt
checkStmt (CondElse expr stmt1 stmt2) ret = do
  annotatedExpr <- checkExpr expr Boolean
  state <- getReturnState
  annotatedStmt1 <- checkStmt stmt1 ret
  branch1 <- getReturnState
  overrideReturnState state
  annotatedStmt2 <- checkStmt stmt2 ret
  branch2 <- getReturnState
  case expr of
    ELitTrue -> do
      overrideReturnState (state <> branch1)
      return $ CondElse annotatedExpr annotatedStmt1 annotatedStmt2
    ELitFalse -> do
      overrideReturnState (state <> branch2)
      return $ CondElse annotatedExpr annotatedStmt1 annotatedStmt2
    _ -> do
      overrideReturnState (state <> both branch1 branch2)
      return $ CondElse annotatedExpr annotatedStmt1 annotatedStmt2
checkStmt (While expr stmt) ret = do
  annotatedExpr <- checkExpr expr Boolean
  state <- getReturnState
  annotatedStmt <- checkStmt stmt ret
  case expr of
    ELitTrue -> return $ While annotatedExpr annotatedStmt
    _ -> do
      overrideReturnState state
      return $ While annotatedExpr annotatedStmt
checkStmt (ForEach typ id expr stmt) ret = do
  annotatedExpr <- checkExpr expr (Array typ)
  newTop
  extendContext (id, typ)
  state <- getReturnState
  annotatedStmt <- checkStmt stmt ret
  discardTop
  overrideReturnState state
  return $ ForEach typ id annotatedExpr annotatedStmt
checkStmt (SExpr expr) _ = do
  annotated <- checkExpr expr Void
  return $ SExpr annotated

-- | Typecheck a single expression. Returns the input augmented with type
-- annotations if succesful or throws a `TypeError` otherwise.
checkExpr :: Expr -> Type -> Chk Expr
checkExpr expr typ = do
  annotated <- inferExpr expr
  let inferred = getType annotated
  if inferred == typ
    then return annotated
    else throwError $ TypeMismatch inferred typ

-- | Check a variable's type. Throws a `TypeError` on type mismatch.
checkVar :: LValue -> Type -> Chk ()
checkVar lval typ = do
  saved <- lookupVar lval
  if saved == typ
    then return ()
    else throwError $ TypeMismatch saved typ

-- | Typecheck a list of `Item`s. Returns the input augmented with type
-- annotations if succesful or throws a `TypeError` otherwise.
checkItems :: [Item] -> Type -> Chk [Item]
checkItems [] typ = return []
checkItems (item : items) typ = case item of
  NoInit id -> do
    extendContext (id, typ)
    next <- checkItems items typ
    return $ item : next
  Init id expr -> do
    annotated <- inferExpr expr
    let inferred = getType annotated
    if inferred == typ
      then do
        extendContext (id, typ)
        next <- checkItems items typ
        return $ Init id annotated : next
      else throwError $ TypeMismatch inferred typ

-- | Typecheck the arguments of a function call. Returns the input augmented
-- with type annotations if succesful or throws a `TypeError` otherwise.
checkFnArgs :: [Expr] -> Type -> Chk [Expr]
checkFnArgs exprs fntype@(Fn ret types) = case (exprs, types) of
  ([], []) -> return []
  (expr : exprs, typ : types) -> do
    annotated <- inferExpr expr
    let inferred = getType annotated
    if inferred == typ
      then do
        next <- checkFnArgs exprs (Fn ret types)
        return $ annotated : next
      else throwError $ TypeMismatch inferred typ
  (_, _) -> throwError ArgumentMismatch
checkFnArgs _exprs typ = throwError $ ExpectedFnType typ

-- | Infer the type of an expression. Augments the expression and (if applicable)
-- any subexpressions with type annotations. Typechecks subexpressions, which
-- can lead to a `TypeError` being throwin.
inferExpr :: Expr -> Chk Expr
inferExpr expr@(EVar id) = ETyped expr <$> lookupVar (Id id)
inferExpr expr@(ELitInt _) = return $ ETyped expr Int
inferExpr expr@(ELitDoub _) = return $ ETyped expr Double
inferExpr ELitTrue = return $ ETyped ELitTrue Boolean
inferExpr ELitFalse = return $ ETyped ELitFalse Boolean
inferExpr (EApp id exprs) = do
  typ <- lookupVar (Id id)
  annotated <- checkFnArgs exprs typ
  -- only type with return type of function, not with function type itself
  case typ of
    (Fn ret _) -> return $ ETyped (EApp id annotated) ret
    _ -> error "Not a function but expected a function."
inferExpr expr@(EString _) = return $ ETyped expr String
inferExpr (ENeg expr) = do
  annotated <- inferUn expr [Int, Double]
  return $ ETyped (ENeg annotated) (getType annotated)
inferExpr (ENot expr) = do
  annotated <- checkExpr expr Boolean
  return $ ETyped (ENot annotated) Boolean
inferExpr (EMul expr1 op expr2) = do
  (annotated1, annotated2) <- inferBin expr1 expr2 exprType
  return $ ETyped (EMul annotated1 op annotated2) (getType annotated1)
  where
    exprType = if op == Mod then [Int] else [Int, Double]
inferExpr (EAdd expr1 op expr2) = do
  (annotated1, annotated2) <- inferBin expr1 expr2 [Int, Double]
  return $ ETyped (EAdd annotated1 op annotated2) (getType annotated1)
inferExpr (ERel expr1 op expr2) = do
  (annotated1, annotated2) <- inferBin expr1 expr2 exprType
  return $ ETyped (ERel annotated1 op annotated2) Boolean
  where
    exprType = if op == EQU || op == NE then [Int, Double, Boolean] else [Int, Double]
inferExpr (EAnd expr1 expr2) = do
  (annotated1, annotated2) <- inferBin expr1 expr2 [Boolean]
  return $ ETyped (EAnd annotated1 annotated2) Boolean
inferExpr (EOr expr1 expr2) = do
  (annotated1, annotated2) <- inferBin expr1 expr2 [Boolean]
  return $ ETyped (EOr annotated1 annotated2) Boolean
inferExpr (EArrAlloc typ expr) = do
  annotated <- checkExpr expr Int
  return $ ETyped (EArrAlloc typ annotated) (Array typ)
inferExpr (EArrIndex expr1 expr2) = do
  annotatedArr <- inferExpr expr1
  annotatedIndex <- checkExpr expr2 Int
  case annotatedArr of
    (ETyped _ (Array inner)) -> return $ ETyped (EArrIndex annotatedArr annotatedIndex) inner
    (ETyped _ typ) -> throwError $ TypeMismatch typ (Array Void)
inferExpr (EArrLen expr) = do
  annotated <- inferExpr expr
  case annotated of
    (ETyped inner (Array typ)) -> return $ ETyped (EArrLen annotated) Int
    (ETyped inner typ) -> throwError $ TypeMismatch typ (Array Void)

-- | Infer the type for an unary expression and return the expression augmented
-- with type information. The inferred type has to be one of the given types.
-- Otherwise, a 'TypeError' is thrown.
inferUn :: Expr -> [Type] -> Chk Expr
inferUn expr types = do
  annotated <- inferExpr expr
  let inferred = getType annotated
  if inferred `elem` types
    then return annotated
    else throwError $ TypeMismatchOverloaded inferred types

-- | Infer the type for a binary expression and return both augmented with type
-- information each. Both subexpressions have to match in their types and the
-- inferred type has to be one of the given types. Otherwise, a 'TypeError' is
-- thrown.
inferBin :: Expr -> Expr -> [Type] -> Chk (Expr, Expr)
inferBin expr1 expr2 types = do
  annotated1 <- inferExpr expr1
  let inferred = getType annotated1
  if inferred `elem` types
    then do
      annotated2 <- checkExpr expr2 inferred
      return (annotated1, annotated2)
    else throwError $ TypeMismatchOverloaded inferred types

-- * Type checking helper functions

-- | Transmute an expression into an lvalue. Performs type checks on nested
-- expressions. Fails with a 'TypeError', if an expression cannot occur as an
-- lvalue.
coerceLVal :: Expr -> Chk LValue
coerceLVal (EVar id) = return $ Id id
coerceLVal (EArrIndex expr1 expr2) = do
  lval <- coerceLVal expr1
  annotated <- checkExpr expr2 Int
  return $ ArrId lval annotated
coerceLVal expr = throwError $ ExpectedLValue expr

-- | Get the type of a typed expression.
getType :: Expr -> Type
getType expr = case expr of
  ETyped _ typ -> typ
  _ -> error "Not a typed expression!"

-- * Context

-- | The local context.
type Context = (Map Ident Type, ReturnState)

-- | The context stack. Consists of individual `Context`s.
type ContextStack = [Context]

-- | A variable entry in the local context.
type ContextEntry = (Ident, Type)

-- | An empty context.
emptyContext :: Context
emptyContext = (Map.empty, NoReturn)

-- | Context that contains the prelude functions of Javalette.
preludeContext :: Context
preludeContext =
  insertContextEntry (Ident "printInt", Fn Void [Int]) $
    insertContextEntry (Ident "printDouble", Fn Void [Double]) $
      insertContextEntry (Ident "printString", Fn Void [String]) $
        insertContextEntry (Ident "readInt", Fn Int []) $
          insertContextEntry (Ident "readDouble", Fn Double []) emptyContext

-- | Insert an entry into a `Context`.
insertContextEntry :: ContextEntry -> Context -> Context
insertContextEntry (id, typ) (map, ret) = (Map.insert id typ map, ret)

-- | Look up whether an entry is present in a context. Returns `Just` if found,
-- otherwise `Nothing`.
lookupContextEntry :: Ident -> Context -> Maybe Type
lookupContextEntry id (map, _) = Map.lookup id map

-- | Look up whether an entry is present in a context.
memberContextEntry :: Ident -> Context -> Bool
memberContextEntry id (map, _) = Map.member id map

-- | Insert an entry into the top of the context stack
extendContext :: ContextEntry -> Chk ()
extendContext (id, typ) = do
  ctx <- get
  case ctx of
    map : ctx -> case lookupContextEntry id map of
      Nothing -> put $ insertContextEntry (id, typ) map : ctx
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

-- | Search the context stack for a variable or function and return its type.
-- Takes the first element that is discovered. Indexed lvalues are properly
-- handled.
lookupVar :: LValue -> Chk Type
lookupVar lval = do
  let id = unwrapLVal lval
  stack <- get
  case stackLookup id stack of
    Just typ -> return $ applyIndexing lval typ
    Nothing -> throwError $ UndeclaredVar id
  where
    -- Get the identifier burried in an lvalue
    unwrapLVal :: LValue -> Ident
    unwrapLVal (ArrId lval _) = unwrapLVal lval
    unwrapLVal (Id id) = id
    -- Adapt the type of the identifier to the lvalue expressions applied to
    -- it.
    applyIndexing :: LValue -> Type -> Type
    applyIndexing (ArrId lval _) (Array typ) = applyIndexing lval typ
    applyIndexing (Id _) typ = typ
    stackLookup :: Ident -> ContextStack -> Maybe Type
    stackLookup id (ctx : stack) = case lookupContextEntry id ctx of
      Just typ -> Just typ
      Nothing -> stackLookup id stack
    stackLookup _id [] = Nothing

-- * Return State

-- | Examine the current return state. Throws an error if `NoReturn` is found.
checkReturnState :: Chk ()
checkReturnState = do
  state <- getReturnState
  -- traceM $ show state
  case state of
    Return -> return ()
    NoReturn -> throwError NonReturningPath

-- | Extract the `ReturnState` from the top context stack item in the `Chk`
-- monad.
getReturnState :: Chk ReturnState
getReturnState = do
  stack <- get
  case stack of
    (_, ret) : _ -> return ret
    [] -> error "Empty context stack!"

-- | Set the `ReturnState` in the top context stack item in the `Chk` monad.
-- Uses `(<>)` to merge the present return state with the new one.
setReturnState :: ReturnState -> Chk ()
setReturnState state = do
  stack <- get
  case stack of
    (map, ret) : rest -> put $ (map, ret <> state) : rest
    [] -> error "Empty context stack!"

-- | Set the `ReturnState` in the top context stack item in the `Chk` monad.
-- Overrides the present return state.
overrideReturnState :: ReturnState -> Chk ()
overrideReturnState state = do
  stack <- get
  case stack of
    (map, _) : rest -> put $ (map, state) : rest
    [] -> error "Empty context stack!"

-- * Error handling

-- | A kind of typing error that can occur in a program.
data TypeError
  = TypeMismatch Type Type
  | TypeMismatchOverloaded Type [Type]
  | ExpectedFnType Type
  | ArgumentMismatch
  | DuplicateVariable Ident
  | NonReturningPath
  | MainArguments
  | MainReturnType
  | MainNotFound
  | NoFunctionFound
  | DuplicateFunction
  | UndeclaredVar Ident
  | ExpectedLValue Expr

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
  showsPrec _ NonReturningPath =
    showString "Missing return statement: execution path without return found."
  showsPrec _ MainArguments = showString "Function 'main' must not have arguments."
  showsPrec _ MainReturnType = showString "Function 'main' must return 'int'."
  showsPrec _ MainNotFound = showString "No function 'main' found."
  showsPrec _ NoFunctionFound = showString "No function definition found."
  showsPrec _ DuplicateFunction = showString "Duplicate function identifier!"
  showsPrec _ (UndeclaredVar (Ident id)) =
    shows $ "Use of undeclared variable " <> id <> "."
  showsPrec _ (ExpectedLValue expr) =
    shows $ "Expected an lvalue but got expression " <> show expr <> "."
