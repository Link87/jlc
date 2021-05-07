{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Trustworthy #-}
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
    MonadError (catchError, throwError),
    runExceptT,
  )
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State
  ( MonadState (get, put),
    StateT (StateT),
    evalStateT,
    gets,
  )
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Debug.Trace
import Javalette.Check.Return (ReturnState (..), both)
import Javalette.Check.TypeError (TypeError (..), TypeResult (..), pattern Err, pattern Ok)
import Javalette.Lang.Abs
import Javalette.Lang.Print (printTree)

-- * Type Check

-- | An annotated program. Nothing special type-wise.
type AnnotatedProg = Prog

-- | Run the type checker on a program.
check :: Prog -> TypeResult AnnotatedProg
check prog = do
  -- First pass: save fns and classes. Also check for main function.
  ctx <- saveGlobDefs prog
  case lookupVarEntry (Ident "main") ctx of
    Just (Fn Int []) -> Ok ctx
    Just (Fn Int _) -> Err MainArguments
    Just (Fn _ _) -> Err MainReturnType
    Nothing -> Err MainNotFound
  -- Second pass: check types in fns and methods
  runChk ctx $ checkProgram prog

-- * Type checking functions (first pass)

-- | Save the top-level definitions (classes and functions) in the context.
-- Main function for the first pass.
saveGlobDefs :: Prog -> TypeResult Context
saveGlobDefs (Program []) = Err EmptyProgram
saveGlobDefs (Program [td]) = do
  res <- createTopDefEntry td
  case res of
    Left (id, typ) -> return $ insertVarEntry (id, typ) preludeContext
    Right cls -> return $ insertClassEntry cls preludeContext
saveGlobDefs (Program (td : tds)) = do
  res <- createTopDefEntry td
  case res of
    Left (id, typ) -> do
      context <- saveGlobDefs $ Program tds
      if not (memberVarEntry id context) && not (memberClassEntry id context)
        then Ok (insertVarEntry (id, typ) context)
        else Err $ DuplicateFunction id
    Right cls@(id, _, _) -> do
      context <- saveGlobDefs $ Program tds
      if not (memberVarEntry id context) && not (memberClassEntry id context)
        then Ok (insertClassEntry cls context)
        else Err $ DuplicateClass id

-- | Evaluate a single 'TopDef'. A functions is converted into a 'VarEntry'.
-- For classes, a 'ClassContext' is generated.
createTopDefEntry :: TopDef -> TypeResult (Either VarEntry ClassContext)
createTopDefEntry (FnDef typ id args _blk) = Ok $ Left (id, Fn typ (argTypes args))
createTopDefEntry (ClsDef id elems) = do
  res <- createClassItemEntries elems id
  return $ Right (id, Nothing, res)
createTopDefEntry (SubClsDef id super elems) = do
  res <- createClassItemEntries elems id
  return $ Right (id, Just super, res)

-- | Strips the identifiers of arguments in a list of arguments to convert it
-- into a list of types.
argTypes :: [Arg] -> [Type]
argTypes [] = []
argTypes (Argument typ _id : as) = typ : argTypes as

-- | Obtain the types of declarations in a class body.
createClassItemEntries :: [ClsItem] -> Ident -> TypeResult (Map Ident Type)
createClassItemEntries [] _ = return Map.empty
createClassItemEntries ((InstVar typ id) : rest) cls = do
  res <- createClassItemEntries rest cls
  if not $ Map.member id res
    then return $ Map.insert id typ res
    else throwError $ DuplicateInstanceVar id
createClassItemEntries ((MethDef typ id args _blk) : rest) cls = do
  res <- createClassItemEntries rest cls
  if not $ Map.member id res
    then return $ Map.insert id (Fn typ (argTypes args)) res
    else throwError $ DuplicateFunction id

-- * Type checking functions (second pass)

-- | The monad used by the type checker. Keeps track of the context using the
-- @State@ monad and any errors using the @Except@ monad.
newtype Chk a = MkChk (StateT Context (ExceptT TypeError Identity) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState Context,
      MonadError TypeError
    )

-- | Run function of 'Chk' monad.
runChk :: Context -> Chk AnnotatedProg -> TypeResult AnnotatedProg
runChk ctx (MkChk rd) = runIdentity $ runExceptT $ evalStateT rd ctx

-- | Typecheck the program. Main function for the second pass.
-- Functions and classes have to be present in context.
checkProgram :: Prog -> Chk AnnotatedProg
checkProgram (Program []) = return $ Program []
checkProgram (Program (td : rest)) = do
  checked <- checkTopDef td
  (Program other) <- checkProgram (Program rest)
  case checked of
    FnDef {} -> return $ Program $ checked : other
    cls@ClsDef {} -> do
      descr <- createClassDescriptor cls
      return $ Program $ descr : other
    cls@SubClsDef {} -> do
      descr <- createClassDescriptor cls
      return $ Program $ descr : other

-- | Typecheck a function's or class' body. Returns the input augmented with
-- type annotations if succesful or throws a 'TypeError' otherwise.
checkTopDef :: TopDef -> Chk TopDef
checkTopDef (FnDef typ id args (Block stmts)) = do
  newTop
  saveArgs args
  when (typ == Void) $ setReturnState Return
  annotated <- checkStmts stmts typ
  checkReturnState
  resetReturnState
  discardTop
  return $ FnDef typ id args (Block annotated)
checkTopDef (ClsDef id elems) = do
  pushClassTop id True
  enterClass id
  annotated <- checkClassBody elems
  discardTop
  leaveClass
  return $ ClsDef id annotated
checkTopDef (SubClsDef id _ elems) = checkTopDef $ ClsDef id elems

-- | Typecheck a list of declarations inside a class. Returns the input
-- augmented with type annotations if succesful or throws a 'TypeError'
-- otherwise.
checkClassBody :: [ClsItem] -> Chk [ClsItem]
checkClassBody [] = return []
checkClassBody (elem : elems) = do
  annotated <- checkClassItem elem
  next <- checkClassBody elems
  return $ annotated : next

-- | Typecheck a declaration inside a class. Returns the input augmented with
-- type annotations if succesful or throws a 'TypeError' otherwise.
checkClassItem :: ClsItem -> Chk ClsItem
checkClassItem var@(InstVar _ _) = return var -- nothing to do here
checkClassItem (MethDef typ id args (Block stmts)) = do
  newTop
  cls <- getCurrentClassName
  let self = selfArg $ fromJust cls
  saveArgs (self : args)
  when (typ == Void) $ setReturnState Return
  annotated <- checkStmts stmts typ
  checkReturnState
  discardTop
  return $ MethDef typ id args (Block annotated)

-- | Typecheck a list of statements. Returns the input augmented with type
-- annotations if succesful or throws a 'TypeError' otherwise.
checkStmts :: [Stmt] -> Type -> Chk [Stmt]
checkStmts stmts ret = case stmts of
  [] -> return []
  stmt : rest -> do
    -- traceM $ show stmt
    annotated <- checkStmt stmt ret
    next <- checkStmts rest ret
    return $ annotated : next

-- | Typecheck a single statement. Returns the input augmented with type
-- annotations if succesful or throws a 'TypeError' otherwise.
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
  annotated <- checkDeclItems items typ
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
-- annotations if succesful or throws a 'TypeError' otherwise.
checkExpr :: Expr -> Type -> Chk Expr
checkExpr expr typ = do
  annotated <- inferExpr expr
  let inferred = getType annotated
  sub <- isSubtype inferred typ
  if inferred == typ || sub
    then return annotated
    else throwError $ TypeMismatch inferred typ

-- | Check a variable's type. Throws a 'TypeError' on type mismatch.
checkVar :: LValue -> Type -> Chk ()
checkVar lval typ = do
  saved <- lookupVar lval
  if saved == typ
    then return ()
    else throwError $ TypeMismatch saved typ

-- | Typecheck a list of 'Item's. Returns the input augmented with type
-- annotations if succesful or throws a 'TypeError' otherwise.
checkDeclItems :: [DeclItem] -> Type -> Chk [DeclItem]
checkDeclItems [] typ = return []
checkDeclItems (item : items) typ = case item of
  NoInit id -> do
    extendContext (id, typ)
    next <- checkDeclItems items typ
    return $ item : next
  Init id expr -> do
    annotated <- inferExpr expr
    let inferred = getType annotated
    sub <- isSubtype inferred typ
    if inferred == typ || sub
      then do
        extendContext (id, typ)
        next <- checkDeclItems items typ
        return $ Init id annotated : next
      else throwError $ TypeMismatch inferred typ

-- | Typecheck the arguments of a function call. Returns the input augmented
-- with type annotations if succesful or throws a 'TypeError' otherwise.
checkFnArgs :: [Expr] -> Type -> Chk [Expr]
checkFnArgs exprs fntype@(Fn ret types) = case (exprs, types) of
  ([], []) -> return []
  (expr : exprs, typ : types) -> do
    annotated <- inferExpr expr
    let inferred = getType annotated
    sub <- isSubtype inferred typ
    if inferred == typ || sub
      then do
        next <- checkFnArgs exprs (Fn ret types)
        return $ annotated : next
      else throwError $ TypeMismatch inferred typ
  (_, _) -> throwError ArgumentMismatch
checkFnArgs _exprs typ = throwError $ ExpectedFnType typ

-- | Infer the type of an expression. Augments the expression and (if applicable)
-- any subexpressions with type annotations. Typechecks subexpressions, which
-- can lead to a 'TypeError' being throwin.
inferExpr :: Expr -> Chk Expr
inferExpr expr@(EVar id) = ETyped expr <$> lookupVar (Id id)
inferExpr expr@(ELitInt _) = return $ ETyped expr Int
inferExpr expr@(ELitDoub _) = return $ ETyped expr Double
inferExpr ELitTrue = return $ ETyped ELitTrue Boolean
inferExpr ELitFalse = return $ ETyped ELitFalse Boolean
inferExpr (ENull id) = do
  typ <- lookupClassName id
  return $ ETyped (ENull id) typ
inferExpr (ECall id exprs) = do
  typ <- lookupVar (Id id)
  annotated <- checkFnArgs exprs typ
  -- only type with return type of function, not with function type itself
  case typ of
    (Fn ret _) -> return $ ETyped (ECall id annotated) ret
inferExpr expr@(EString _) = return $ ETyped expr String
inferExpr (ENew typ sizes) = case sizes of
  [] -> inferExpr $ EObjInit typ
  _ -> inferExpr $ EArrAlloc typ sizes
inferExpr (EArrIndex expr1 expr2) = do
  annotatedArr <- inferExpr expr1
  annotatedIndex <- checkExpr expr2 Int
  case annotatedArr of
    ETyped _ (Array inner) -> return $ ETyped (EArrIndex annotatedArr annotatedIndex) inner
    ETyped _ typ -> throwError $ ExpectedArrType typ
inferExpr (EDot expr (EVar id)) =
  if id == "length"
    then inferExpr (EArrLen expr)
    else throwError $ UnknownProperty id
inferExpr (EDot expr (ECall id args)) = inferExpr (EMethCall expr id args)
inferExpr (EDot _ expr) = throwError $ InvalidAccessor expr
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
  catchError
    ( do
        (annotated1, annotated2) <- inferBinObj expr1 expr2
        return $ ETyped (ERel annotated1 op annotated2) Boolean
    )
    ( \_ -> do
        (annotated1, annotated2) <- inferBin expr1 expr2 exprType
        return $ ETyped (ERel annotated1 op annotated2) Boolean
    )
  where
    exprType = if op == EQU || op == NE then [Int, Double, Boolean] else [Int, Double]
inferExpr (EAnd expr1 expr2) = do
  (annotated1, annotated2) <- inferBin expr1 expr2 [Boolean]
  return $ ETyped (EAnd annotated1 annotated2) Boolean
inferExpr (EOr expr1 expr2) = do
  (annotated1, annotated2) <- inferBin expr1 expr2 [Boolean]
  return $ ETyped (EOr annotated1 annotated2) Boolean
inferExpr (EObjInit typ) =
  case typ of
    Object id -> return $ ETyped (EObjInit typ) typ
    _ -> throwError $ ExpectedObjType typ
inferExpr (EArrAlloc innerType sizes) = do
  (annotated, arrType) <- inferSizeItems sizes innerType
  return $ ETyped (EArrAlloc innerType annotated) arrType
inferExpr (EMethCall expr id exprs) = do
  annotatedObj <- inferExpr expr
  case annotatedObj of
    ETyped _ (Object cls) -> do
      typ <- lookupMethodName id cls
      annotatedArgs <- checkFnArgs exprs typ
      -- only type with return type of function, not with function type itself
      case typ of
        (Fn ret _) -> return $ ETyped (EMethCall annotatedObj id annotatedArgs) ret
    ETyped _ typ -> throwError $ ExpectedObjType typ
inferExpr (EArrLen expr) = do
  annotated <- inferExpr expr
  case annotated of
    ETyped inner (Array typ) -> return $ ETyped (EArrLen annotated) Int
    ETyped inner typ -> throwError $ ExpectedArrType typ
inferExpr expr = error $ "Don't know how to handle expression " ++ show expr

-- | Infer the type for an unary expression and return the expression augmented
-- with type information. The inferred type has to be one of the given types.
-- Otherwise, a 'TypeError' is thrown. 'Array' and 'Object' types are matched
-- exactly.
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
-- thrown. 'Array' and 'Object' types are matched exactly.
inferBin :: Expr -> Expr -> [Type] -> Chk (Expr, Expr)
inferBin expr1 expr2 types = do
  annotated1 <- inferExpr expr1
  let inferred = getType annotated1
  if inferred `elem` types
    then do
      annotated2 <- checkExpr expr2 inferred
      return (annotated1, annotated2)
    else throwError $ TypeMismatchOverloaded inferred types

-- | Infer the type for a binary expression and return both augmented with type
-- information each. Both subexpressions have to be objects and be of the same
-- type or one has to be a subtype of the other. Otherwise, a 'TypeError' is
-- thrown.
inferBinObj :: Expr -> Expr -> Chk (Expr, Expr)
inferBinObj expr1 expr2 = do
  annotated1 <- inferExpr expr1
  let inferred = getType annotated1
  case inferred of
    Object cls -> do
      annotated2 <- checkExpr expr2 inferred
      return (annotated1, annotated2)
    _ -> throwError $ ExpectedObjType inferred

-- | Infer the type of an array allocation expression with a given base type
-- based on the number of specified array dimensions. The base type is wrapped
-- in 'Array' constructors corresponding to that number. Expressions specifying
-- the size of each dimension are type checked and type annotated. Throws a
-- 'TypeError', if a size is not 'Int'.
inferSizeItems :: [SizeItem] -> Type -> Chk ([SizeItem], Type)
inferSizeItems [] typ = return ([], typ)
inferSizeItems (SizeSpec expr : rest) typ = do
  annotated <- checkExpr expr Int
  (items, wrappedType) <- inferSizeItems rest typ
  return (SizeSpec annotated : items, Array wrappedType)

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

-- | Save the function arguments in the variable context stack top layer.
saveArgs :: [Arg] -> Chk ()
saveArgs (arg : args) = case arg of
  Argument typ id -> do
    extendContext (id, typ)
    saveArgs args
saveArgs [] = return ()

-- | Get the type of a typed expression.
getType :: Expr -> Type
getType expr = case expr of
  ETyped _ typ -> typ
  _ -> error "Not a typed expression!"

-- | Check if an object type is a subtype of another object type. Returns
-- 'False' if one of the types is not an object.
isSubtype :: Type -> Type -> Chk Bool
isSubtype (Object cls1) (Object cls2) =
  if cls1 == cls2
    then return True
    else do
      super <- getSuperClassName cls1
      case super of
        Just super -> isSubtype (Object super) (Object cls2)
        Nothing -> return False
isSubtype _ _ = return False

selfArg :: Ident -> Arg
selfArg cls = Argument (Object cls) "self"

createClassDescriptor :: TopDef -> Chk TopDef
createClassDescriptor (SubClsDef id _ items) = createClassDescriptor $ ClsDef id items
createClassDescriptor (ClsDef id items) = do
  addClassItems (ClsDescr id items [] []) id
  where
    addClassItems :: TopDef -> Ident -> Chk TopDef
    addClassItems descr@(ClsDescr cls items ivars meths) cur = do
      ctx <- get
      case Map.lookup cur (classes ctx) of
        Just (_, Just super, mems) -> do
          descrSuper <- addClassItems descr super
          case descrSuper of
            (ClsDescr _ _ superVars superMeths) -> do
              let (ivars, meths) = getItems (Map.toList mems) cur
              return $ ClsDescr cls items (mergeVars superVars ivars) (mergeMeths superMeths meths)
        Just (_, Nothing, mems) -> do
          let (ivars, meths) = getItems (Map.toList mems) cur
          return $ ClsDescr cls items ivars meths
    getItems :: [(Ident, Type)] -> Ident -> ([ClsVar], [ClsMeth])
    getItems [] _ = return []
    getItems ((id, Fn ret args) : rest) cls =
      let (ivars, meths) = getItems rest cls
       in (ivars, ClsMeth cls id (Fn ret args) : meths)
    getItems ((id, typ) : rest) cls =
      let (ivars, meths) = getItems rest cls
       in (ClsVar typ id : ivars, meths)
    mergeVars :: [ClsVar] -> [ClsVar] -> [ClsVar]
    mergeVars superVars thisVars = superVars ++ thisVars
    mergeMeths :: [ClsMeth] -> [ClsMeth] -> [ClsMeth]
    mergeMeths superMeths [] = superMeths
    mergeMeths [] thisMeths = thisMeths
    mergeMeths (superMeth@(ClsMeth super id typ) : superMeths) thisMeths =
      case find (\(ClsMeth _ other _) -> id == other) thisMeths of
        Just meth -> mergeMeths superMeths thisMeths
        Nothing -> superMeth : mergeMeths superMeths thisMeths

-- * Context

-- | The local context.
data Context = Context
  { -- | The variable context stack. The bottom layer contains global
    -- declarations, including function definitions. Each statement block
    -- introduces a new layer.
    vars :: ContextStack,
    -- | Save whether the current function had a @return@ on the currently
    -- traversed code path.
    returnState :: ReturnState,
    -- | The class definitions. Maps between class names and their context.
    classes :: Map Ident ClassContext,
    -- | The current class name
    curClass :: Maybe Ident
  }
  deriving (Show, Eq)

-- | The context of a class. Contains a mapping between names and types of
-- instance variables and methods. Also contains name of superclass
type ClassContext = (Ident, Maybe Ident, Map Ident Type)

-- | The context stack. Consists of individual variable contexts.
type ContextStack = [Map Ident Type]

-- | A variable entry in the local context.
type VarEntry = (Ident, Type)

-- | An empty context.
emptyContext :: Context
emptyContext =
  Context
    { vars = [Map.empty],
      returnState = NoReturn,
      classes = Map.empty,
      curClass = Nothing
    }

-- | Context that contains the prelude functions of Javalette.
preludeContext :: Context
preludeContext =
  insertVarEntry (Ident "printInt", Fn Void [Int]) $
    insertVarEntry (Ident "printDouble", Fn Void [Double]) $
      insertVarEntry (Ident "printString", Fn Void [String]) $
        insertVarEntry (Ident "readInt", Fn Int []) $
          insertVarEntry (Ident "readDouble", Fn Double []) emptyContext

-- | Insert an entry into the top layer of the variable context stack.
insertVarEntry :: VarEntry -> Context -> Context
insertVarEntry (id, typ) ctx = ctx {vars = Map.insert id typ (head $ vars ctx) : tail (vars ctx)}

-- | Look up whether an entry is present in the top layer of the variable
-- context stack. Returns 'Just' with the entry if found, otherwise 'Nothing'.
lookupVarEntry :: Ident -> Context -> Maybe Type
lookupVarEntry id ctx = Map.lookup id (head $ vars ctx)

-- | Look up whether an entry is present in the top layer of the variable
-- context stack.
memberVarEntry :: Ident -> Context -> Bool
memberVarEntry id ctx = Map.member id (head $ vars ctx)

-- | Insert an entry into the top of the context stack
extendContext :: VarEntry -> Chk ()
extendContext (id, typ) = do
  ctx <- get
  if not $ memberVarEntry id ctx
    then put $ insertVarEntry (id, typ) ctx
    else throwError $ DuplicateVariable id

-- | Discard the top context of the context stack.
discardTop :: Chk ()
discardTop = do
  ctx <- get
  put $ ctx {vars = tail $ vars ctx}

-- | Add an empty context on top of the context stack.
newTop :: Chk ()
newTop = do
  ctx <- get
  put $ ctx {vars = Map.empty : vars ctx}

-- | Push the class contexts of a class hierarchy on top of the variable context
-- stack.
pushClassTop :: Ident -> Bool -> Chk ()
pushClassTop id isTop = do
  ctx <- get
  case fromJust $ Map.lookup id (classes ctx) of
    (_, Nothing, mems) -> put $ ctx {vars = filterNonFns mems (not isTop) : vars ctx}
    (_, Just super, mems) -> do
      pushClassTop super False
      put $ ctx {vars = filterNonFns mems (not isTop) : vars ctx}
  where
    filterNonFns :: Map Ident Type -> Bool -> Map Ident Type
    filterNonFns elems doit =
      if doit
        then
          Map.filter
            ( \case
                Fn ret args -> True
                _ -> False
            )
            elems
        else elems

-- | Search the context stack for a variable or function and return its type.
-- Takes the first element that is discovered.
--
-- In case an lvalue contains an index, the type of the base variable is
-- searched for and the appropriate 'Array' type applied afterwards.
lookupVar :: LValue -> Chk Type
lookupVar lval = do
  let id = unwrapLVal lval
  ctx <- get
  case stackLookup id (vars ctx) of
    Just typ -> return $ applyIndexing lval typ
    Nothing -> throwError $ UndeclaredVar id
  where
    -- Get the identifier burried in an lvalue
    unwrapLVal :: LValue -> Ident
    unwrapLVal (ArrId lval _) = unwrapLVal lval
    unwrapLVal (Id id) = id
    -- Adapt the type of the identifier to the lvalue expressions applied to it.
    applyIndexing :: LValue -> Type -> Type
    applyIndexing (ArrId lval _) (Array typ) = applyIndexing lval typ
    applyIndexing (Id _) typ = typ
    stackLookup :: Ident -> ContextStack -> Maybe Type
    stackLookup id (top : rest) = case Map.lookup id top of
      Just typ -> Just typ
      Nothing -> stackLookup id rest
    stackLookup _id [] = Nothing

-- | Insert a class entry into the context.
insertClassEntry :: ClassContext -> Context -> Context
insertClassEntry cls@(id, _, _) ctx = ctx {classes = Map.insert id cls (classes ctx)}

-- | Search for a class entry in the context. Returns 'Just' with the entry if
-- found or 'Nothing' otherwise.
lookupClassEntry :: Ident -> Context -> Maybe ClassContext
lookupClassEntry id ctx = Map.lookup id (classes ctx)

-- | Search for a class entry in the context and return whether it is present.
memberClassEntry :: Ident -> Context -> Bool
memberClassEntry id ctx = Map.member id (classes ctx)

-- | Set the current class name.
enterClass :: Ident -> Chk ()
enterClass id = do
  ctx <- get
  put $ ctx {curClass = Just id}

-- | Clear the current class name.
leaveClass :: Chk ()
leaveClass = do
  ctx <- get
  put $ ctx {curClass = Nothing}

-- | Get the name of the class the current code is written in.
getCurrentClassName :: Chk (Maybe Ident)
getCurrentClassName = gets curClass

-- | Get the name of the super class of a class. Returns a 'Just' with the name
-- of the super class if it has one or 'Nothing' otherwise. Returns a
-- 'TypeError' if the base class does not exist.
getSuperClassName :: Ident -> Chk (Maybe Ident)
getSuperClassName id = do
  ctx <- get
  case Map.lookup id (classes ctx) of
    Just (_, super, _) -> return super
    Nothing -> throwError $ ClassNotFound id

-- | Search the registered classes for the class name. Returns the corresponding
-- 'Object' type if the class exists. Throws a 'TypeError' otherwise.
lookupClassName :: Ident -> Chk Type
lookupClassName id = do
  ctx <- get
  if Map.member id (classes ctx)
    then return $ Object id
    else throwError $ ClassNotFound id

-- | Search for a method in a class and all its super classes. Returns the
-- corresponding function type if the method exists. Throws a 'TypeError'
-- otherwise.
lookupMethodName :: Ident -> Ident -> Chk Type
lookupMethodName id cls = do
  ctx <- get
  case Map.lookup cls (classes ctx) of
    Just (name, super, mems) ->
      case Map.lookup id mems of
        Just typ -> return typ
        Nothing ->
          case super of
            Just superId -> lookupMethodName id superId
            Nothing -> throwError $ MethodNotFound id
    Nothing -> throwError $ ClassNotFound id

-- * Return State

-- | Examine the current return state. Throws an error if 'NoReturn' is found.
checkReturnState :: Chk ()
checkReturnState = do
  state <- getReturnState
  case state of
    Return -> return ()
    NoReturn -> throwError NonReturningPath

-- | Get the current 'ReturnState' from the context.
getReturnState :: Chk ReturnState
getReturnState = gets returnState

-- | Set the current 'ReturnState' in the context. Uses '(<>)' to merge the
-- present return state with the new one.
setReturnState :: ReturnState -> Chk ()
setReturnState state = do
  ctx <- get
  put $ ctx {returnState = returnState ctx <> state}

-- | Set the current 'ReturnState' in the context. Overrides the present return
-- state.
overrideReturnState :: ReturnState -> Chk ()
overrideReturnState state = do
  ctx <- get
  put $ ctx {returnState = state}

-- | Reset the current 'ReturnState' in the context to 'NoReturn'.
resetReturnState :: Chk ()
resetReturnState = do
  ctx <- get
  put $ ctx {returnState = NoReturn}
