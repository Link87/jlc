{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_HADDOCK prune, ignore-exports, show-extensions #-}

-- | Type check a Javalette Abstract Syntax Tree (AST) with the 'check'
-- function. Translates the original Javalette AST into a Typed AST. Performs
-- also validity and return checking.
--
-- Converts syntax elements where the LR parser is not powerful enough. That is,
-- lvalue expressions are converted to 'LValue's and 'ENew' (@new Expr@) and
-- 'EDot' (@Expr . Expr@) expressions are converted to the corresponding
-- specialised expression for classes or arrays. Creates also class descriptor
-- annotations in 'ClsDef's.
module Javalette.Check.TypeCheck
  ( TypeError (..),
    AnnotatedProg,
    check,
  )
where

import Control.Monad (when, (>=>))
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
    modify,
  )
import Data.Foldable (foldrM)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Debug.Trace
import Javalette.Check.Return (ReturnState (..), both)
import Javalette.Check.TypeError (TypeError (..), TypeResult (..), pattern Err, pattern Ok)
import Javalette.Check.TypedAST
import qualified Javalette.Lang.Abs as J

-- * Type Check

-- | A type-annotated program. Returned by the type checker.
type AnnotatedProg = TypedProg

-- | Run the type checker on a program.
check :: J.Prog -> TypeResult AnnotatedProg
check prog@(J.Program tds) = do
  -- First pass: collect identifiers and what kind of definition they belong to.
  defs <- collectGlobIds prog
  runChk preludeContext $ do
    -- Second pass: save fns and classes. Also check for main function.
    setDefKinds defs
    mapM_ saveTopDef tds
    main <- lookupFnEntry "main"
    case main of
      Just (Fn Int []) -> return ()
      Just (Fn Int _) -> throwError MainArguments
      Just (Fn _ _) -> throwError MainReturnType
      Nothing -> throwError MainNotFound
    -- Third pass: check types in fns and methods
    checkProgram prog

-- * Collecting global identifiers (first pass)

-- | The kinds of definitions that can occur on the top-level.
data DefKind = ClsKind
  deriving (Show, Eq)

-- | Collect the names and according 'DefKind's of top-level definitions. Throws
-- a 'TypeError', if an identifier occurs more than once.
--
-- Functions do not share a name space with type definitions such as classes and
-- structs and are therefore ignored.
collectGlobIds :: J.Prog -> TypeResult DefKinds
collectGlobIds (J.Program []) = Err EmptyProgram
collectGlobIds (J.Program tds) = foldrM insertDef Map.empty tds
  where
    insertDef :: J.TopDef -> DefKinds -> TypeResult DefKinds
    insertDef td defs
      | Map.notMember (topDefId td) defs = return $ Map.insert (topDefId td) (topDefKind td) defs
      | otherwise = Err $ DuplicateDefinition (topDefId td)
    topDefId (J.FnDef _ id _ _) = toTypeId id
    topDefId (J.ClsDef id _) = toTypeId id
    topDefId (J.SubClsDef id _ _) = toTypeId id
    topDefKind J.ClsDef {} = ClsKind
    topDefKind J.SubClsDef {} = ClsKind

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

-- | Save a 'TopDef' into the current 'Context'.
saveTopDef :: J.TopDef -> Chk ()
saveTopDef (J.FnDef ret id params _blk) = do
  let fnId = toFnId id
  castedRet <- castType ret
  castedArgs <- paramTypes params
  extendFnContext (toFnId id, castedRet, castedArgs)
saveTopDef (J.ClsDef id elems) = do
  let clsId = toTypeId id
  meths <- createMethodEntries elems clsId
  instVars <- createInstVarEntries elems clsId
  insertClassEntry $
    ClsContext
      { clsName = clsId,
        super = Nothing,
        meths = meths,
        instVars = instVars
      }
saveTopDef (J.SubClsDef id super elems) = do
  let clsId = toTypeId id
  meths <- createMethodEntries elems clsId
  instVars <- createInstVarEntries elems clsId
  insertClassEntry $
    ClsContext
      { clsName = clsId,
        super = Just (toTypeId super),
        meths = meths,
        instVars = instVars
      }

-- | Collect the types of methods in a class body.
createMethodEntries :: [J.ClsItem] -> TypeIdent -> Chk (Map FnIdent Type)
createMethodEntries [] _ = return Map.empty
createMethodEntries (J.InstVar {} : rest) clsId = createMethodEntries rest clsId
createMethodEntries ((J.MethDef ret id params _blk) : rest) clsId = do
  let fnId = toFnId id
  castedRet <- castType ret
  castedArgs <- fmap (Object clsId :) (paramTypes params)
  meths <- createMethodEntries rest clsId
  if not $ Map.member fnId meths
    then return $ Map.insert fnId (Fn castedRet castedArgs) meths
    else throwError $ DuplicateMethod fnId clsId

-- | Collect the types of instance variables in a class body.
createInstVarEntries :: [J.ClsItem] -> TypeIdent -> Chk (Map VarIdent Type)
createInstVarEntries [] _ = return Map.empty
createInstVarEntries ((J.InstVar typ id) : rest) clsId = do
  let varId = toVarId id
  casted <- castType typ
  instVars <- createInstVarEntries rest clsId
  if not $ Map.member varId instVars
    then return $ Map.insert varId casted instVars
    else throwError $ DuplicateInstanceVar varId clsId
createInstVarEntries (J.MethDef {} : rest) clsId = createInstVarEntries rest clsId

-- * Type checking functions (third pass)

-- | Typecheck the program. Main function for the third pass.
-- Functions and classes have to be present in context from previous passes.
checkProgram :: J.Prog -> Chk AnnotatedProg
checkProgram (J.Program tds) = Program <$> mapM checkTopDef tds

-- | Typecheck a function's or class' body. Returns the input augmented with
-- type annotations if succesful or throws a 'TypeError' otherwise.
checkTopDef :: J.TopDef -> Chk TopDef
checkTopDef (J.FnDef ret id params (J.Block stmts)) = do
  castedRet <- castType ret
  castedArgs <- mapM castParam params
  newVarTop
  saveParams params
  when (castedRet == Void) $ setReturnState Return
  annotated <- checkStmts stmts castedRet
  checkReturnState
  resetReturnState
  discardVarTop
  return $ FnDef castedRet (toFnId id) castedArgs (Block annotated)
checkTopDef (J.ClsDef id elems) = do
  let clsId = toTypeId id
  pushClassTop clsId True
  enterClass clsId
  annotated <- mapM checkClassItem elems
  discardClassTop
  leaveClass
  createClassDescriptor clsId annotated
checkTopDef (J.SubClsDef id _ elems) = checkTopDef $ J.ClsDef id elems

-- | Typecheck a declaration inside a class. Returns the input augmented with
-- type annotations if succesful or throws a 'TypeError' otherwise.
checkClassItem :: J.ClsItem -> Chk ClsItem
checkClassItem (J.InstVar typ id) = do
  casted <- castType typ
  return $ InstVar casted (toVarId id)
checkClassItem (J.MethDef ret id params (J.Block stmts)) = do
  castedRet <- castType ret
  castedArgs <- mapM castParam params
  self <- selfParam . J.Ident . ident . fromJust <$> getCurrentClassName
  castedSelf <- castParam self
  newVarTop
  saveParams (self : params)
  when (castedRet == Void) $ setReturnState Return
  annotated <- checkStmts stmts castedRet
  checkReturnState
  resetReturnState
  discardVarTop
  return $ MethDef castedRet (toFnId id) (castedSelf : castedArgs) (Block annotated)

-- | Typecheck a list of statements. Returns the input augmented with type
-- annotations if succesful or throws a 'TypeError' otherwise.
checkStmts :: [J.Stmt] -> Type -> Chk [Stmt]
checkStmts stmts ret = mapM (`checkStmt` ret) stmts

-- | Typecheck a single statement. Returns the input augmented with type
-- annotations if succesful or throws a 'TypeError' otherwise.
checkStmt :: J.Stmt -> Type -> Chk Stmt
checkStmt J.Empty _ = return Empty
checkStmt (J.BStmt (J.Block stmts)) ret = do
  newVarTop
  annotated <- checkStmts stmts ret
  state <- getReturnState
  discardVarTop
  setReturnState state
  return (BStmt (Block annotated))
checkStmt (J.Decl typ items) _ = do
  casted <- castType typ
  annotated <- checkDeclItems items casted
  return $ Decl casted annotated
checkStmt (J.Ass expr1 expr2) _ = do
  lval <- coerceLVal expr1
  annotated <- checkExpr expr2 (getLValType lval)
  return $ Ass lval annotated
checkStmt (J.Incr expr) _ = do
  lval <- coerceLVal expr
  checkLVal lval Int
  return $ Incr lval
checkStmt (J.Decr expr) _ = do
  lval <- coerceLVal expr
  checkLVal lval Int
  return $ Decr lval
checkStmt (J.Ret expr) ret = do
  annotated <- checkExpr expr ret
  setReturnState Return
  return $ Ret annotated
checkStmt J.VRet ret =
  if ret == Void
    then setReturnState Return >> return VRet
    else throwError $ TypeMismatch ret Void
checkStmt (J.Cond expr stmt) ret = do
  annotatedExpr <- checkExpr expr Boolean
  state <- getReturnState
  annnotatedStmt <- checkStmt stmt ret
  case expr of
    J.ELitTrue -> return $ Cond annotatedExpr annnotatedStmt
    _ -> do
      overrideReturnState state
      return $ Cond annotatedExpr annnotatedStmt
checkStmt (J.CondElse expr stmt1 stmt2) ret = do
  annotatedExpr <- checkExpr expr Boolean
  state <- getReturnState
  annotatedStmt1 <- checkStmt stmt1 ret
  branch1 <- getReturnState
  overrideReturnState state
  annotatedStmt2 <- checkStmt stmt2 ret
  branch2 <- getReturnState
  case expr of
    J.ELitTrue -> do
      overrideReturnState (state <> branch1)
      return $ CondElse annotatedExpr annotatedStmt1 annotatedStmt2
    J.ELitFalse -> do
      overrideReturnState (state <> branch2)
      return $ CondElse annotatedExpr annotatedStmt1 annotatedStmt2
    _ -> do
      overrideReturnState (state <> both branch1 branch2)
      return $ CondElse annotatedExpr annotatedStmt1 annotatedStmt2
checkStmt (J.While expr stmt) ret = do
  annotatedExpr <- checkExpr expr Boolean
  state <- getReturnState
  annotatedStmt <- checkStmt stmt ret
  case expr of
    J.ELitTrue -> return $ While annotatedExpr annotatedStmt
    _ -> do
      overrideReturnState state
      return $ While annotatedExpr annotatedStmt
checkStmt (J.ForEach typ id expr stmt) ret = do
  let varId = toVarId id
  casted <- castType typ
  annotatedExpr <- checkExpr expr (Array casted)
  newVarTop
  extendContext (varId, casted)
  state <- getReturnState
  annotatedStmt <- checkStmt stmt ret
  discardVarTop
  overrideReturnState state
  return $ ForEach casted varId annotatedExpr annotatedStmt
checkStmt (J.SExpr expr) _ = do
  annotated <- checkExpr expr Void
  return $ SExpr annotated

-- | Typecheck a single expression. Returns the input augmented with type
-- annotations if succesful or throws a 'TypeError' otherwise.
checkExpr :: J.Expr -> Type -> Chk TExpr
checkExpr expr typ = do
  annotated <- inferExpr expr
  let inferred = getType annotated
  sub <- isSubtype inferred typ
  if inferred == typ || sub
    then return annotated
    else throwError $ TypeMismatch inferred typ

-- | Check a variable's type. Throws a 'TypeError' on type mismatch.
checkLVal :: LValue -> Type -> Chk ()
checkLVal lval typ = do
  let lvalType = getLValType lval
  if lvalType == typ
    then return ()
    else throwError $ TypeMismatch lvalType typ

-- | Typecheck a list of 'Item's. Returns the input augmented with type
-- annotations if succesful or throws a 'TypeError' otherwise.
checkDeclItems :: [J.DeclItem] -> Type -> Chk [DeclItem]
checkDeclItems [] typ = return []
checkDeclItems (item : items) typ = case item of
  J.NoInit id -> do
    let varId = toVarId id
    extendContext (varId, typ)
    next <- checkDeclItems items typ
    return $ NoInit varId : next
  J.Init id expr -> do
    let varId = toVarId id
    annotated <- inferExpr expr
    let inferred = getType annotated
    sub <- isSubtype inferred typ
    if inferred == typ || sub
      then do
        extendContext (varId, typ)
        next <- checkDeclItems items typ
        return $ Init varId annotated : next
      else throwError $ TypeMismatch inferred typ

-- | Typecheck the arguments of a function call. Returns the input augmented
-- with type annotations if succesful or throws a 'TypeError' otherwise. If
-- 'True' is specified, the function is treated as a method and the first
-- parameter is ignored, i.e. the first argument is checked against the second
-- parameter.
checkFnArgs :: [J.Expr] -> Type -> Bool -> Chk [TExpr]
checkFnArgs exprs (Fn ret (typ : types)) True = checkFnArgs exprs (Fn ret types) False
checkFnArgs exprs fntype@(Fn ret types) False = case (exprs, types) of
  ([], []) -> return []
  (expr : exprs, typ : types) -> do
    annotated <- inferExpr expr
    let inferred = getType annotated
    sub <- isSubtype inferred typ
    if inferred == typ || sub
      then do
        next <- checkFnArgs exprs (Fn ret types) False
        return $ annotated : next
      else throwError $ TypeMismatch inferred typ
  (_, _) -> throwError ArgumentMismatch
checkFnArgs _exprs typ _ = throwError $ ExpectedFnType typ

-- | Infer the type of an expression. Augments the expression and (if applicable)
-- any subexpressions with type annotations. Typechecks subexpressions, which
-- can lead to a 'TypeError' being throwin.
inferExpr :: J.Expr -> Chk TExpr
inferExpr (J.EVar id) = let varId = toVarId id in EVar varId <$> findVar varId
inferExpr (J.ELitInt int) = return $ ELitInt int
inferExpr (J.ELitDouble doub) = return $ ELitDouble doub
inferExpr J.ELitTrue = return ELitTrue
inferExpr J.ELitFalse = return ELitFalse
inferExpr (J.ENull id) = do
  let clsId = toTypeId id
  typ <- lookupClass clsId
  return $ ENull clsId typ
inferExpr (J.ECall id exprs) = do
  let fnId = toFnId id
  typ <- findFn fnId
  annotated <- checkFnArgs exprs typ False
  -- only type with return type of function, not with function type itself
  case typ of
    (Fn ret _) -> return $ ECall fnId annotated ret
inferExpr (J.EString str) = return $ EString str
inferExpr (J.ENew typ sizes) = do
  casted <- castType typ
  case sizes of
    [] ->
      case casted of
        Object clsId -> return $ EObjInit casted
        _ -> throwError $ ExpectedObjType casted
    _ -> do
      (annotated, arrType) <- inferSizeItems sizes casted
      return $ EArrAlloc casted annotated arrType
inferExpr (J.EArrIndex expr1 expr2) = do
  annotatedArr <- inferExpr expr1
  annotatedIndex <- checkExpr expr2 Int
  case getType annotatedArr of
    (Array inner) -> return $ EArrIndex annotatedArr annotatedIndex inner
    typ -> throwError $ ExpectedArrType typ
inferExpr (J.EDot expr (J.EVar id)) = do
  let varId = toVarId id
  if varId == "length"
    then do
      annotated <- inferExpr expr
      case getType annotated of
        (Array typ) -> return $ EArrLen annotated
        typ -> throwError $ ExpectedArrType typ
    else throwError $ UnknownProperty varId
inferExpr (J.EDot expr (J.ECall id args)) = do
  let fnId = toFnId id
  annotatedObj <- inferExpr expr
  case getType annotatedObj of
    (Object cls) -> do
      typ <- lookupMethod fnId cls
      annotatedArgs <- checkFnArgs args typ True
      -- only type with return type of function, not with function type itself
      case typ of
        (Fn ret _) -> return $ EMethCall annotatedObj fnId annotatedArgs ret
    typ -> throwError $ ExpectedObjType typ
inferExpr (J.EDot _ expr) = throwError $ InvalidAccessor expr
inferExpr (J.ENeg expr) = do
  annotated <- inferUn expr [Int, Double]
  return $ ENeg annotated (getType annotated)
inferExpr (J.ENot expr) = do
  annotated <- checkExpr expr Boolean
  return $ ENot annotated
inferExpr (J.EMul expr1 op expr2) = do
  (annotated1, annotated2) <- inferBin expr1 expr2 exprType
  return $ EMul annotated1 (castMulOp op) annotated2 (getType annotated1)
  where
    exprType = if castMulOp op == Mod then [Int] else [Int, Double]
inferExpr (J.EAdd expr1 op expr2) = do
  (annotated1, annotated2) <- inferBin expr1 expr2 [Int, Double]
  return $ EAdd annotated1 (castAddOp op) annotated2 (getType annotated1)
inferExpr (J.ERel expr1 op expr2) = do
  catchError
    ( do
        (annotated1, annotated2) <- inferBinObj expr1 expr2
        return $ ERel annotated1 relOp annotated2
    )
    ( \_ -> do
        (annotated1, annotated2) <- inferBin expr1 expr2 exprType
        return $ ERel annotated1 relOp annotated2
    )
  where
    relOp = castRelOp op
    exprType = if relOp == EQU || relOp == NE then [Int, Double, Boolean] else [Int, Double]
inferExpr (J.EAnd expr1 expr2) = do
  (annotated1, annotated2) <- inferBin expr1 expr2 [Boolean]
  return $ EAnd annotated1 annotated2
inferExpr (J.EOr expr1 expr2) = do
  (annotated1, annotated2) <- inferBin expr1 expr2 [Boolean]
  return $ EOr annotated1 annotated2

-- inferExpr expr = error $ "Don't know how to handle expression " ++ show expr

-- | Infer the type for an unary expression and return the expression augmented
-- with type information. The inferred type has to be one of the given types.
-- Otherwise, a 'TypeError' is thrown. 'Array' and 'Object' types are matched
-- exactly.
inferUn :: J.Expr -> [Type] -> Chk TExpr
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
inferBin :: J.Expr -> J.Expr -> [Type] -> Chk (TExpr, TExpr)
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
inferBinObj :: J.Expr -> J.Expr -> Chk (TExpr, TExpr)
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
inferSizeItems :: [J.SizeItem] -> Type -> Chk ([SizeItem], Type)
inferSizeItems [] typ = return ([], typ)
inferSizeItems (J.SizeSpec expr : rest) typ = do
  annotated <- checkExpr expr Int
  (items, wrappedType) <- inferSizeItems rest typ
  return (SizeSpec annotated : items, Array wrappedType)

-- * Type checking helper functions

-- | Convert a list of parameters into a list of corresponding types.
paramTypes :: [J.Param] -> Chk [Type]
paramTypes = mapM ((\(J.Parameter typ _id) -> return typ) >=> castType)

-- | Convert an expression into an lvalue. Performs type checks on nested
-- expressions. Fails with a 'TypeError', if an expression cannot occur as an
-- lvalue.
coerceLVal :: J.Expr -> Chk LValue
coerceLVal (J.EVar id) = let varId = toVarId id in VarVal varId <$> findVar varId
coerceLVal (J.EArrIndex expr1 expr2) = do
  lval <- coerceLVal expr1
  annotated <- checkExpr expr2 Int
  case getLValType lval of
    Array typ -> return $ ArrVal lval annotated typ
coerceLVal expr = throwError $ ExpectedLValue expr

-- | Save the function arguments in the variable context stack top layer.
saveParams :: [J.Param] -> Chk ()
saveParams =
  mapM_
    ( \(J.Parameter typ id) -> do
        casted <- castType typ
        extendContext (toVarId id, casted)
    )

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

-- | The @self@ argument of methods. Requires the name of the class that the
-- method belongs to.
selfParam :: J.Ident -> J.Param
selfParam id = J.Parameter (J.Named id) "self"

-- | Generate a 'ClsDef' class descriptor from a classes identifier and members.
createClassDescriptor :: TypeIdent -> [ClsItem] -> Chk TopDef
createClassDescriptor clsId items = do
  ivars <- instVarList clsId items clsId
  methods <- methodList clsId items clsId
  return $ ClsDef clsId items ivars methods
  where
    instVarList :: TypeIdent -> [ClsItem] -> TypeIdent -> Chk [ClsVar]
    instVarList clsId items curId = do
      cls <- fromJust <$> lookupClassEntry curId
      let thisInstVars = map (\(id, typ) -> ClsVar curId typ id) (Map.toList $ instVars cls)
      case super cls of
        Just superId -> do
          superInstVars <- instVarList clsId items superId
          return $ mergeVars superInstVars thisInstVars
        Nothing -> return thisInstVars
    methodList :: TypeIdent -> [ClsItem] -> TypeIdent -> Chk [ClsMeth]
    methodList clsId items curId = do
      cls <- fromJust <$> lookupClassEntry curId
      let thisMeths = map (uncurry $ ClsMeth curId) (Map.toList $ meths cls)
      case super cls of
        Just superId -> do
          superMeths <- methodList clsId items superId
          return $ mergeMeths superMeths thisMeths
        Nothing -> return thisMeths
    mergeVars :: [ClsVar] -> [ClsVar] -> [ClsVar]
    mergeVars superVars thisVars = superVars ++ thisVars
    mergeMeths :: [ClsMeth] -> [ClsMeth] -> [ClsMeth]
    mergeMeths superMeths [] = superMeths
    mergeMeths [] thisMeths = thisMeths
    mergeMeths (superMeth@(ClsMeth super id typ) : superMeths) thisMeths =
      case find (\(ClsMeth _ other _) -> id == other) thisMeths of
        Just meth -> mergeMeths superMeths thisMeths
        Nothing -> superMeth : mergeMeths superMeths thisMeths

-- Specialise an 'J.Ident' into a variable identifier ('VarIdent').
toVarId :: J.Ident -> VarIdent
toVarId (J.Ident id) = VarId id

-- Specialise an 'J.Ident' into a function or method identifier ('FnIdent').
toFnId :: J.Ident -> FnIdent
toFnId (J.Ident id) = FnId id

-- Specialise an 'J.Ident' into a named type identifier ('TypeIdent').
toTypeId :: J.Ident -> TypeIdent
toTypeId (J.Ident id) = ClsId id

-- Convert a 'J.Type' into a 'Type'. Performs type specialisation on 'J.Named'
-- types.
castType :: J.Type -> Chk Type
castType J.Int = return Int
castType J.Double = return Double
castType J.Boolean = return Boolean
castType J.Void = return Void
castType (J.Array typ) = Array <$> castType typ
castType (J.Named id) = do
  let typeId = toTypeId id
  entry <- getDefKind typeId
  case entry of
    Just kind -> case kind of
      ClsKind -> return $ Object typeId
    Nothing -> throwError $ TypeNotFound typeId

-- | Convert a list of 'J.Param's into a list of 'Param's. Performs type
-- conversion in the process.
castParam :: J.Param -> Chk Param
castParam (J.Parameter typ id) = do
  casted <- castType typ
  return $ Parameter casted (toVarId id)

-- | Convert an 'J.AddOp' into an 'AddOp'. This is a trivial type conversion.
castAddOp :: J.AddOp -> AddOp
castAddOp J.Plus = Plus
castAddOp J.Minus = Minus

-- | Convert a 'J.MulOp' into a 'MulOp'. This is a trivial type conversion.
castMulOp :: J.MulOp -> MulOp
castMulOp J.Times = Times
castMulOp J.Div = Div
castMulOp J.Mod = Mod

-- | Convert a 'J.RelOp' into a 'RelOp'. This is a trivial type conversion.
castRelOp :: J.RelOp -> RelOp
castRelOp J.LTH = LTH
castRelOp J.LE = LE
castRelOp J.GTH = GTH
castRelOp J.GE = GE
castRelOp J.EQU = EQU
castRelOp J.NE = NE

-- * Context

-- | The local context.
data Context = Context
  { -- | The variable context stack. Each statement block introduces a new
    -- layer.
    vars :: ContextStack,
    -- | The function context stack. Contains globally defined functions, as
    -- well as methods of the current class hierarchy, if applicable.
    fns :: FnContextStack,
    -- | Save whether the current function had a @return@ on the currently
    -- traversed code path.
    retSt :: ReturnState,
    -- | The class definitions. Maps between class names and their context.
    clss :: Map TypeIdent ClassContext,
    -- | The current class name.
    curCls :: Maybe TypeIdent,
    -- | The kind ('DefKind') of globally defined types. Needed for type
    -- specialisation.
    defKinds :: DefKinds
  }
  deriving (Show)

-- | The context of a class. Contains a mapping between names and types of
-- instance variables and methods. Also contains name of superclass
data ClassContext = ClsContext
  { -- | The variable context stack. The bottom layer contains global
    -- declarations, including function definitions. Each statement block
    -- introduces a new layer.
    clsName :: TypeIdent,
    -- | Save whether the current function had a @return@ on the currently
    -- traversed code path.
    super :: Maybe TypeIdent,
    -- | The class definitions. Maps between class names and their context.
    meths :: Map FnIdent Type,
    -- | The current class name.
    instVars :: Map VarIdent Type
  }
  deriving (Show)

-- | The context stack. Consists of individual variable contexts.
type ContextStack = [Map VarIdent Type]

-- | The function (and method) context stack. Consists of individual function
-- contexts.
type FnContextStack = [Map FnIdent Type]

-- | A variable entry in the local context.
type VarEntry = (VarIdent, Type)

-- | A variable entry in the local context.
type FnEntry = (FnIdent, Type, [Type])

-- | The 'DefKind's of globally defined named types.
type DefKinds = Map TypeIdent DefKind

-- | An empty context.
emptyContext :: Context
emptyContext =
  Context
    { vars = [],
      fns = [Map.empty],
      retSt = NoReturn,
      clss = Map.empty,
      curCls = Nothing,
      defKinds = Map.empty
    }

-- | Context that contains the prelude functions of Javalette.
preludeContext :: Context
preludeContext =
  emptyContext
    { fns =
        [ Map.insert "printInt" (Fn Void [Int]) $
            Map.insert "printDouble" (Fn Void [Double]) $
              Map.insert "printString" (Fn Void [String]) $
                Map.insert "readInt" (Fn Int []) $
                  Map.insert "readDouble" (Fn Double []) Map.empty
        ]
    }

-- | Insert an entry into the top layer of the variable context stack.
insertVarEntry :: VarEntry -> Chk ()
insertVarEntry (id, typ) = modify (\ctx -> ctx {vars = Map.insert id typ (head $ vars ctx) : tail (vars ctx)})

-- | Look up whether an entry is present in the top layer of the variable
-- context stack. Returns 'Just' with the entry if found, otherwise 'Nothing'.
lookupVarEntry :: VarIdent -> Chk (Maybe Type)
lookupVarEntry id = gets $ \ctx -> Map.lookup id (head $ vars ctx)

-- | Look up whether an entry is present in the top layer of the variable
-- context stack.
memberVarEntry :: VarIdent -> Chk Bool
memberVarEntry id = gets $ \ctx -> Map.member id (head $ vars ctx)

-- | Insert an variable entry into the top of the context stack
extendContext :: VarEntry -> Chk ()
extendContext (id, typ) = do
  mem <- memberVarEntry id
  if not mem
    then insertVarEntry (id, typ)
    else throwError $ DuplicateVariable id

-- | Discard the top context of the variable context stack.
discardVarTop :: Chk ()
discardVarTop = modify (\ctx -> ctx {vars = tail $ vars ctx})

-- | Add an empty context on top of the variable context stack.
newVarTop :: Chk ()
newVarTop = modify (\ctx -> ctx {vars = Map.empty : vars ctx})

-- | Search the context stack for a variable and return its type.
-- Takes the first element that is discovered.
findVar :: VarIdent -> Chk Type
findVar id = do
  ctx <- get
  case stackLookup id (vars ctx) of
    Just typ -> return typ
    Nothing -> throwError $ UndeclaredVar id
  where
    stackLookup :: VarIdent -> ContextStack -> Maybe Type
    stackLookup id (top : rest) = case Map.lookup id top of
      Just typ -> Just typ
      Nothing -> stackLookup id rest
    stackLookup _id [] = Nothing

-- | Insert an entry into the top layer of the function context stack.
insertFnEntry :: FnEntry -> Chk ()
insertFnEntry (id, ret, args) = modify (\ctx -> ctx {fns = Map.insert id (Fn ret args) (head $ fns ctx) : tail (fns ctx)})

-- | Look up whether an entry is present in the top layer of the function
-- context stack. Returns 'Just' with the entry if found, otherwise 'Nothing'.
lookupFnEntry :: FnIdent -> Chk (Maybe Type)
lookupFnEntry id = gets $ \ctx -> Map.lookup id (head $ fns ctx)

-- | Look up whether an entry is present in the top layer of the function
-- context stack.
memberFnEntry :: FnIdent -> Chk Bool
memberFnEntry id = gets $ \ctx -> Map.member id (head $ fns ctx)

-- | Insert a function entry into the top of the context stack
extendFnContext :: FnEntry -> Chk ()
extendFnContext entry@(id, _, _) = do
  mem <- memberFnEntry id
  if not mem
    then insertFnEntry entry
    else throwError $ DuplicateFunction id

-- | Discard the top context of the function context stack.
discardFnTop :: Chk ()
discardFnTop = modify (\ctx -> ctx {fns = tail $ fns ctx})

-- | Add an empty context on top of the function context stack.
newFnTop :: Chk ()
newFnTop = modify (\ctx -> ctx {fns = Map.empty : fns ctx})

-- | Search the botton layer of the function context stack for a globally
-- defined function.
findFn :: FnIdent -> Chk Type
findFn id = do
  entry <- gets $ \ctx -> Map.lookup id (last $ fns ctx)
  case entry of
    Just typ -> return typ
    Nothing -> throwError $ FunctionNotFound id

-- | Push the class contexts of a class hierarchy on top of the variable context
-- stack.
pushClassTop :: TypeIdent -> Bool -> Chk ()
pushClassTop id isTop = do
  ctx <- get
  let cls = fromJust $ Map.lookup id (clss ctx)
  case super cls of
    Nothing -> do
      put $ ctx {fns = meths cls : fns ctx}
      when isTop $ put $ ctx {vars = instVars cls : vars ctx}
    Just super -> do
      pushClassTop super False
      when isTop $ put $ ctx {vars = instVars cls : vars ctx}

-- | Remove class members from the context stacks. Discards the variable context
-- stack and all but the bottom layer of the function context stack.
discardClassTop :: Chk ()
discardClassTop = do
  modify (\ctx -> ctx {fns = [last $ fns ctx]})
  modify (\ctx -> ctx {vars = []})

-- | Insert a class entry into the context.
insertClassEntry :: ClassContext -> Chk ()
insertClassEntry cls = modify (\ctx -> ctx {clss = Map.insert (clsName cls) cls (clss ctx)})

-- | Search for a class entry in the context. Returns 'Just' with the entry if
-- found or 'Nothing' otherwise.
lookupClassEntry :: TypeIdent -> Chk (Maybe ClassContext)
lookupClassEntry id = gets $ \ctx -> Map.lookup id (clss ctx)

-- | Search for a class entry in the context and return whether it is present.
memberClassEntry :: TypeIdent -> Chk Bool
memberClassEntry id = gets $ \ctx -> Map.member id (clss ctx)

-- | Set the current class name.
enterClass :: TypeIdent -> Chk ()
enterClass id = modify (\ctx -> ctx {curCls = Just id})

-- | Clear the current class name.
leaveClass :: Chk ()
leaveClass = modify (\ctx -> ctx {curCls = Nothing})

-- | Get the name of the class the current code is written in.
getCurrentClassName :: Chk (Maybe TypeIdent)
getCurrentClassName = gets curCls

-- | Get the name of the super class of a class. Returns a 'Just' with the name
-- of the super class if it has one or 'Nothing' otherwise. Returns a
-- 'TypeError' if the base class does not exist.
getSuperClassName :: TypeIdent -> Chk (Maybe TypeIdent)
getSuperClassName id = do
  ctx <- get
  case Map.lookup id (clss ctx) of
    Just cls -> return $ super cls
    Nothing -> throwError $ ClassNotFound id

-- | Search the registered classes for the class name. Returns the corresponding
-- 'Object' type if the class exists. Throws a 'TypeError' otherwise.
lookupClass :: TypeIdent -> Chk Type
lookupClass id = do
  ctx <- get
  if Map.member id (clss ctx)
    then return $ Object id
    else throwError $ ClassNotFound id

-- | Search for a method in a class and all its super classes. Returns the
-- corresponding function type if the method exists. Throws a 'TypeError'
-- otherwise.
lookupMethod :: FnIdent -> TypeIdent -> Chk Type
lookupMethod id clsId = do
  ctx <- get
  case Map.lookup clsId (clss ctx) of
    Just cls ->
      case Map.lookup id (meths cls) of
        Just typ -> return typ
        Nothing ->
          case super cls of
            Just superId -> lookupMethod id superId
            Nothing -> throwError $ MethodNotFound id
    Nothing -> throwError $ ClassNotFound clsId

-- | Set the kind of all top-level type definitions.
setDefKinds :: DefKinds -> Chk ()
setDefKinds defs = modify (\ctx -> ctx {defKinds = defs})

-- | Get the kind of a top-level type definition.
getDefKind :: TypeIdent -> Chk (Maybe DefKind)
getDefKind id = gets $ \ctx -> Map.lookup id (defKinds ctx)

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
getReturnState = gets retSt

-- | Set the current 'ReturnState' in the context. Uses '(<>)' to merge the
-- present return state with the new one.
setReturnState :: ReturnState -> Chk ()
setReturnState state = modify (\ctx -> ctx {retSt = retSt ctx <> state})

-- | Set the current 'ReturnState' in the context. Overrides the present return
-- state.
overrideReturnState :: ReturnState -> Chk ()
overrideReturnState state = modify (\ctx -> ctx {retSt = state})

-- | Reset the current 'ReturnState' in the context to 'NoReturn'.
resetReturnState :: Chk ()
resetReturnState = modify (\ctx -> ctx {retSt = NoReturn})
