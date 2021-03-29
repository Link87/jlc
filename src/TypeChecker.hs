{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module TypeChecker
  ( TypeError(..),
    typecheck,
  )
where

import AbsJavalette
import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import PrintJavalette (printTree)

pattern Ok a = Right a

pattern Err msg = Left msg

type AnnotatedProg = Prog

data TypeError
  = MainArguments
  | MainReturnType
  | MainNotFound
  | NoFunctionFound
  | DuplicateFunction
  | UndeclaredVar Ident

instance Show TypeError where
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

newtype Chk a = MkChk (ReaderT ContextStack (ExceptT TypeError Identity) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader ContextStack,
      MonadError TypeError
    )

-- | An empty environment.
emptyContext :: Context
emptyContext = Map.empty

-- | Insert an element into the top of the stack
extend :: ContextEntry -> ContextStack -> ContextStack
extend (id, typ) (map : ctx) = Map.insert id typ map : ctx

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
runChk ctx (MkChk rd) = runIdentity $ runExceptT $ runReaderT rd [ctx]

-- | Save the top-level functins in the context.
saveFns :: Prog -> TypeResult Context
saveFns (Program []) = Err NoFunctionFound
saveFns (Program [fn]) = do
  (id, typ) <- saveSignature fn
  return $ Map.insert id typ emptyContext
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
  checked <- checkFn fn
  (Program other) <- checkTypes (Program rest)
  return $ Program $ checked : other

checkFn :: TopDef -> Chk TopDef
checkFn = return

lookupVar :: Ident -> Chk Type
lookupVar id = do
  stack <- ask
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

checkStmt :: Context -> Stmt -> TypeResult Type
checkStmt = undefined

checkExpr :: Expr -> Type -> Chk AnnotatedProg
checkExpr = undefined

inferExp :: Expr -> Chk Type
inferExp (EVar id) = do lookupVar id
inferExp (ELitInt int) = return Int
inferExp (ELitDoub doub) = return Doub
inferExp ELitTrue = return Bool
inferExp ELitFalse = return Bool
inferExp (EApp id exprs) = lookupVar id -- TODO exprs unchecked?
inferExp (Neg expr) = undefined
