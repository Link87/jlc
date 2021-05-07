{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK prune, ignore-exports, show-extensions #-}

module Javalette.Check.Return
  ( ReturnState (NoReturn, Return),
    both,
  )
where

-- | Used to indicate whether a function returns after a certain statement.
-- Mimics a `Bool`.
data ReturnState = NoReturn | Return
  deriving (Show, Eq)

-- | A function returns, if just one statement is a return statement (Except
-- for control flow statements). Therefore, think of it as the `(||)` operation.
instance Semigroup ReturnState where
  Return <> _ = Return
  _ <> Return = Return
  _ <> _ = NoReturn

instance Monoid ReturnState where
  mempty = NoReturn
  mappend = (<>)

-- | Helper function. Return `Return` of both inputs are `Return`, else
-- `NoReturn`.
both :: ReturnState -> ReturnState -> ReturnState
both Return Return = Return
both _ _ = NoReturn
