module TypeChecker where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import AbsJavalette
import PrintJavalette (printTree)

type AnnotatedProgram = Prog -- TODO: choose your flavor of typed ASTs
type TypeError = String

typecheck :: Prog -> Either TypeError AnnotatedProgram
typecheck  = return
