{-# LANGUAGE PatternSynonyms #-}

import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO.Error    (isUserError, ioeGetErrorString)

import ParJavalette       (pProg, myLexer)
import PrintJavalette       (printTree)
import ErrM               (pattern Ok, pattern Bad)

import TypeChecker        (typecheck)

-- | Parse, type check, and interpret a program given by the @String@.

check :: String -> IO ()
check s = do
  case pProg (myLexer s) of
    Bad err  -> do
      putStrLn "Syntax error:"
      putStrLn err
      putStrLn "Compilation failed!"
      exitFailure
    Ok tree -> do
      putStrLn $ printTree tree
      case typecheck tree of
        Left err -> do
          putStrLn "Type mismatch:"
          print err
          putStrLn "Compilation failed!"
          exitFailure
        Right annotated -> do
          putStrLn "Type check successful."

-- | Main: read file passed by only command line argument and call 'check'.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= check
    _      -> do
      putStrLn "Usage: jlc <source_file>"
      exitFailure
