{-# LANGUAGE PatternSynonyms #-}

module Main where

import Debug.Trace
import Javalette.ErrM (pattern Bad, pattern Ok)
import Javalette.Par (myLexer, pProg)
import Javalette.Print (printTree)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import TypeChecker (typecheck)

-- | Parse, type check, and interpret a program given by the @String@.
check :: String -> IO ()
check s = do
  case pProg (myLexer s) of
    Bad err -> do
      putStrLn "Syntax error:"
      putStrLn err
      putStrLn "Compilation failed!"
      hPutStrLn stderr "ERROR"
      exitFailure
    Ok tree -> do
      print tree
      case typecheck tree of
        Left err -> do
          putStrLn "Type mismatch:"
          print err
          putStrLn "Compilation failed!"
          hPutStrLn stderr "ERROR"
          exitFailure
        Right annotated -> do
          putStrLn "Type check successful."
          hPutStrLn stderr "OK"

-- | Main: read file passed by only command line argument and call 'check'.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-f", file] -> readFile file >>= check
    [] -> readFileStdin >>= check
    _ -> do
      putStrLn "Usage: 'jlc -f <source_file>' or suppy input with stdin"
      -- hPutStrLn stderr $ "INPUT ERROR: " ++ show args
      exitFailure

-- | Read a file from stdin
readFileStdin :: IO String
readFileStdin = do
  done <- isEOF
  if not done
    then do
      inp <- getLine
      next <- readFileStdin
      return $ inp ++ next
    else return ""
