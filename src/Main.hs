{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Debug.Trace
import Javalette.ErrM (pattern Bad, pattern Ok)
import Javalette.Par (myLexer, pProg)
import Javalette.Print (printTree)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import TypeChecker (typecheck)

newtype Output a = MkOutput (ReaderT [Flag] IO a)
  deriving (Functor, Applicative, Monad, MonadReader [Flag], MonadIO)

runOutput :: [Flag] -> Output () -> IO ()
runOutput flags (MkOutput out) = runReaderT out flags >> return ()

-- | Possible compiler flags
data Flag = Standalone | OutputFile String | IntermediateRepr | TypeCheck | LLVM | X86
  deriving (Show)

-- | Two options are treated as equal independent from their arguments.
instance Eq Flag where
  Standalone == Standalone = True
  OutputFile _ == OutputFile _ = True
  IntermediateRepr == IntermediateRepr = True
  TypeCheck == TypeCheck = True
  LLVM == LLVM = True
  X86 == X86 = True
  _ == _ = False

-- | Write output to the specified output destination.
output :: Show a => a -> Output ()
output out = do
  flags <- ask
  case find (== OutputFile "") flags of
    Nothing -> do
      liftIO $ putStrLn "Generated IR:"
      liftIO $ print out
    Just (OutputFile file) -> liftIO $ writeFile file (show out)

-- | Parse, type check, and interpret a program given by the @String@.
check :: String -> Output ()
check s = do
  standalone <- isStandalone
  intermediate <- intermediateFlagSet
  case pProg (myLexer s) of
    Bad err -> do
      if standalone
        then do
          liftIO $ hPutStrLn stderr "Syntax error:"
          liftIO $ hPutStrLn stderr err
          liftIO $ hPutStrLn stderr "Compilation failed!"
        else do
          liftIO $ hPutStrLn stderr "ERROR"
          liftIO $ hPutStrLn stderr err
      liftIO exitFailure
    Ok tree -> do
      case typecheck tree of
        Left err -> do
          if standalone
            then do
              liftIO $ hPutStrLn stderr "Type mismatch:"
              liftIO $ hPrint stderr err
              liftIO $ hPutStrLn stderr "Compilation failed!"
            else do
              liftIO $ hPutStrLn stderr "ERROR"
              liftIO $ hPrint stderr err
          liftIO exitFailure
        Right annotated -> do
          if standalone
            then do
              liftIO $ putStrLn "Type check successful."
              when intermediate $ output annotated
            else do
              when intermediate $ output annotated
              liftIO $ hPutStrLn stderr "OK"

-- | Main: read file passed by only command line argument and call 'check'.
main :: IO ()
main = do
  args <- getArgs
  (flags, file) <- compilerOpts args
  case file of
    Just path -> readFile path >>= runOutput flags . check
    Nothing -> getContents >>= runOutput flags . check

options :: [OptDescr Flag]
options =
  [ Option
      ['s']
      ["standalone"]
      (NoArg Standalone)
      "Print more useful output made for humans. Not for piping into LLVM or automated testing.",
    Option
      ['o']
      ["output"]
      (OptArg outp "FILE")
      "Redirect output to this file.",
    Option
      ['i']
      ["intermediate"]
      (NoArg IntermediateRepr)
      "Abort compilation after typechecking and output the IR. Not compatible with '-t', '-l' or '-x'. (default)",
    Option
      ['t']
      ["typecheck"]
      (NoArg TypeCheck)
      "Only run type checker. Not compatible with '-i', '-l' or '-x'.",
    Option
      ['l']
      ["llvm"]
      (NoArg LLVM)
      "Compile to LLVM byte code. Not compatible with '-i', '-t' or '-x'. (unsupported atm)",
    Option
      ['x']
      ["x86"]
      (NoArg X86)
      "Compile to x86 assembler. Not compatible with '-i', '-l' or '-t'. (unsupported atm)"
  ]
  where
    outp :: Maybe String -> Flag
    outp = OutputFile . fromMaybe "stdout"

compilerOpts :: [String] -> IO ([Flag], Maybe String)
compilerOpts args =
  case getOpt RequireOrder options args of
    (flags, files, []) -> do
      if length (filter (\x -> x == IntermediateRepr || x == TypeCheck || x == LLVM || x == X86) flags) > 1
        then printErrors ["Conflicting arguments. Can only use one of '-i', '-t', '-l' or '-x'."]
        else
          if elem LLVM flags || elem X86 flags
            then printErrors ["LLVM and x86 are currently unsupported."]
            else case files of
              [] -> return (flags, Nothing)
              [file] -> return (flags, Just file)
              _ -> printErrors ["Either supply exactly one file name or pipe file into stdin."]
    (_, _, errs) -> printErrors errs
  where
    printErrors errs = ioError (userError (concat errs ++ usageInfo header options))
    header = "Usage: jlc [<options>...] [<file>]"

isStandalone :: Output Bool
isStandalone = asks $ elem Standalone

intermediateFlagSet :: Output Bool
intermediateFlagSet = do
  flags <- ask
  return $
    elem IntermediateRepr flags
      || all (\x -> x /= IntermediateRepr && x /= TypeCheck && x /= LLVM && x /= X86) flags
