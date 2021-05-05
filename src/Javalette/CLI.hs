{-# LANGUAGE Safe #-}

module Javalette.CLI
  ( Flag (..),
    parseOpts,
  )
where

import Data.Maybe ( fromMaybe )
import System.Console.GetOpt
    ( getOpt,
      usageInfo,
      ArgDescr(NoArg, OptArg),
      ArgOrder(RequireOrder),
      OptDescr(..) )

-- | Possible compiler flags.
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

-- | Option definition for the command line.
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
      "Abort compilation after typechecking and output the IR. Not compatible with '-t', '-l' or '-x'.",
    Option
      ['t']
      ["typecheck"]
      (NoArg TypeCheck)
      "Only run type checker. Not compatible with '-i', '-l' or '-x'.",
    Option
      ['l']
      ["llvm"]
      (NoArg LLVM)
      "Compile to LLVM byte code. Not compatible with '-i', '-t' or '-x'. (default)",
    Option
      ['x']
      ["x86"]
      (NoArg X86)
      "Compile to x86 assembler. Not compatible with '-i', '-l' or '-t'. (unsupported atm)"
  ]
  where
    outp :: Maybe String -> Flag
    outp = OutputFile . fromMaybe "stdout"

-- | Parse the argument list into a list of flags and a 'Maybe' with a file.
-- Throws 'IO' errors if parsing fails.
parseOpts :: [String] -> IO ([Flag], Maybe String)
parseOpts args =
  case getOpt RequireOrder options args of
    (flags, files, []) -> do
      if length (filter (\x -> x == IntermediateRepr || x == TypeCheck || x == LLVM || x == X86) flags) > 1
        then printErrors ["Conflicting arguments. Can only use one of '-i', '-t', '-l' or '-x'."]
        else
          if X86 `elem` flags
            then printErrors ["x86 is currently unsupported."]
            else case files of
              [] -> return (flags, Nothing)
              [file] -> return (flags, Just file)
              _ -> printErrors ["Either supply exactly one file name or pipe file into stdin."]
    (_, _, errs) -> printErrors errs
  where
    printErrors errs = ioError (userError (concat errs ++ usageInfo header options))
    header = "Usage: jlc [<options>...] [<file>]"
