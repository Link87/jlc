{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_HADDOCK prune, ignore-exports, show-extensions #-}

-- | Main module. Either loads a file from disk or reads input from @stdin@.
module Main where

import Control.Monad (void, when)
import Control.Monad.Reader
  ( MonadIO (..),
    MonadReader (ask),
    ReaderT (..),
    asks,
    void,
    when,
  )
import qualified Data.ByteString as S
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Javalette.CLI (Flag (..), parseOpts)
import Javalette.Check.TypeCheck (check)
import Javalette.Gen.LLVM (generateIR)
import Javalette.Lang.Par (myLexer, pProg)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (Handle, stderr, stdout)

-- | Main function. Read input and compile it.
main :: IO ()
main = do
  args <- getArgs
  (flags, file) <- parseOpts args
  case file of
    Just path -> S.readFile path >>= runOutput flags . compile . TE.decodeUtf8
    Nothing -> TIO.getContents >>= runOutput flags . compile

-- | Monad for handling compilation process. Flags are stored in 'ReaderT' and
-- output is done via 'IO'.
newtype Output a = MkOutput (ReaderT [Flag] IO a)
  deriving (Functor, Applicative, Monad, MonadReader [Flag], MonadIO)

-- | Run the 'Output' monad with the given flags.
runOutput :: [Flag] -> Output () -> IO ()
runOutput flags (MkOutput out) = void $ runReaderT out flags

-- | Parse, type check, and compile a program given by the @String@.
compile :: Text -> Output ()
compile s = do
  standalone <- asks $ elem Standalone
  parser <- asks $ elem ParserRepr
  typecheck <- asks $ elem TypeCheck
  intermediate <- asks $ elem IntermediateRepr
  llvm <- llvmFlagSet
  when standalone $ outputString "Parsing..."
  case pProg (myLexer s) of
    Left err -> do
      if standalone
        then do
          outputErr "Syntax error:"
          outputErr $ T.pack err
          outputErr "Compilation failed!"
        else do
          outputErr "ERROR"
          outputErr $ T.pack err
      liftIO exitFailure
    Right tree -> do
      when parser $ outputResult tree >> liftIO exitSuccess
      when standalone $ outputString "Checking..."
      case check tree of
        Left err -> do
          if standalone
            then do
              outputErr "Type mismatch:"
              outputErr $ T.pack $ show err
              outputErr "Compilation failed!"
            else do
              outputErr "ERROR"
              outputErr $ T.pack $ show err
          liftIO exitFailure
        Right annotated -> do
          if standalone
            then outputString "Type check successful."
            else outputErr "OK"
          when typecheck $ liftIO exitSuccess
          when intermediate $ outputResult annotated >> liftIO exitSuccess
          when llvm $ do
            when standalone $ outputString "Compiling..."
            outputTextResult (generateIR annotated)

-- | Print text to @stdout@.
outputString :: Text -> Output ()
outputString = liftIO . TIO.hPutStrLn stdout

-- | Print text to @stderr@.
outputErr :: Text -> Output ()
outputErr = liftIO . TIO.hPutStrLn stderr

-- | Write output to the output destination specified in the flags. Use this for
-- a compilation result.
outputResult :: Show a => a -> Output ()
outputResult out = do
  flags <- ask
  case find (== OutputFile "") flags of
    Nothing -> outputString (T.pack $ show out)
    Just (OutputFile file) -> do
      liftIO $ writeFile file (show out)
      outputString $ "Output successfully written to " <> T.pack file

-- | Write output to the output destination specified in the flags. Use this for
-- an already 'Text'-based compilation result.
outputTextResult :: Text -> Output ()
outputTextResult out = do
  flags <- ask
  case find (== OutputFile "") flags of
    Nothing -> outputString out
    Just (OutputFile file) -> do
      liftIO $ TIO.writeFile file out
      outputString $ "Output successfully written to " <> T.pack file

-- | Read flags and return whether the llvm flag is set. This is the
-- default value, i.e., also @True@ if no flag is set.
llvmFlagSet :: Output Bool
llvmFlagSet = do
  flags <- ask
  return $
    elem LLVM flags
      || all (\x -> x /= IntermediateRepr && x /= TypeCheck && x /= LLVM && x /= X86) flags
