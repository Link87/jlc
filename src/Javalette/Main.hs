{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Control.Monad ( when, void )
import Control.Monad.Reader
    ( when, void, MonadIO(..), ReaderT(..), asks, MonadReader(ask) )
import Data.List ( find )
import qualified Data.ByteString as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Javalette.CLI ( Flag(..), parseOpts )
import Javalette.Check.TypeCheck (check)
import Javalette.Lang.ErrM (pattern Bad, pattern Ok)
import Javalette.Lang.Par (myLexer, pProg)
import Javalette.Lang.Print (printTree)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO ( Handle, stderr, stdout )

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
  standalone <- isStandalone
  intermediate <- intermediateFlagSet
  case pProg (myLexer s) of
    Bad err -> do
      if standalone
        then do
          outputErr "Syntax error:"
          outputErr $ T.pack err
          outputErr "Compilation failed!"
        else do
          outputErr "ERROR"
          outputErr $ T.pack err
      liftIO exitFailure
    Ok tree -> do
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
            then do
              outputString "Type check successful."
              when intermediate $ outputResult annotated
            else do
              outputErr "OK"
              when intermediate $ outputResult annotated

-- | Print text to @stdout@.
outputString :: Text -> Output ()
outputString = liftIO . TIO.hPutStrLn stdout

-- | Print text to @stderr@.
outputErr :: Text -> Output ()
outputErr = liftIO . TIO.hPutStrLn stderr

-- | Write output to the output destination specified in the flags. Use this for
-- compilation result.
outputResult :: Show a => a -> Output ()
outputResult out = do
  flags <- ask
  case find (== OutputFile "") flags of
    Nothing -> do
      outputString "Generated IR:"
      outputString (T.pack $ show out)
    Just (OutputFile file) -> liftIO $ writeFile file (show out)

-- | Read flags and return whether the standalone flag is set.
isStandalone :: Output Bool
isStandalone = asks $ elem Standalone

-- | Read flags and return whether the intermediate flag is set. Currently,
-- this is the default value, i.e., also @True@ if no flag is set.
intermediateFlagSet :: Output Bool
intermediateFlagSet = do
  flags <- ask
  return $
    elem IntermediateRepr flags
      || all (\x -> x /= IntermediateRepr && x /= TypeCheck && x /= LLVM && x /= X86) flags
