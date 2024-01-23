{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.EO.Phi (
  defaultMain,
  normalize,
  parseProgram,
  unsafeParseProgram,
  module Language.EO.Phi.Syntax,
) where

import System.Exit (exitFailure)

import qualified Language.EO.Phi.Syntax.Abs as Phi
import qualified Language.EO.Phi.Syntax.Par as Phi

import Language.EO.Phi.Normalize
import Language.EO.Phi.Syntax

-- | Parse a 'Program' or return a parsing error.
parseProgram :: String -> Either String Phi.Program
parseProgram input = Phi.pProgram tokens
 where
  tokens = Phi.myLexer input

-- | Parse a 'Program' from a 'String'.
-- May throw an 'error` if input has a syntactical or lexical errors.
unsafeParseProgram :: String -> Phi.Program
unsafeParseProgram input =
  case parseProgram input of
    Left parseError -> error parseError
    Right program -> program

-- | Default entry point.
-- Parses a 𝜑-program from standard input, normalizes,
-- then pretty-prints the result to standard output.
defaultMain :: IO ()
defaultMain = do
  input <- getContents -- read entire standard input
  let tokens = Phi.myLexer input
  case Phi.pProgram tokens of
    Left parseError -> do
      putStrLn parseError
      exitFailure
    Right program -> do
      putStrLn (printTree (normalize program))
