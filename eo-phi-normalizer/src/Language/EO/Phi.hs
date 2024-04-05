module Language.EO.Phi (
  defaultMain,
  normalize,
  parseProgram,
  unsafeParseObject,
  unsafeParseProgram,
  module Language.EO.Phi.Syntax,
) where

import System.Exit (exitFailure)

import Language.EO.Phi.Syntax.Abs qualified as Phi
import Language.EO.Phi.Syntax.Par qualified as Phi

import Language.EO.Phi.Normalize
import Language.EO.Phi.Syntax

-- | Parse a 'Program' or return a parsing error.
parseProgram :: String -> Either String Phi.Program
parseProgram input = Phi.pProgram tokens
 where
  tokens = Phi.myLexer input

-- | Parse an 'Object' or return a parsing error.
parseObject :: String -> Either String Phi.Object
parseObject = Phi.pObject . Phi.myLexer

-- | Parse a 'Program' from a 'String'.
-- May throw an 'error` if input has a syntactical or lexical errors.
unsafeParseProgram :: String -> Phi.Program
unsafeParseProgram input =
  case parseProgram input of
    Left parseError -> error parseError
    Right program -> program

-- | Parse an 'Object' from a 'String'.
-- May throw an 'error` if input has a syntactical or lexical errors.
unsafeParseObject :: String -> Phi.Object
unsafeParseObject = either error id . parseObject

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
