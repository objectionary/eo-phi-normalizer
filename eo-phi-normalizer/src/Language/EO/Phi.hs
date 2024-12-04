{- FOURMOLU_DISABLE -}
-- The MIT License (MIT)

-- Copyright (c) 2016-2024 Objectionary.com

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
{- FOURMOLU_ENABLE -}
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
import Language.EO.Phi.Rules.Common (parseWith)
import Language.EO.Phi.Syntax

-- | Parse a 'Program' or return a parsing error.
parseProgram :: String -> Either String Phi.Program
parseProgram = parseWith Phi.pProgram

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
