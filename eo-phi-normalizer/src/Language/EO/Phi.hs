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
  normalize,
  parseProgram,
  unsafeParseObject,
  unsafeParseProgram,
  unsafeParseProgramFromFile,
  module Language.EO.Phi.Syntax,
) where

import Language.EO.Phi.Normalize
import Language.EO.Phi.Syntax
import Language.EO.Phi.Syntax.Abs qualified as Phi
import Language.EO.Phi.Syntax.Par qualified as Phi

-- | Parse a 'Program' or return a parsing error.
parseProgram :: String -> Either String Phi.Program
parseProgram = parseWith Phi.pProgram

-- | Parse an 'Object' or return a parsing error.
parseObject :: String -> Either String Phi.Object
parseObject = parseWith Phi.pObject

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

unsafeParseProgramFromFile :: FilePath -> IO Phi.Program
unsafeParseProgramFromFile inputFile = do
  src <- readFile inputFile
  case parseProgram src of
    Left err -> error ("Error parsing program from '" ++ inputFile ++ "':\n" ++ err)
    Right program -> pure program
