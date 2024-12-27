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
{-# LANGUAGE BlockArguments #-}

module Language.EO.Phi.Preprocess where

import Control.Monad (void)
import Data.Void (Void)
import Replace.Megaparsec (splitCap)
import Text.Megaparsec (MonadParsec (..), Parsec, Stream (..), between, match, sepBy)
import Text.Megaparsec.Byte.Lexer qualified as L
import Text.Megaparsec.Char (lowerChar, space)

symbol :: String -> Parser String
symbol = L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

type Parser = Parsec Void String

parseLabelId :: Parser ()
parseLabelId = lexeme do
  void lowerChar
  void $ takeWhileP (Just "LabelId") (`notElem` " \r\n\t,.|':;!?][}{)(⟧⟦")

parseBindingArrow :: Parser ()
parseBindingArrow = void $ symbol "↦"

parseAlphaAttr :: Parser ()
parseAlphaAttr = do
  void parseLabelId
  void $ between (symbol "(") (symbol ")") (sepBy parseLabelId (symbol ","))

parseAlphaBindingSugar :: Parser ()
parseAlphaBindingSugar = do
  parseAlphaAttr
  parseBindingArrow

splitInput :: Parser a -> String -> [Either String (Tokens [Char])]
splitInput sep = splitCap (fst <$> match sep)

addPrefix :: Parser a -> String -> [String]
addPrefix sep = map (either id ("~" <>)) . splitInput sep

preprocess' :: Parser a -> String -> String
preprocess' sep = concat . addPrefix sep

preprocess :: String -> String
preprocess = preprocess' parseAlphaBindingSugar

input1 :: String
input1 = "{⟦ org ↦ ⟦ eolang ↦ ⟦ number( as-bytes, abra  ) ↦ ⟦ φ ↦ ξ.as-bytes, neg ↦ ξ.times(-1), ⟧, λ ⤍ Package ⟧, λ ⤍ Package ⟧ ⟧}"

-- >>> addPrefix parseAlphaBindingSugar input1
-- ["{\10214 org \8614 \10214 eolang \8614 \10214 ","~number( as-bytes, abra  ) \8614 ","\10214 \966 \8614 \958.as-bytes, neg \8614 \958.times(-1), \10215, \955 \10509 Package \10215, \955 \10509 Package \10215 \10215}"]

-- >>> preprocess input1
-- "{\10214 org \8614 \10214 eolang \8614 \10214 ~number( as-bytes, abra  ) \8614 \10214 \966 \8614 \958.as-bytes, neg \8614 \958.times(-1), \10215, \955 \10509 Package \10215, \955 \10509 Package \10215 \10215}"
