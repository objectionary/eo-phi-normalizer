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
{-# LANGUAGE LambdaCase #-}

module Language.EO.Phi.Preprocess where

import Control.Monad (void)
import Data.Void (Void)
import Language.EO.Phi.Syntax.Abs
import Replace.Megaparsec (splitCap)
import Text.Megaparsec (MonadParsec (..), Parsec, Stream (..), between, choice, match, oneOf, optional, sepBy)
import Text.Megaparsec.Byte.Lexer qualified as L
import Text.Megaparsec.Char (space, string)

symbol :: String -> Parser String
symbol = L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

type Parser = Parsec Void String

parseTail :: Parser String
parseTail = takeWhileP (Just "LabelId") (`notElem` " \r\n\t,.|':;!?][}{)(⟧⟦↦")

parseLabelId :: Parser LabelId
parseLabelId = lexeme do
  l <- oneOf ['a' .. 'z']
  ls <- parseTail
  pure $ LabelId (l : ls)

parseToken :: String -> (String -> a) -> Parser a
parseToken prefix cons = lexeme do
  void $ string prefix
  ls <- parseTail
  pure $ cons (prefix <> ls)

parseObjectMetaId :: Parser ObjectMetaId
parseObjectMetaId = parseToken "!b" ObjectMetaId

parseBytesMetaId :: Parser BytesMetaId
parseBytesMetaId = parseToken "!y" BytesMetaId

parseLabelMetaId :: Parser LabelMetaId
parseLabelMetaId = parseToken "!τ" LabelMetaId

parseMetaId :: Parser MetaId
parseMetaId =
  choice
    [ MetaIdObject <$> parseObjectMetaId
    , MetaIdBytes <$> parseBytesMetaId
    , MetaIdLabel <$> parseLabelMetaId
    ]

parseAlphaIndex :: Parser AlphaIndex
parseAlphaIndex = parseToken "α" AlphaIndex

parseAttribute :: Parser Attribute
parseAttribute = lexeme do
  choice
    [ Phi <$ symbol "φ"
    , Rho <$ symbol "ρ"
    , Label <$> parseLabelId
    , Alpha <$> parseAlphaIndex
    ]

parseBindingArrow :: Parser ()
parseBindingArrow = void $ symbol "↦"

parseAttributeSugar :: Parser AttributeSugar
parseAttributeSugar = do
  choice
    [ do
        labelId <- parseLabelId
        attrs <- optional $ between (symbol "(") (symbol ")") (sepBy parseAttribute (symbol ","))
        case attrs of
          Nothing -> pure $ AttributeNoSugar (Label labelId)
          Just attrs' -> pure $ AttributeSugar labelId attrs'
    , AttributeNoSugar <$> parseAttribute
    ]

type Attr = Either MetaId AttributeSugar

parseAlphaBindingSugar :: Parser Attr
parseAlphaBindingSugar = do
  attr <-
    choice
      [ Left <$> parseMetaId
      , Right <$> parseAttributeSugar
      ]
  parseBindingArrow
  notFollowedBy (symbol "∅")
  pure attr

splitInput :: Parser a -> String -> [Either String (Tokens [Char], a)]
splitInput sep = splitCap (match sep)

addPrefix :: Parser Attr -> String -> [String]
addPrefix sep = fmap (either id (\(x, a) -> choosePrefix a <> x)) . splitInput sep
 where
  choosePrefix = \case
    Right AttributeSugar{} -> "~"
    _ -> "#"

preprocess' :: Parser Attr -> String -> String
preprocess' sep = concat . addPrefix sep

preprocess :: String -> String
preprocess = preprocess' parseAlphaBindingSugar

input1 :: String
input1 = "{⟦ org ↦ ⟦ ⟧(α0 ↦ !b1) ⟧}"

-- >>> preprocess input1
-- "{\10214 #org \8614 \10214 \10215(#\945\&0 \8614 !b1) \10215}"
