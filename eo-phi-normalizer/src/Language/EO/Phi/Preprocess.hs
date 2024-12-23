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
