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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.EO.Phi.Syntax (
  module Language.EO.Phi.Syntax.Abs,
  desugar,
  printTree,
  shrinkDots,

  -- * Conversion to 'Bytes'
  intToBytes,
  floatToBytes,
  boolToBytes,
  stringToBytes,

  -- * Conversion from 'Bytes'
  bytesToInt,
  bytesToFloat,
  bytesToString,
  bytesToBool,

  -- * Wrapping 'Bytes' into 'Object'
  wrapBytesInConstInt,
  wrapBytesInConstFloat,
  wrapBytesInConstString,
  wrapBytesInBytes,
  wrapBytesInInt,
  wrapBytesInFloat,
  wrapBytesAsBool,
  wrapBytesInString,
  wrapTermination,

  -- * Functions over 'Bytes'
  sliceBytes,
  concatBytes,

  -- * Helpers
  chunksOf,
  paddedLeftChunksOf,
  normalizeBytes,
  parseWith,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString.Strict
import Data.Char (isSpace, toUpper)
import Data.List (intercalate)
import Data.Serialize qualified as Serialize
import Data.String (IsString (fromString))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Language.EO.Phi.Syntax.Abs
import Language.EO.Phi.Syntax.Lex (Token)
import Language.EO.Phi.Syntax.Par
import Language.EO.Phi.Syntax.Print qualified as Phi
import Numeric (readHex, showHex)
import PyF (fmt)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists

desugar :: Object -> Object
desugar = \case
  ConstString string -> wrapBytesInString (stringToBytes string)
  ConstInt n -> wrapBytesInInt (intToBytes (fromInteger n))
  ConstFloat x -> wrapBytesInFloat (floatToBytes x)
  Formation bindings -> Formation (map desugarBinding bindings)
  Application obj bindings -> Application (desugar obj) (map desugarBinding bindings)
  ObjectDispatch obj a -> ObjectDispatch (desugar obj) a
  GlobalObject -> GlobalObject
  ThisObject -> ThisObject
  Termination -> Termination
  MetaSubstThis obj this -> MetaSubstThis (desugar obj) (desugar this)
  obj@MetaObject{} -> obj
  MetaContextualize obj1 obj2 -> MetaContextualize (desugar obj1) (desugar obj2)
  MetaTailContext obj metaId -> MetaTailContext (desugar obj) metaId
  MetaFunction name obj -> MetaFunction name (desugar obj)

desugarBinding :: Binding -> Binding
desugarBinding = \case
  AlphaBinding a obj -> AlphaBinding a (desugar obj)
  binding -> binding

-- MetaSubstThis

wrapBytesInInt :: Bytes -> Object
wrapBytesInInt (Bytes bytes) = [fmt|Φ.org.eolang.int(as-bytes ↦ Φ.org.eolang.bytes(Δ ⤍ {bytes}))|]
wrapBytesInFloat :: Bytes -> Object
wrapBytesInFloat (Bytes bytes) = [fmt|Φ.org.eolang.float(as-bytes ↦ Φ.org.eolang.bytes(Δ ⤍ {bytes}))|]
wrapBytesInString :: Bytes -> Object
wrapBytesInString (Bytes bytes) = [fmt|Φ.org.eolang.string(as-bytes ↦ Φ.org.eolang.bytes(Δ ⤍ {bytes}))|]
wrapBytesInBytes :: Bytes -> Object
wrapBytesInBytes (Bytes bytes) = [fmt|Φ.org.eolang.bytes(Δ ⤍ {bytes})|]
wrapTermination :: Object
wrapTermination = [fmt|Φ.org.eolang.error(α0 ↦ Φ.org.eolang.string(as-bytes ↦ Φ.org.eolang.bytes(Δ ⤍ {bytes})))|]
 where
  Bytes bytes = stringToBytes "unknown error"

wrapBytesInConstInt :: Bytes -> Object
wrapBytesInConstInt bytes@(Bytes bs)
  | n < 0 = [fmt|Φ.org.eolang.int(as-bytes ↦ Φ.org.eolang.bytes(Δ ⤍ {bs}))|]
  | otherwise = [fmt|Φ.org.eolang.int(as-bytes ↦ {n})|]
 where
  n = bytesToInt bytes

wrapBytesInConstFloat :: Bytes -> Object
wrapBytesInConstFloat bytes@(Bytes bs)
  | x == 0 = [fmt|Φ.org.eolang.float(as-bytes ↦ 0.0)|]
  | x < 0 = [fmt|Φ.org.eolang.float(as-bytes ↦ Φ.org.eolang.bytes(Δ ⤍ {bs}))|]
  | otherwise = [fmt|Φ.org.eolang.float(as-bytes ↦ {printf "%f" x :: String})|]
 where
  x = bytesToFloat bytes

wrapBytesInConstString :: Bytes -> Object
wrapBytesInConstString bytes = [fmt|Φ.org.eolang.string(as-bytes ↦ {show (bytesToString bytes)})|]

wrapBytesAsBool :: Bytes -> Object
wrapBytesAsBool bytes
  | bytesToInt bytes == 0 = [fmt|Φ.org.eolang.false|]
  | otherwise = [fmt|Φ.org.eolang.true|]

-- * Overriding generated pretty-printer

-- | Like 'Phi.printTree', but without spaces around dots and no indentation for curly braces.
printTree :: (Phi.Print a) => a -> String
printTree = shrinkDots . render . Phi.prt 0

-- | Remove spaces around dots.
--
-- >>> shrinkDots "a ↦ ξ . a" == "a ↦ ξ.a"
-- True
shrinkDots :: String -> String
shrinkDots [] = []
shrinkDots (' ' : '.' : ' ' : cs) = '.' : shrinkDots cs
shrinkDots (c : cs) = c : shrinkDots cs

readFloat :: String -> Maybe Double
readFloat s | '.' `elem` s = readMaybe s
readFloat _ = Nothing

fixFloat :: String -> String
fixFloat s =
  case readFloat s of
    Just x -> printf "%f" x
    Nothing -> s

-- | Copy of 'Phi.render', except no indentation is made for curly braces.
render :: Phi.Doc -> String
render d = rend 0 False (map (fixFloat . ($ "")) $ d []) ""
 where
  rend ::
    Int ->
    -- \^ Indentation level.
    Bool ->
    -- \^ Pending indentation to be output before next character?
    [String] ->
    ShowS
  rend i p = \case
    "[" : "]" : ts -> showString "[]" . rend i False ts
    "(" : ")" : (t : ts) -> handleTrailingComma "()" t ts
    "⟦" : "⟧" : (t : ts) -> handleTrailingComma "⟦⟧" t ts
    "[" : ts -> char '[' . rend i False ts
    "(" : ts -> char '(' . new (i + 1) ts
    "{" : "⟦" : ts -> showChar '{' . onNewLine (i + 1) p . showChar '⟦' . new (i + 2) ts
    "⟦" : ts -> showChar '⟦' . new (i + 1) ts
    ")" : "," : ts -> onNewLine (i - 1) p . showString ")," . new (i - 1) ts
    "⟧" : "," : ts -> onNewLine (i - 1) p . showString "⟧," . new (i - 1) ts
    ["⟧", "}"] -> onNewLine (i - 1) p . showChar '⟧' . new (i - 2) ["}"]
    "⟧" : ts -> onNewLine (i - 1) p . showChar '⟧' . new (i - 1) ts
    ")" : ts -> onNewLine (i - 1) p . showChar ')' . new (i - 1) ts
    [";"] -> char ';'
    ";" : ts -> char ';' . new i ts
    "." : ts -> rend i p (" ." : ts)
    t : (s : ss) | closingOrPunctuation s -> handleTrailingComma t s ss
    t : ts -> pending . space t . rend i False ts
    [] -> id
   where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    handleTrailingComma str t ts =
      (pending . showString str)
        . ( case t of
              "," -> showChar ',' . new i ts
              _ -> rend i False (t : ts)
          )

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = Phi.replicateS (2 * i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t, null spc, null rest) of
      (True, _, True) -> [] -- remove trailing space
      (False, _, True) -> t -- remove trailing space
      (False, True, False) -> t ++ ' ' : s -- add space if none
      _ -> t ++ s
   where
    (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _ = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

padLeft :: Int -> [Char] -> [Char]
padLeft n s = replicate (n - length s) '0' ++ s

-- | Split a list into chunks of given size.
-- All lists in the result are guaranteed to have length less than or equal to the given size.
--
-- >>> chunksOf 2 "012345678"
-- ["01","23","45","67","8"]
--
-- See 'paddedLeftChunksOf' for a version with padding to guarantee exact chunk size.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = chunk : chunksOf n leftover
 where
  (chunk, leftover) = splitAt n xs

-- | Split a list into chunks of given size,
-- padding on the left if necessary.
-- All lists in the result are guaranteed to have given size.
--
-- >>> paddedLeftChunksOf '0' 2 "1234567"
-- ["01","23","45","67"]
-- >>> paddedLeftChunksOf '0' 2 "123456"
-- ["12","34","56"]
--
-- prop> n > 0  ==>  all (\chunk -> length chunk == n) (paddedLeftChunksOf c n s)
paddedLeftChunksOf :: a -> Int -> [a] -> [[a]]
paddedLeftChunksOf padSymbol n xs
  | padSize == n = chunksOf n xs
  | otherwise = chunksOf n (replicate padSize padSymbol ++ xs)
 where
  len = length xs
  padSize = n - len `mod` n

-- | Normalize the bytestring representation to fit valid 'Bytes' token.
--
-- >>> normalizeBytes "238714ABCDEF"
-- "23-87-14-AB-CD-EF"
--
-- >>> normalizeBytes "0238714ABCDEF"
-- "00-23-87-14-AB-CD-EF"
--
-- >>> normalizeBytes "4"
-- "04-"
normalizeBytes :: String -> String
normalizeBytes = withDashes . paddedLeftChunksOf '0' 2 . map toUpper
 where
  withDashes = \case
    [] -> "00-"
    [byte] -> byte <> "-"
    bytes -> intercalate "-" bytes

-- | Concatenate 'Bytes'.
-- FIXME: we should really use 'ByteString' instead of the underlying 'String' representation.
--
-- >>> concatBytes "00-" "01-02"
-- Bytes "00-01-02"
--
-- >>> concatBytes "03-04" "01-02"
-- Bytes "03-04-01-02"
--
-- >>> concatBytes "03-04" "01-"
-- Bytes "03-04-01"
concatBytes :: Bytes -> Bytes -> Bytes
concatBytes (Bytes xs) (Bytes zs) = Bytes (normalizeBytes (filter (/= '-') (xs <> zs)))

-- | Select a slice (section) of 'Bytes'.
--
-- >>> sliceBytes "12-34-56" 1 1
-- Bytes "34-"
--
-- >>> sliceBytes "12-34-56" 1 0
-- Bytes "00-"
--
-- >>> sliceBytes "12-34-56" 0 2
-- Bytes "12-34"
sliceBytes :: Bytes -> Int -> Int -> Bytes
sliceBytes (Bytes bytes) start len = Bytes $ normalizeBytes $ take (2 * len) (drop (2 * start) (filter (/= '-') bytes))

-- | Convert an 'Int' into 'Bytes' representation.
--
-- >>> intToBytes 7
-- Bytes "00-00-00-00-00-00-00-07"
-- >>> intToBytes (3^33)
-- Bytes "00-13-BF-EF-A6-5A-BB-83"
-- >>> intToBytes (-1)
-- Bytes "FF-FF-FF-FF-FF-FF-FF-FF"
intToBytes :: Int -> Bytes
intToBytes n = Bytes $ normalizeBytes $ foldMap (padLeft 2 . (`showHex` "")) $ ByteString.Strict.unpack $ Serialize.encode n

-- | Parse 'Bytes' as 'Int'.
--
-- >>> bytesToInt "00-13-BF-EF-A6-5A-BB-83"
-- 5559060566555523
-- >>> bytesToInt "AB-"
-- 171
--
-- May error on invalid 'Bytes':
--
-- >>> bytesToInt "s"
-- *** Exception: Prelude.head: empty list
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...
bytesToInt :: Bytes -> Int
bytesToInt (Bytes (dropWhile (== '0') . filter (/= '-') -> bytes))
  | null bytes = 0
  | otherwise = fst $ head $ readHex bytes

-- | Convert 'Bool' to 'Bytes'.
--
-- >>> boolToBytes False
-- Bytes "00-"
-- >>> boolToBytes True
-- Bytes "01-"
boolToBytes :: Bool -> Bytes
boolToBytes True = Bytes "01-"
boolToBytes False = Bytes "00-"

-- | Interpret 'Bytes' as 'Bool'.
--
-- Zero is interpreted as 'False'.
--
-- >>> bytesToBool "00-"
-- False
--
-- >>> bytesToBool "00-00"
-- False
--
-- Everything else is interpreted as 'True'.
--
-- >>> bytesToBool "01-"
-- True
--
-- >>> bytesToBool "00-01"
-- True
--
-- >>> bytesToBool "AB-CD"
-- True
bytesToBool :: Bytes -> Bool
bytesToBool (Bytes (dropWhile (== '0') . filter (/= '-') -> [])) = False
bytesToBool _ = True

-- | Encode 'String' as 'Bytes'.
--
-- >>> stringToBytes "Hello, world!"
-- Bytes "48-65-6C-6C-6F-2C-20-77-6F-72-6C-64-21"
--
-- >>> stringToBytes "Привет, мир!"
-- Bytes "D0-9F-D1-80-D0-B8-D0-B2-D0-B5-D1-82-2C-20-D0-BC-D0-B8-D1-80-21"
--
-- >>> stringToBytes  "hello, 大家!"
-- Bytes "68-65-6C-6C-6F-2C-20-E5-A4-A7-E5-AE-B6-21"
stringToBytes :: String -> Bytes
stringToBytes s = bytestringToBytes $ Text.encodeUtf8 (Text.pack s)

bytestringToBytes :: ByteString -> Bytes
bytestringToBytes = Bytes . normalizeBytes . foldMap (padLeft 2 . (`showHex` "")) . ByteString.Strict.unpack

bytesToByteString :: Bytes -> ByteString
bytesToByteString (Bytes bytes) = ByteString.Strict.pack (go (filter (/= '-') bytes))
 where
  go [] = []
  go (x : y : xs) = fst (head (readHex [x, y])) : go xs
  go [_] = error "impossible: partial byte"

-- | Decode 'String' from 'Bytes'.
--
-- >>> bytesToString "48-65-6C-6C-6F-2C-20-77-6F-72-6C-64-21"
-- "Hello, world!"
bytesToString :: Bytes -> String
bytesToString = Text.unpack . Text.decodeUtf8 . bytesToByteString

-- | Encode 'Double' as 'Bytes' following IEEE754.
--
-- Note: it is called "float" in EO, but it actually occupies 8 bytes so it corresponds to 'Double'.
--
-- >>> floatToBytes 0
-- Bytes "00-00-00-00-00-00-00-00"
--
-- >>> floatToBytes (-0.1)
-- Bytes "BF-B9-99-99-99-99-99-9A"
--
-- >>> floatToBytes (1/0)       -- Infinity
-- Bytes "7F-F0-00-00-00-00-00-00"
--
-- >>> floatToBytes (asin 2) `elem` ["FF-F8-00-00-00-00-00-00", "7F-F8-00-00-00-00-00-00"]  -- sNaN or qNaN
-- True
floatToBytes :: Double -> Bytes
floatToBytes f = Bytes $ normalizeBytes $ foldMap (padLeft 2 . (`showHex` "")) $ ByteString.Strict.unpack $ Serialize.encode f

-- | Decode 'Double' from 'Bytes' following IEEE754.
--
-- >>> bytesToFloat "00-00-00-00-00-00-00-00"
-- 0.0
--
-- >>> bytesToFloat "BF-B9-99-99-99-99-99-9A"
-- -0.1
--
-- >>> bytesToFloat "7F-F0-00-00-00-00-00-00"
-- Infinity
--
-- >>> bytesToFloat "FF-F8-00-00-00-00-00-00"
-- NaN
bytesToFloat :: Bytes -> Double
bytesToFloat (Bytes bytes) =
  case Serialize.decode $ ByteString.Strict.pack $ map (fst . head . readHex) $ words (map dashToSpace bytes) of
    Left msg -> error msg
    Right x -> x
 where
  dashToSpace '-' = ' '
  dashToSpace c = c

instance IsString Program where fromString = unsafeParseWith pProgram
instance IsString Object where fromString = unsafeParseWith pObject
instance IsString Binding where fromString = unsafeParseWith pBinding
instance IsString Attribute where fromString = unsafeParseWith pAttribute
instance IsString RuleAttribute where fromString = unsafeParseWith pRuleAttribute
instance IsString PeeledObject where fromString = unsafeParseWith pPeeledObject
instance IsString ObjectHead where fromString = unsafeParseWith pObjectHead

instance IsString MetaId where fromString = unsafeParseWith pMetaId

parseWith :: ([Token] -> Either String a) -> String -> Either String a
parseWith parser input = either (\x -> Left [fmt|{x}\non the input:\n{input}|]) Right parsed
 where
  tokens = myLexer input
  parsed = parser tokens

-- | Parse a 'Object' from a 'String'.
-- May throw an 'error` if input has a syntactical or lexical errors.
unsafeParseWith :: ([Token] -> Either String a) -> String -> a
unsafeParseWith parser input =
  case parseWith parser input of
    Left parseError -> error parseError
    Right object -> object
