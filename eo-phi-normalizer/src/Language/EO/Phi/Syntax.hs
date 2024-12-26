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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.EO.Phi.Syntax (
  module Language.EO.Phi.Syntax.Abs,
  desugar,
  printTree,
  shrinkDots,

  -- * Conversion to 'Bytes'
  intToBytes,
  int64ToBytes,
  int32ToBytes,
  int16ToBytes,
  floatToBytes,
  boolToBytes,
  stringToBytes,

  -- * Conversion from 'Bytes'
  bytesToInt,
  bytesToInt64,
  bytesToInt32,
  bytesToInt16,
  bytesToFloat,
  bytesToString,
  bytesToBool,

  -- * Wrapping 'Bytes' into 'Object'
  wrapBytesInConstInt,
  wrapBytesInConstInt64,
  wrapBytesInConstInt32,
  wrapBytesInConstInt16,
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
  errorExpectedDesugaredObject,
  errorExpectedDesugaredBinding,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString.Strict
import Data.Char (isSpace, toUpper)
import Data.Int
import Data.List (intercalate)
import Data.Serialize qualified as Serialize
import Data.String (IsString (fromString))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Float (isDoubleFinite)
import Language.EO.Phi.Preprocess (preprocess)
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

errorExpectedDesugaredObject :: Object -> a
errorExpectedDesugaredObject x = error ("impossible: expected desugared Object, but got: " <> printTree x)

errorExpectedDesugaredBinding :: Binding -> a
errorExpectedDesugaredBinding x = error ("impossible: expected desugared Binding, but got: " <> printTree x)

class DesugarableInitially a where
  desugarInitially :: a -> a

instance DesugarableInitially Object where
  desugarInitially :: Object -> Object
  desugarInitially = \case
    obj@(ConstString{}) -> obj
    obj@(ConstInt{}) -> obj
    ConstIntRaw (IntegerSigned x) -> ConstInt (read x)
    obj@(ConstFloat{}) -> obj
    ConstFloatRaw (DoubleSigned x) -> ConstFloat (read x)
    Formation bindings -> Formation (desugarInitially bindings)
    Application obj bindings -> Application (desugarInitially obj) (desugarInitially bindings)
    ObjectDispatch obj a -> ObjectDispatch (desugarInitially obj) a
    GlobalObject -> GlobalObject
    GlobalObjectPhiOrg -> "Φ.org.eolang"
    ThisObject -> ThisObject
    Termination -> Termination
    MetaSubstThis obj this -> MetaSubstThis (desugarInitially obj) (desugarInitially this)
    obj@MetaObject{} -> obj
    MetaContextualize obj1 obj2 -> MetaContextualize (desugarInitially obj1) (desugarInitially obj2)
    MetaTailContext obj metaId -> MetaTailContext (desugarInitially obj) metaId
    MetaFunction name obj -> MetaFunction name (desugarInitially obj)

instance DesugarableInitially [Binding] where
  desugarInitially :: [Binding] -> [Binding]
  desugarInitially = zipWith go [0 ..]
   where
    go :: Int -> Binding -> Binding
    go idx = \case
      AlphaBinding (AttrSugar l ls) (Formation bindings) ->
        let bindingsDesugared = desugarInitially bindings
         in AlphaBinding (Label l) (Formation ((EmptyBinding . Label <$> ls) <> bindingsDesugared))
      AlphaBinding a obj -> AlphaBinding a (desugarInitially obj)
      AlphaBindingSugar obj -> AlphaBinding (Alpha (AlphaIndex [fmt|α{idx}|])) (desugarInitially obj)
      binding -> binding

instance DesugarableInitially Program where
  desugarInitially :: Program -> Program
  desugarInitially (Program bindings) = Program (desugarInitially bindings)

instance DesugarableInitially Binding where
  desugarInitially = \case
    obj@AlphaBindingSugar{} -> errorExpectedDesugaredBinding obj
    AlphaBinding a obj -> AlphaBinding a (desugarInitially obj)
    obj -> obj

instance DesugarableInitially Attribute where desugarInitially = id
instance DesugarableInitially RuleAttribute where desugarInitially = id
instance DesugarableInitially PeeledObject where desugarInitially = id
instance DesugarableInitially ObjectHead where desugarInitially = id
instance DesugarableInitially MetaId where desugarInitially = id

class SugarableFinally a where
  sugarFinally :: a -> a

instance SugarableFinally Object where
  sugarFinally :: Object -> Object
  sugarFinally = \case
    "Φ.org.eolang" -> GlobalObjectPhiOrg
    obj@ConstString{} -> obj
    obj@ConstInt{} -> obj
    obj@ConstIntRaw{} -> errorExpectedDesugaredObject obj
    obj@ConstFloat{} -> obj
    obj@ConstFloatRaw{} -> errorExpectedDesugaredObject obj
    Formation bindings -> Formation (sugarFinally <$> bindings)
    Application obj bindings -> Application (sugarFinally obj) (sugarFinally bindings)
    ObjectDispatch obj a -> ObjectDispatch (sugarFinally obj) a
    GlobalObject -> GlobalObject
    obj@GlobalObjectPhiOrg -> errorExpectedDesugaredObject obj
    ThisObject -> ThisObject
    Termination -> Termination
    MetaSubstThis obj this -> MetaSubstThis (sugarFinally obj) (sugarFinally this)
    obj@MetaObject{} -> obj
    MetaContextualize obj1 obj2 -> MetaContextualize (sugarFinally obj1) (sugarFinally obj2)
    MetaTailContext obj metaId -> MetaTailContext (sugarFinally obj) metaId
    MetaFunction name obj -> MetaFunction name (sugarFinally obj)

instance SugarableFinally [Binding] where
  sugarFinally :: [Binding] -> [Binding]
  sugarFinally bs =
    if and (zipWith go [0 ..] bs)
      then (\(~(AlphaBinding _ obj)) -> AlphaBindingSugar (sugarFinally obj)) <$> bs
      else sugarFinally <$> bs
   where
    go :: Int -> Binding -> Bool
    go idx = \case
      obj@AlphaBindingSugar{} -> errorExpectedDesugaredBinding obj
      obj@(AlphaBinding (AttrSugar _ _) _) -> errorExpectedDesugaredBinding obj
      AlphaBinding (Alpha (AlphaIndex ('α' : idx'))) _ -> idx == read idx'
      _ -> False

instance SugarableFinally Binding where
  sugarFinally :: Binding -> Binding
  sugarFinally = \case
    obj@AlphaBindingSugar{} -> errorExpectedDesugaredBinding obj
    AlphaBinding a obj -> AlphaBinding a (sugarFinally obj)
    x -> x

instance {-# OVERLAPPABLE #-} SugarableFinally a where
  sugarFinally :: a -> a
  sugarFinally = id

desugar :: Object -> Object
desugar = \case
  ConstString string -> wrapBytesInString (stringToBytes string)
  ConstInt n -> wrapBytesInInt (intToBytes (fromInteger n))
  obj@ConstIntRaw{} -> errorExpectedDesugaredObject obj
  ConstFloat x -> wrapBytesInFloat (floatToBytes x)
  obj@ConstFloatRaw{} -> errorExpectedDesugaredObject obj
  Formation bindings -> Formation (desugarBinding <$> bindings)
  Application obj bindings -> Application (desugar obj) (desugarBinding <$> bindings)
  ObjectDispatch obj a -> ObjectDispatch (desugar obj) a
  GlobalObject -> GlobalObject
  obj@GlobalObjectPhiOrg -> errorExpectedDesugaredObject obj
  ThisObject -> ThisObject
  Termination -> Termination
  MetaSubstThis obj this -> MetaSubstThis (desugar obj) (desugar this)
  obj@MetaObject{} -> obj
  MetaContextualize obj1 obj2 -> MetaContextualize (desugar obj1) (desugar obj2)
  MetaTailContext obj metaId -> MetaTailContext (desugar obj) metaId
  MetaFunction name obj -> MetaFunction name (desugar obj)

desugarBinding :: Binding -> Binding
desugarBinding = \case
  AlphaBinding (AttrSugar l ls) (Formation bindings) ->
    let bindingsDesugared = desugarBinding <$> bindings
     in AlphaBinding (Label l) (Formation ((EmptyBinding . Label <$> ls) <> bindingsDesugared))
  AlphaBinding a obj -> AlphaBinding a (desugar obj)
  obj@(AlphaBindingSugar{}) -> errorExpectedDesugaredBinding obj
  binding -> binding

-- MetaSubstThis

wrapBytesInInt :: Bytes -> Object
wrapBytesInInt (Bytes bytes) = [fmt|Φ.org.eolang.i64(as-bytes ↦ Φ.org.eolang.bytes(Δ ⤍ {bytes}))|]
wrapBytesInFloat :: Bytes -> Object
wrapBytesInFloat (Bytes bytes) = [fmt|Φ.org.eolang.number(as-bytes ↦ Φ.org.eolang.bytes(Δ ⤍ {bytes}))|]
wrapBytesInString :: Bytes -> Object
wrapBytesInString (Bytes bytes) = [fmt|Φ.org.eolang.string(as-bytes ↦ Φ.org.eolang.bytes(Δ ⤍ {bytes}))|]
wrapBytesInBytes :: Bytes -> Object
wrapBytesInBytes (Bytes bytes) = [fmt|Φ.org.eolang.bytes(Δ ⤍ {bytes})|]
wrapTermination :: Object
wrapTermination = [fmt|Φ.org.eolang.error(α0 ↦ Φ.org.eolang.string(as-bytes ↦ Φ.org.eolang.bytes(Δ ⤍ {bytes})))|]
 where
  Bytes bytes = stringToBytes "unknown error"

wrapBytesInConstInt :: Bytes -> Object
wrapBytesInConstInt = wrapBytesInConstInt64

wrapBytesInConstInt64 :: Bytes -> Object
wrapBytesInConstInt64 bytes@(Bytes bs)
  | n < 0 = [fmt|Φ.org.eolang.i64(as-bytes ↦ Φ.org.eolang.bytes(Δ ⤍ {bs}))|]
  | otherwise = [fmt|Φ.org.eolang.i64(as-bytes ↦ {n})|]
 where
  n = bytesToInt bytes

wrapBytesInConstInt32 :: Bytes -> Object
wrapBytesInConstInt32 bytes@(Bytes bs)
  | n < 0 = [fmt|Φ.org.eolang.i32(as-bytes ↦ Φ.org.eolang.bytes(Δ ⤍ {bs}))|]
  | otherwise = [fmt|Φ.org.eolang.i32(as-bytes ↦ {n})|]
 where
  n = bytesToInt bytes

wrapBytesInConstInt16 :: Bytes -> Object
wrapBytesInConstInt16 bytes@(Bytes bs)
  | n < 0 = [fmt|Φ.org.eolang.i16(as-bytes ↦ Φ.org.eolang.bytes(Δ ⤍ {bs}))|]
  | otherwise = [fmt|Φ.org.eolang.i16(as-bytes ↦ {n})|]
 where
  n = bytesToInt bytes

wrapBytesInConstFloat :: Bytes -> Object
wrapBytesInConstFloat bytes@(Bytes bs)
  | x == 0 = [fmt|Φ.org.eolang.number(as-bytes ↦ 0.0)|]
  | x > 0 && isDoubleFinite x == 1 = [fmt|Φ.org.eolang.number(as-bytes ↦ {printf "%f" x :: String})|]
  | otherwise = [fmt|Φ.org.eolang.number(as-bytes ↦ Φ.org.eolang.bytes(Δ ⤍ {bs}))|]
 where
  x = bytesToFloat bytes

wrapBytesInConstString :: Bytes -> Object
wrapBytesInConstString bytes@(Bytes bs)
  | '\\' `elem` s = [fmt|Φ.org.eolang.string(as-bytes ↦ Φ.org.eolang.bytes(Δ ⤍ {bs}))|]
  | otherwise = [fmt|Φ.org.eolang.string(as-bytes ↦ {s})|]
 where
  s = show (bytesToString bytes)

wrapBytesAsBool :: Bytes -> Object
wrapBytesAsBool bytes
  | bytesToInt bytes == 0 = [fmt|Φ.org.eolang.false|]
  | otherwise = [fmt|Φ.org.eolang.true|]

-- * Overriding generated pretty-printer

-- | Like 'Phi.printTree', but without spaces around dots and no indentation for curly braces.
printTree :: (Phi.Print a, SugarableFinally a) => a -> String
printTree = shrinkDots . render . Phi.prt 0 . sugarFinally

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

-- | Convert an 'Int64' into 'Bytes' representation.
--
-- >>> int64ToBytes 7
-- Bytes "00-00-00-00-00-00-00-07"
-- >>> int64ToBytes (3^33)
-- Bytes "00-13-BF-EF-A6-5A-BB-83"
-- >>> int64ToBytes (-1)
-- Bytes "FF-FF-FF-FF-FF-FF-FF-FF"
int64ToBytes :: Int64 -> Bytes
int64ToBytes n = Bytes $ normalizeBytes $ foldMap (padLeft 2 . (`showHex` "")) $ ByteString.Strict.unpack $ Serialize.encode n

-- | Convert an 'Int32' into 'Bytes' representation.
--
-- >>> int32ToBytes 7
-- Bytes "00-00-00-07"
-- >>> int32ToBytes (3^33)
-- Bytes "A6-5A-BB-83"
-- >>> int32ToBytes (-1)
-- Bytes "FF-FF-FF-FF"
int32ToBytes :: Int32 -> Bytes
int32ToBytes n = Bytes $ normalizeBytes $ foldMap (padLeft 2 . (`showHex` "")) $ ByteString.Strict.unpack $ Serialize.encode n

-- | Convert an 'Int16' into 'Bytes' representation.
--
-- >>> int16ToBytes 7
-- Bytes "00-07"
-- >>> int16ToBytes (3^33)
-- Bytes "BB-83"
-- >>> int16ToBytes (-1)
-- Bytes "FF-FF"
int16ToBytes :: Int16 -> Bytes
int16ToBytes n = Bytes $ normalizeBytes $ foldMap (padLeft 2 . (`showHex` "")) $ ByteString.Strict.unpack $ Serialize.encode n

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

-- | Parse 'Bytes' as 'Int64'.
--
-- >>> bytesToInt64 "00-13-BF-EF-A6-5A-BB-83"
-- 5559060566555523
-- >>> bytesToInt64 "AB-"
-- 171
--
-- May error on invalid 'Bytes':
--
-- >>> bytesToInt64 "s"
-- *** Exception: Prelude.head: empty list
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...
bytesToInt64 :: Bytes -> Int64
bytesToInt64 (Bytes (dropWhile (== '0') . filter (/= '-') -> bytes))
  | null bytes = 0
  | otherwise = fst $ head $ readHex bytes

-- | Parse 'Bytes' as 'Int32'.
--
-- >>> bytesToInt32 "A6-5A-BB-83"
-- -1504003197
-- >>> bytesToInt32 "AB-"
-- 171
--
-- May error on invalid 'Bytes':
--
-- >>> bytesToInt32 "s"
-- *** Exception: Prelude.head: empty list
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...
bytesToInt32 :: Bytes -> Int32
bytesToInt32 (Bytes (dropWhile (== '0') . filter (/= '-') -> bytes))
  | null bytes = 0
  | otherwise = fst $ head $ readHex bytes

-- | Parse 'Bytes' as 'Int16'.
--
-- >>> bytesToInt16 "BB-83"
-- -17533
-- >>> bytesToInt16 "AB-"
-- 171
--
-- May error on invalid 'Bytes':
--
-- >>> bytesToInt16 "s"
-- *** Exception: Prelude.head: empty list
-- ...
-- ...
-- ...
-- ...
-- ...
-- ...
bytesToInt16 :: Bytes -> Int16
bytesToInt16 (Bytes (dropWhile (== '0') . filter (/= '-') -> bytes))
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

-- >>> "{⟦ org ↦ ⟦ eolang ↦ ⟦ number(as-bytes) ↦ ⟦ φ ↦ ξ.as-bytes, neg ↦ ξ.times(-1), ⟧, λ ⤍ Package ⟧, λ ⤍ Package ⟧ ⟧}" :: Program
-- syntax error at line 1, column 76 due to lexer error
-- on the input:
-- {⟦ org ↦ ⟦ eolang ↦ ⟦ ~number(as-bytes) ↦ ⟦ φ ↦ ξ.as-bytes, neg ↦ ξ.times(-1), ⟧, λ ⤍ Package ⟧, λ ⤍ Package ⟧ ⟧}

-- >>> "-1.0" :: Object
-- ConstFloatRaw (DoubleSigned "-1.0")

instance IsString Program where fromString = unsafeParseWith pProgram
instance IsString Object where fromString = unsafeParseWith pObject
instance IsString Binding where fromString = unsafeParseWith pBinding
instance IsString Attribute where fromString = unsafeParseWith pAttribute
instance IsString RuleAttribute where fromString = unsafeParseWith pRuleAttribute
instance IsString PeeledObject where fromString = unsafeParseWith pPeeledObject
instance IsString ObjectHead where fromString = unsafeParseWith pObjectHead
instance IsString MetaId where fromString = unsafeParseWith pMetaId

parseWith :: (DesugarableInitially a) => ([Token] -> Either String a) -> String -> Either String a
parseWith parser input = either (\x -> Left [fmt|{x}\non the input:\n{input'}|]) (Right . desugarInitially) parsed
 where
  input' = preprocess input
  tokens = myLexer input'
  parsed = parser tokens

-- | Parse a 'Object' from a 'String'.
-- May throw an 'error` if input has a syntactical or lexical errors.
unsafeParseWith :: (DesugarableInitially a) => ([Token] -> Either String a) -> String -> a
unsafeParseWith parser input =
  case parseWith parser input of
    Left parseError -> error parseError
    Right object -> object
