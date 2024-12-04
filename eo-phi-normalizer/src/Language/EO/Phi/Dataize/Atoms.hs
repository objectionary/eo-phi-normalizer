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

module Language.EO.Phi.Dataize.Atoms where

import Data.Bits
import Data.List (singleton)
import Language.EO.Phi.Dataize
import Language.EO.Phi.Rules.Common
import Language.EO.Phi.Syntax.Abs

knownAtomsList :: [(String, String -> Object -> EvaluationState -> DataizeChain (Object, EvaluationState))]
knownAtomsList =
  [ ("Lorg_eolang_int_gt", evaluateIntIntBoolFunChain (>))
  , ("Lorg_eolang_int_plus", evaluateIntIntIntFunChain (+))
  , ("Lorg_eolang_int_times", evaluateIntIntIntFunChain (*))
  , ("Lorg_eolang_int_div", evaluateIntIntIntFunChain quot)
  , ("Lorg_eolang_bytes_eq", evaluateBinaryDataizationFunChain boolToBytes bytesToInt wrapBytesAsBool extractRho (extractLabel "b") (==))
  ,
    ( "Lorg_eolang_bytes_size"
    , let f = evaluateUnaryDataizationFunChain intToBytes id wrapBytesInBytes extractRho (\(Bytes bytes) -> length (words (map dashToSpace bytes)))
           where
            dashToSpace '-' = ' '
            dashToSpace c = c
       in f
    )
  ,
    ( "Lorg_eolang_bytes_slice"
    , \name obj state -> do
        thisStr <- incLogLevel $ dataizeRecursivelyChain True (extractRho obj)
        bytes <- case thisStr of
          Right bytes -> pure bytes
          Left _ -> fail "Couldn't find bytes"
        evaluateBinaryDataizationFunChain id bytesToInt wrapBytesInBytes (extractLabel "start") (extractLabel "len") (sliceBytes bytes) name obj state
    )
  , ("Lorg_eolang_bytes_and", evaluateBytesBytesBytesFunChain (.&.))
  , ("Lorg_eolang_bytes_or", evaluateBytesBytesBytesFunChain (.|.))
  , ("Lorg_eolang_bytes_xor", evaluateBytesBytesBytesFunChain (.^.))
  , ("Lorg_eolang_bytes_not", evaluateBytesBytesFunChain complement)
  , ("Lorg_eolang_bytes_right", evaluateBinaryDataizationFunChain intToBytes bytesToInt wrapBytesInBytes extractRho (extractLabel "x") (\x i -> shift x (-i)))
  , ("Lorg_eolang_bytes_concat", evaluateBinaryDataizationFunChain id id wrapBytesInBytes extractRho (extractLabel "b") concatBytes)
  , -- float
    ("Lorg_eolang_float_gt", evaluateBinaryDataizationFunChain boolToBytes bytesToFloat wrapBytesInBytes extractRho (extractLabel "x") (>))
  , ("Lorg_eolang_float_times", evaluateFloatFloatFloatFunChain (*))
  , ("Lorg_eolang_float_plus", evaluateFloatFloatFloatFunChain (+))
  , ("Lorg_eolang_float_div", evaluateFloatFloatFloatFunChain (/))
  , ("Lorg_eolang_float_gt", evaluateBinaryDataizationFunChain boolToBytes bytesToFloat wrapBytesInBytes extractRho (extractLabel "x") (>))
  , ("Lorg_eolang_float_times", evaluateFloatFloatFloatFunChain (*))
  , ("Lorg_eolang_float_plus", evaluateFloatFloatFloatFunChain (+))
  , ("Lorg_eolang_float_div", evaluateFloatFloatFloatFunChain (/))
  , -- string
    ("Lorg_eolang_string_length", evaluateUnaryDataizationFunChain intToBytes bytesToString wrapBytesInInt extractRho length)
  ,
    ( "Lorg_eolang_string_slice"
    , \name obj state -> do
        thisStr <- incLogLevel $ dataizeRecursivelyChain True (extractRho obj)
        string <- case thisStr of
          Right bytes -> pure $ bytesToString bytes
          Left _ -> fail "Couldn't find bytes"
        evaluateBinaryDataizationFunChain stringToBytes bytesToInt wrapBytesInString (extractLabel "start") (extractLabel "len") (\start len -> take len (drop start string)) name obj state
    )
  , -- others
    ("Lorg_eolang_dataized", evaluateUnaryDataizationFunChain id id wrapBytesInBytes (extractLabel "target") id)
  , ("Lorg_eolang_error", evaluateUnaryDataizationFunChain stringToBytes bytesToString wrapBytesInBytes (extractLabel "message") error)
  ,
    ( "Package"
    , let
        f _name obj@(Formation bindings) = do
          \state ->
            getContext
              >>= ( \case
                      True -> do
                        let (packageBindings, restBindings) = span isPackage bindings
                        bs <- mapM dataizeBindingChain restBindings
                        logStep "Dataized 'Package' siblings" (Left $ Formation (bs ++ packageBindings))
                        return (Formation (bs ++ packageBindings), state)
                      False ->
                        return (Formation bindings, state)
                  )
                . dataizePackage
         where
          isPackage (LambdaBinding (Function "Package")) = True
          isPackage _ = False
          dataizeBindingChain (AlphaBinding attr o) = do
            ctx <- getContext
            let extendedContext = (extendContextWith obj ctx){currentAttr = attr}
            dataizationResult <- incLogLevel $ withContext extendedContext $ dataizeRecursivelyChain False o
            return (AlphaBinding attr (either id (Formation . singleton . DeltaBinding) dataizationResult))
          dataizeBindingChain b = return b
        f name _otherwise = evaluateBuiltinFunChainUnknown name _otherwise
       in
        f
    )
  ]
