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
import Language.EO.Phi.Syntax

knownAtomsList :: [(String, String -> Object -> EvaluationState -> DataizeChain (Object, EvaluationState))]
knownAtomsList =
  -- [ ("Lorg_eolang_as_phi", _)
  [ ("Lorg_eolang_int_gt", evaluateIntIntBoolFunChain (>))
  , ("Lorg_eolang_int_plus", evaluateIntIntIntFunChain (+))
  , ("Lorg_eolang_int_times", evaluateIntIntIntFunChain (*))
  , ("Lorg_eolang_int_div", evaluateIntIntIntFunChain quot)
  , ("Lorg_eolang_bytes_and", evaluateBytesBytesBytesFunChain (.&.))
  , ("Lorg_eolang_bytes_concat", evaluateBinaryDataizationFunChain id id wrapBytesInBytes extractRho (extractLabel "b") concatBytes)
  , ("Lorg_eolang_bytes_eq", evaluateBinaryDataizationFunChain boolToBytes bytesToInt wrapBytesAsBool extractRho (extractLabel "b") (==))
  , ("Lorg_eolang_bytes_not", evaluateBytesBytesFunChain complement)
  , ("Lorg_eolang_bytes_or", evaluateBytesBytesBytesFunChain (.|.))
  , ("Lorg_eolang_bytes_right", evaluateBinaryDataizationFunChain intToBytes bytesToInt wrapBytesInBytes extractRho (extractLabel "x") (\x i -> shift x (-i)))
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
          AsBytes bytes -> pure bytes
          AsObject _ -> fail "Couldn't find bytes"
        evaluateBinaryDataizationFunChain id bytesToInt wrapBytesInBytes (extractLabel "start") (extractLabel "len") (sliceBytes bytes) name obj state
    )
  , ("Lorg_eolang_bytes_xor", evaluateBytesBytesBytesFunChain (.^.))
  , -- deprecated
    ("Lorg_eolang_dataized", evaluateUnaryDataizationFunChain id id wrapBytesInBytes (extractLabel "target") id)
  , -- , ("Lorg_eolang_cage_encaged_encage", _)
    -- , ("Lorg_eolang_cage_encaged_φ", _)
    -- , ("Lorg_eolang_cage_φ", _)
    ("Lorg_eolang_error", evaluateUnaryDataizationFunChain stringToBytes bytesToString wrapBytesInBytes (extractLabel "message") error)
  , -- float
    -- deprecated
    ("Lorg_eolang_float_gt", evaluateBinaryDataizationFunChain boolToBytes bytesToFloat wrapBytesInBytes extractRho (extractLabel "x") (>))
  , -- deprecated
    ("Lorg_eolang_float_times", evaluateFloatFloatFloatFunChain (*))
  , -- deprecated
    ("Lorg_eolang_float_plus", evaluateFloatFloatFloatFunChain (+))
  , -- deprecated
    ("Lorg_eolang_float_div", evaluateFloatFloatFloatFunChain (/))
  , -- deprecated
    ("Lorg_eolang_float_gt", evaluateBinaryDataizationFunChain boolToBytes bytesToFloat wrapBytesInBytes extractRho (extractLabel "x") (>))
  , -- deprecated
    ("Lorg_eolang_float_times", evaluateFloatFloatFloatFunChain (*))
  , -- deprecated
    ("Lorg_eolang_float_plus", evaluateFloatFloatFloatFunChain (+))
  , -- deprecated
    ("Lorg_eolang_float_div", evaluateFloatFloatFloatFunChain (/))
  , -- , ("Lorg_eolang_fs_dir_made_mkdir", _)
    -- , ("Lorg_eolang_fs_dir_tmpfile_touch", _)
    -- , ("Lorg_eolang_fs_dir_walk", _)
    -- , ("Lorg_eolang_fs_file_deleted_delete", _)
    -- , ("Lorg_eolang_fs_file_exists", _)
    -- , ("Lorg_eolang_fs_file_is_directory", _)
    -- , ("Lorg_eolang_fs_file_moved_move", _)
    -- , ("Lorg_eolang_fs_file_open_file_stream_read_read_bytes", _)
    -- , ("Lorg_eolang_fs_file_open_file_stream_write_written_bytes", _)
    -- , ("Lorg_eolang_fs_file_open_process_file", _)
    -- , ("Lorg_eolang_fs_file_size", _)
    -- , ("Lorg_eolang_fs_file_touched_touch", _)
    ("Lorg_eolang_i16_as_i32", evaluateUnaryDataizationFunChain int16ToBytes bytesToInt64 wrapBytesInConstInt extractRho fromIntegral)
  , ("Lorg_eolang_i32_as_i64", evaluateUnaryDataizationFunChain int32ToBytes bytesToInt64 wrapBytesInConstInt extractRho fromIntegral)
  , ("Lorg_eolang_i64_as_number", evaluateUnaryDataizationFunChain floatToBytes bytesToInt64 wrapBytesInConstInt extractRho fromIntegral)
  , ("Lorg_eolang_i64_div", evaluateIntIntMaybeIntFunChain (\x y -> if y == 0 then Nothing else Just (x `quot` y)))
  , ("Lorg_eolang_i64_gt", evaluateIntIntBoolFunChain (>))
  , ("Lorg_eolang_i64_plus", evaluateIntIntIntFunChain (+))
  , ("Lorg_eolang_i64_times", evaluateIntIntIntFunChain (*))
  , -- , ("Lorg_eolang_malloc_of_allocated_read", _)
    -- , ("Lorg_eolang_malloc_of_allocated_resize", _)
    -- , ("Lorg_eolang_malloc_of_allocated_size", _)
    -- , ("Lorg_eolang_malloc_of_allocated_write", _)
    -- , ("Lorg_eolang_malloc_of_φ", _)
    ("Lorg_eolang_math_angle_cos", evaluateUnaryDataizationFunChain floatToBytes bytesToFloat wrapBytesInConstFloat extractRho cos)
  , ("Lorg_eolang_math_angle_sin", evaluateUnaryDataizationFunChain floatToBytes bytesToFloat wrapBytesInConstFloat extractRho sin)
  , ("Lorg_eolang_math_real_acos", evaluateUnaryDataizationFunChain floatToBytes bytesToFloat wrapBytesInConstFloat extractRho acos)
  , ("Lorg_eolang_math_real_asin", evaluateUnaryDataizationFunChain floatToBytes bytesToFloat wrapBytesInConstFloat extractRho asin)
  , ("Lorg_eolang_math_real_ln", evaluateUnaryDataizationFunChain floatToBytes bytesToFloat wrapBytesInConstFloat extractRho log)
  , ("Lorg_eolang_math_real_pow", evaluateFloatFloatFloatFunChain (**))
  , ("Lorg_eolang_math_real_sqrt", evaluateUnaryDataizationFunChain floatToBytes bytesToFloat wrapBytesInConstFloat extractRho sqrt)
  , ("Lorg_eolang_number_as_i64", evaluateUnaryDataizationFunChain int64ToBytes bytesToFloat wrapBytesInConstInt extractRho round)
  , ("Lorg_eolang_number_div", evaluateFloatFloatFloatFunChain (/))
  , ("Lorg_eolang_number_floor", evaluateUnaryDataizationFunChain intToBytes bytesToFloat wrapBytesInConstInt extractRho floor)
  , ("Lorg_eolang_number_gt", evaluateBinaryDataizationFunChain boolToBytes bytesToFloat wrapBytesInBytes extractRho (extractLabel "x") (>))
  , ("Lorg_eolang_number_plus", evaluateFloatFloatFloatFunChain (+))
  , ("Lorg_eolang_number_times", evaluateFloatFloatFloatFunChain (*))
  , -- , ("Lorg_eolang_rust", _)
    -- string
    ("Lorg_eolang_string_length", evaluateUnaryDataizationFunChain intToBytes bytesToString wrapBytesInConstInt extractRho length)
  ,
    ( "Lorg_eolang_string_slice"
    , \name obj state -> do
        thisStr <- incLogLevel $ dataizeRecursivelyChain True (extractRho obj)
        string <- case thisStr of
          AsBytes bytes -> pure $ bytesToString bytes
          AsObject _ -> fail "Couldn't find bytes"
        evaluateBinaryDataizationFunChain stringToBytes bytesToInt wrapBytesInConstString (extractLabel "start") (extractLabel "len") (\start len -> take len (drop start string)) name obj state
    )
  , -- others
    -- , ("Lorg_eolang_sys_os_name", _)
    -- , ("Lorg_eolang_sys_posix_φ", _)
    -- , ("Lorg_eolang_sys_win32_φ", _)
    -- , ("Lorg_eolang_try", _)
    -- , ("Lorg_eolang_txt_regex_compiled", _)
    -- , ("Lorg_eolang_txt_regex_pattern_match_matched_from_index", _)
    -- , ("Lorg_eolang_txt_sprintf", _)
    -- , ("Lorg_eolang_txt_sscanf", _)

    ( "Package"
    , let
        f _name obj@(Formation bindings) = do
          \state ->
            getContext
              >>= ( \case
                      True -> do
                        let (packageBindings, restBindings) = span isPackage bindings
                        bs <- mapM dataizeBindingChain restBindings
                        logStep "Dataized 'Package' siblings" (AsObject $ Formation (bs ++ packageBindings))
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
