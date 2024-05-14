{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant fmap" #-}

module Language.EO.Phi.Dataize where

import Control.Arrow (left)
import Data.Bits
import Data.List (singleton)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (listToMaybe)
import Language.EO.Phi.Rules.Common
import Language.EO.Phi.Syntax.Abs
import PyF (fmt)
import System.IO.Unsafe (unsafePerformIO)

-- | Perform one step of dataization to the object (if possible).
dataizeStep :: Context -> Object -> (Context, Either Object Bytes)
dataizeStep ctx obj = snd $ head $ runChain (dataizeStepChain obj) ctx -- FIXME: head is bad

dataizeStep' :: Context -> Object -> Either Object Bytes
dataizeStep' ctx obj = snd (dataizeStep ctx obj)

-- | State of evaluation is not needed yet, but it might be in the future
type EvaluationState = ()

-- | Recursively perform normalization and dataization until we get bytes in the end.
dataizeRecursively :: Context -> Object -> Either Object Bytes
dataizeRecursively ctx obj = snd $ dataizeRecursivelyChain' ctx obj

dataizeStepChain' :: Context -> Object -> ([(String, Either Object Bytes)], Either Object Bytes)
dataizeStepChain' ctx obj = snd <$> head (runChain (dataizeStepChain obj) ctx) -- FIXME: head is bad

-- | Perform one step of dataization to the object (if possible), reporting back individiual steps.
dataizeStepChain :: Object -> DataizeChain (Context, Either Object Bytes)
dataizeStepChain obj@(Formation bs)
  | Just (DeltaBinding bytes) <- listToMaybe [b | b@(DeltaBinding _) <- bs]
  , not hasEmpty = do
      logStep "Found bytes" (Right bytes)
      ctx <- getContext
      return (ctx, Right bytes)
  | Just (LambdaBinding (Function funcName)) <- listToMaybe [b | b@(LambdaBinding _) <- bs]
  , not hasEmpty = do
      logStep ("Evaluating lambda '" <> funcName <> "'") (Left obj)
      (obj', _state) <- evaluateBuiltinFunChain funcName obj ()
      ctx <- getContext
      return (ctx, Left obj')
  | Just (AlphaBinding Phi decoratee) <- listToMaybe [b | b@(AlphaBinding Phi _) <- bs]
  , not hasEmpty = do
      logStep "Dataizing inside phi" (Left decoratee)
      ctx <- getContext
      let extendedContext = (extendContextWith obj ctx){currentAttr = Phi}
      withContext extendedContext $ dataizeStepChain decoratee
  | otherwise = do
      logStep "No change to formation" (Left obj)
      ctx <- getContext
      return (ctx, Left obj)
 where
  isEmpty (EmptyBinding _) = True
  isEmpty DeltaEmptyBinding = True
  isEmpty _ = False
  hasEmpty = any isEmpty bs
-- IMPORTANT: dataize the object being copied IF normalization is stuck on it!
dataizeStepChain (Application obj bindings) = do
  logStep "Dataizing inside application" (Left obj)
  modifyContext (\c -> c{dataizePackage = False}) $ do
    (ctx, obj') <- dataizeStepChain obj
    return (ctx, left (`Application` bindings) obj')
-- IMPORTANT: dataize the object being dispatched IF normalization is stuck on it!
dataizeStepChain (ObjectDispatch obj attr) = do
  logStep "Dataizing inside dispatch" (Left obj)
  modifyContext (\c -> c{dataizePackage = False}) $ do
    (ctx, obj') <- dataizeStepChain obj
    return (ctx, left (`ObjectDispatch` attr) obj')
dataizeStepChain obj = do
  logStep "Nothing to dataize" (Left obj)
  ctx <- getContext
  return (ctx, Left obj)

dataizeRecursivelyChain' :: Context -> Object -> ([(String, Either Object Bytes)], Either Object Bytes)
dataizeRecursivelyChain' ctx obj = head (runChain (dataizeRecursivelyChain False obj) ctx)

-- | Recursively perform normalization and dataization until we get bytes in the end,
-- reporting intermediate steps
dataizeRecursivelyChain :: Bool -> Object -> DataizeChain (Either Object Bytes)
dataizeRecursivelyChain normalizeRequired obj = do
  logStep "Dataizing" (Left obj)
  ctx <- getContext
  let globalObject = NonEmpty.last (outerFormations ctx)
      limits = defaultApplicationLimits (objectSize globalObject)
  msplit (transformNormLogs (applyRulesChainWith limits obj)) >>= \case
    Nothing -> do
      logStep "No rules applied" (Left obj)
      return (Left obj)
    -- We trust that all chains lead to the same result due to confluence
    Just (normObj, _alternatives)
      | normObj == obj && normalizeRequired -> return (Left obj)
      | otherwise -> do
          (ctx', step) <- dataizeStepChain normObj
          case step of
            (Left stillObj)
              | stillObj == normObj && ctx `sameContext` ctx' -> return step -- dataization changed nothing
              | otherwise -> withContext ctx' $ dataizeRecursivelyChain False stillObj -- partially dataized
            bytes -> return bytes

-- | Given converters between Bytes and some data type, a binary function on this data type, an object,
-- and the current state of evaluation, returns the new object and a possibly modified state along with intermediate steps.
evaluateDataizationFunChain ::
  -- | How to convert the result back to bytes
  (res -> Bytes) ->
  -- | How to interpret the bytes in terms of the given data type
  (Bytes -> a) ->
  -- | How to wrap the bytes in an object
  (Bytes -> Object) ->
  -- | A binary function on the data
  (a -> a -> res) ->
  Object ->
  EvaluationState ->
  DataizeChain (Object, EvaluationState)
evaluateDataizationFunChain resultToBytes bytesToParam wrapBytes func obj _state = do
  let o_rho = ObjectDispatch obj Rho
  let o_a0 = ObjectDispatch obj (Alpha (AlphaIndex "α0"))
  logStep "Evaluating LHS" (Left o_rho)
  lhs <- dataizeRecursivelyChain True o_rho
  logStep "Evaluating RHS" (Left o_a0)
  rhs <- dataizeRecursivelyChain True o_a0
  result <- case (lhs, rhs) of
    (Right l, Right r) -> do
      let bytes = resultToBytes (bytesToParam r `func` bytesToParam l)
          resultObj = wrapBytes bytes
      logStep "Evaluated function" (Left resultObj)
      return resultObj
    _ -> do
      logStep "Couldn't find bytes in one or both of LHS and RHS" (Left Termination)
      return Termination
  return (result, ())

evaluateBinaryDataizationFunChain ::
  -- | How to convert the result back to bytes
  (res -> Bytes) ->
  -- | How to interpret the bytes in terms of the given data type
  (Bytes -> a) ->
  -- | How to wrap the bytes in an object
  (Bytes -> Object) ->
  -- | Extract the 1st argument to be dataized
  (Object -> Object) ->
  -- | Extract the 2nd argument to be dataized
  (Object -> Object) ->
  -- | A binary function on the argument
  (a -> a -> res) ->
  Object ->
  EvaluationState ->
  DataizeChain (Object, EvaluationState)
evaluateBinaryDataizationFunChain resultToBytes bytesToParam wrapBytes arg1 arg2 func obj _state = do
  let lhsArg = arg1 obj
  let rhsArg = arg2 obj
  logStep "Evaluating LHS" (Left lhsArg)
  lhs <- dataizeRecursivelyChain True lhsArg
  logStep "Evaluating RHS" (Left rhsArg)
  rhs <- dataizeRecursivelyChain True rhsArg
  result <- case (lhs, rhs) of
    (Right l, Right r) -> do
      let bytes = resultToBytes (bytesToParam l `func` bytesToParam r)
          resultObj = wrapBytes bytes
      logStep "Evaluated function" (Left resultObj)
      return resultObj
    _ -> do
      logStep "Couldn't find bytes in one or both of LHS and RHS" (Left Termination)
      return Termination
  return (result, ())

-- | Unary functions operate on the given object without any additional parameters
evaluateUnaryDataizationFunChain ::
  -- | How to convert the result back to bytes
  (res -> Bytes) ->
  -- | How to interpret the bytes in terms of the given data type
  (Bytes -> a) ->
  -- | How to wrap the bytes in an object
  (Bytes -> Object) ->
  -- | Extract the argument to be dataized
  (Object -> Object) ->
  -- | A unary function on the argument
  (a -> res) ->
  Object ->
  EvaluationState ->
  DataizeChain (Object, EvaluationState)
evaluateUnaryDataizationFunChain resultToBytes bytesToParam wrapBytes extractArg func =
  evaluateBinaryDataizationFunChain resultToBytes bytesToParam wrapBytes extractArg extractArg (const . func)

evaluateIODataizationFunChain :: IO String -> Object -> EvaluationState -> DataizeChain (Object, EvaluationState)
evaluateIODataizationFunChain action _obj state =
  return (Formation [DeltaBinding (stringToBytes (unsafePerformIO action))], state)

extractRho :: Object -> Object
extractRho = (`ObjectDispatch` Rho)
extractAlpha0 :: Object -> Object
extractAlpha0 = (`ObjectDispatch` Alpha (AlphaIndex "α0"))
extractLabel :: String -> Object -> Object
extractLabel attrName = (`ObjectDispatch` Label (LabelId attrName))
wrapBytesInInt :: Bytes -> Object
wrapBytesInInt (Bytes bytes) = [fmt|Φ.org.eolang.int(as-bytes ↦ Φ.org.eolang.bytes(Δ ⤍ {bytes}))|]
wrapBytesInFloat :: Bytes -> Object
wrapBytesInFloat (Bytes bytes) = [fmt|Φ.org.eolang.float(as-bytes ↦ Φ.org.eolang.bytes(Δ ⤍ {bytes}))|]
wrapBytesInString :: Bytes -> Object
wrapBytesInString (Bytes bytes) = [fmt|Φ.org.eolang.string(as-bytes ↦ Φ.org.eolang.bytes(Δ ⤍ {bytes}))|]
wrapBytesInBytes :: Bytes -> Object
wrapBytesInBytes (Bytes bytes) = [fmt|Φ.org.eolang.bytes(Δ ⤍ {bytes})|]

-- This should maybe get converted to a type class and some instances?
evaluateIntIntIntFunChain :: (Int -> Int -> Int) -> Object -> EvaluationState -> DataizeChain (Object, EvaluationState)
evaluateIntIntIntFunChain = evaluateBinaryDataizationFunChain intToBytes bytesToInt wrapBytesInInt extractRho (extractLabel "x")

evaluateIntIntBoolFunChain :: (Int -> Int -> Bool) -> Object -> EvaluationState -> DataizeChain (Object, EvaluationState)
evaluateIntIntBoolFunChain = evaluateBinaryDataizationFunChain boolToBytes bytesToInt wrapBytesInBytes extractRho (extractLabel "x")

-- Int because Bytes are just a string, but Int has a Bits instance
evaluateBytesBytesBytesFunChain :: (Int -> Int -> Int) -> Object -> EvaluationState -> DataizeChain (Object, EvaluationState)
evaluateBytesBytesBytesFunChain = evaluateBinaryDataizationFunChain intToBytes bytesToInt wrapBytesInBytes extractRho (extractLabel "b")

evaluateBytesBytesFunChain :: (Int -> Int) -> Object -> EvaluationState -> DataizeChain (Object, EvaluationState)
evaluateBytesBytesFunChain = evaluateUnaryDataizationFunChain intToBytes bytesToInt wrapBytesInBytes extractRho

evaluateFloatFloatFloatFunChain :: (Double -> Double -> Double) -> Object -> EvaluationState -> DataizeChain (Object, EvaluationState)
evaluateFloatFloatFloatFunChain = evaluateBinaryDataizationFunChain floatToBytes bytesToFloat wrapBytesInFloat extractRho (extractLabel "x")

-- | Like `evaluateDataizationFunChain` but specifically for the built-in functions.
-- This function is not safe. It returns undefined for unknown functions
evaluateBuiltinFunChain :: String -> Object -> EvaluationState -> DataizeChain (Object, EvaluationState)
-- int
evaluateBuiltinFunChain "Lorg_eolang_int_gt" obj = evaluateIntIntBoolFunChain (>) obj
evaluateBuiltinFunChain "Lorg_eolang_int_plus" obj = evaluateIntIntIntFunChain (+) obj
evaluateBuiltinFunChain "Lorg_eolang_int_times" obj = evaluateIntIntIntFunChain (*) obj
evaluateBuiltinFunChain "Lorg_eolang_int_div" obj = evaluateIntIntIntFunChain div obj
-- bytes
evaluateBuiltinFunChain "Lorg_eolang_bytes_eq" obj = evaluateBinaryDataizationFunChain boolToBytes bytesToInt wrapBytesInBytes extractRho (extractLabel "b") (==) obj
evaluateBuiltinFunChain "Lorg_eolang_bytes_size" obj = evaluateUnaryDataizationFunChain intToBytes id wrapBytesInBytes extractRho (\(Bytes bytes) -> length (words (map dashToSpace bytes))) obj
 where
  dashToSpace '-' = ' '
  dashToSpace c = c
evaluateBuiltinFunChain "Lorg_eolang_bytes_slice" obj = \state -> do
  thisStr <- dataizeRecursivelyChain True (extractRho obj)
  (Bytes bytes) <- case thisStr of
    Right bytes -> pure bytes
    Left _ -> fail "Couldn't find bytes"
  evaluateBinaryDataizationFunChain id bytesToInt wrapBytesInBytes (extractLabel "start") (extractLabel "len") (\start len -> Bytes $ normalizeBytes $ take len (drop start (filter (/= '-') bytes))) obj state
evaluateBuiltinFunChain "Lorg_eolang_bytes_and" obj = evaluateBytesBytesBytesFunChain (.&.) obj
evaluateBuiltinFunChain "Lorg_eolang_bytes_or" obj = evaluateBytesBytesBytesFunChain (.|.) obj
evaluateBuiltinFunChain "Lorg_eolang_bytes_xor" obj = evaluateBytesBytesBytesFunChain (.^.) obj
evaluateBuiltinFunChain "Lorg_eolang_bytes_not" obj = evaluateBytesBytesFunChain complement obj
evaluateBuiltinFunChain "Lorg_eolang_bytes_right" obj = evaluateBinaryDataizationFunChain intToBytes bytesToInt wrapBytesInBytes extractRho (extractLabel "x") shiftR obj
evaluateBuiltinFunChain "Lorg_eolang_bytes_concat" obj = evaluateBinaryDataizationFunChain id id wrapBytesInBytes extractRho (extractLabel "b") (\(Bytes b1) (Bytes b2) -> Bytes (b1 ++ b2)) obj
-- float
evaluateBuiltinFunChain "Lorg_eolang_float_gt" obj = evaluateBinaryDataizationFunChain boolToBytes bytesToFloat wrapBytesInBytes extractRho (extractLabel "x") (>) obj
evaluateBuiltinFunChain "Lorg_eolang_float_times" obj = evaluateFloatFloatFloatFunChain (*) obj
evaluateBuiltinFunChain "Lorg_eolang_float_plus" obj = evaluateFloatFloatFloatFunChain (+) obj
evaluateBuiltinFunChain "Lorg_eolang_float_div" obj = evaluateFloatFloatFloatFunChain (/) obj
-- string
evaluateBuiltinFunChain "Lorg_eolang_string_length" obj = evaluateUnaryDataizationFunChain intToBytes bytesToString wrapBytesInInt extractRho length obj
evaluateBuiltinFunChain "Lorg_eolang_string_slice" obj = \state -> do
  thisStr <- dataizeRecursivelyChain True (extractRho obj)
  string <- case thisStr of
    Right bytes -> pure $ bytesToString bytes
    Left _ -> fail "Couldn't find bytes"
  evaluateBinaryDataizationFunChain stringToBytes bytesToInt wrapBytesInString (extractLabel "start") (extractLabel "len") (\start len -> take len (drop start string)) obj state
-- malloc
-- evaluateBuiltinFunChain "Lorg_eolang_malloc_φ" obj = _ -- TODO
-- evaluateBuiltinFunChain "Lorg_eolang_malloc_memory_block_pointer_read" obj = _ -- TODO
-- evaluateBuiltinFunChain "Lorg_eolang_malloc_memory_block_pointer_write" obj = _ -- TODO
-- evaluateBuiltinFunChain "Lorg_eolang_malloc_memory_block_pointer_free" obj = _ -- TODO
-- cage
-- evaluateBuiltinFunChain "Lorg_eolang_cage_φ" obj = _ -- TODO
-- evaluateBuiltinFunChain "Lorg_eolang_cage_encaged_φ" obj = _ -- TODO
-- evaluateBuiltinFunChain "Lorg_eolang_cage_encaged_encage" obj = _ -- TODO
-- I/O
evaluateBuiltinFunChain "Lorg_eolang_io_stdin_next_line" obj = evaluateIODataizationFunChain getLine obj
evaluateBuiltinFunChain "Lorg_eolang_io_stdin_φ" obj = evaluateIODataizationFunChain getContents obj
evaluateBuiltinFunChain "Lorg_eolang_io_stdout" obj = evaluateUnaryDataizationFunChain boolToBytes bytesToString wrapBytesInBytes (extractLabel "text") ((`seq` True) . unsafePerformIO . putStrLn) obj
-- others
-- evaluateBuiltinFunChain "Lorg_eolang_dataized" obj = _ -- TODO
evaluateBuiltinFunChain "Lorg_eolang_error" obj = evaluateUnaryDataizationFunChain stringToBytes bytesToString wrapBytesInBytes (extractLabel "message") error obj
-- evaluateBuiltinFunChain "Lorg_eolang_seq" obj = _ -- TODO
-- evaluateBuiltinFunChain "Lorg_eolang_as_phi" obj = _ -- TODO
-- evaluateBuiltinFunChain "Lorg_eolang_rust" obj = _ -- TODO
-- evaluateBuiltinFunChain "Lorg_eolang_try" obj = _ -- TODO
evaluateBuiltinFunChain "Package" obj@(Formation bindings) = do
  \state -> do
    fmap dataizePackage getContext >>= \case
      True -> do
        let (packageBindings, restBindings) = span isPackage bindings
        bs <- mapM dataizeBindingChain restBindings
        logStep "Dataized 'Package' siblings" (Left $ Formation (bs ++ packageBindings))
        return (Formation (bs ++ packageBindings), state)
      False ->
        return (Formation bindings, state)
 where
  isPackage (LambdaBinding (Function "Package")) = True
  isPackage _ = False
  dataizeBindingChain (AlphaBinding attr o) = do
    ctx <- getContext
    let extendedContext = (extendContextWith obj ctx){currentAttr = attr}
    dataizationResult <- withContext extendedContext $ dataizeRecursivelyChain False o
    return (AlphaBinding attr (either id (Formation . singleton . DeltaBinding) dataizationResult))
  dataizeBindingChain b = return b
evaluateBuiltinFunChain _ obj = \state -> return (obj, state)

-- | Like `evaluateDataizationFun` but specifically for the built-in functions.
-- This function is not safe. It returns undefined for unknown functions
evaluateBuiltinFun :: Context -> String -> Object -> EvaluationState -> (Object, EvaluationState)
evaluateBuiltinFun ctx name obj state = snd $ head $ runChain (evaluateBuiltinFunChain name obj state) ctx -- FIXME: head is bad
