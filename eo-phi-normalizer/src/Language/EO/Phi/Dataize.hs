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
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant fmap" #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.EO.Phi.Dataize where

import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (listToMaybe)
import Language.EO.Phi.Rules.Common
import Language.EO.Phi.Rules.Fast (fastYegorInsideOut)
import Language.EO.Phi.Rules.Yaml (substThis)
import Language.EO.Phi.Syntax
import PyF (fmt)
import System.IO.Unsafe (unsafePerformIO)

desugarAsBytes :: Either Object Bytes -> Either Object Bytes
desugarAsBytes (Left obj) = case obj of
  ConstString s -> Right (stringToBytes s)
  ConstInt n -> Right (intToBytes (fromInteger n))
  ConstFloat x -> Right (floatToBytes x)
  _ -> Left obj
desugarAsBytes (Right bytes) = Right bytes

pattern AsBytes :: Bytes -> Either Object Bytes
pattern AsBytes bytes <- (desugarAsBytes -> Right bytes)
  where
    AsBytes bytes = Right bytes

pattern AsObject :: Object -> Either Object Bytes
pattern AsObject obj <- (desugarAsBytes -> Left obj)
  where
    AsObject obj = Left obj

{-# COMPLETE AsBytes, AsObject #-}

-- | Perform one step of dataization to the object (if possible).
dataizeStep :: Context -> Object -> (Context, Either Object Bytes)
dataizeStep ctx obj = snd $ head $ runChain (dataizeStepChain obj) ctx -- FIXME: head is bad

dataizeStep' :: Context -> Object -> Either Object Bytes
dataizeStep' ctx obj = snd (dataizeStep ctx obj)

-- | Recursively perform normalization and dataization until we get bytes in the end.
dataizeRecursively :: Context -> Object -> Either Object Bytes
dataizeRecursively ctx obj = snd $ dataizeRecursivelyChain' ctx obj

dataizeStepChain' :: Context -> Object -> ([LogEntry (Either Object Bytes)], Either Object Bytes)
dataizeStepChain' ctx obj = snd <$> head (runChain (dataizeStepChain obj) ctx) -- FIXME: head is bad

-- | Perform one step of dataization to the object (if possible), reporting back individiual steps.
dataizeStepChain :: Object -> DataizeChain (Context, Either Object Bytes)
dataizeStepChain obj@(Formation bs)
  | Just (DeltaBinding bytes) <- listToMaybe [b | b@(DeltaBinding _) <- bs]
  , not hasEmpty = do
      logStep "Found bytes" (AsBytes bytes)
      ctx <- getContext
      return (ctx, AsBytes bytes)
  | Just (LambdaBinding (Function funcName)) <- listToMaybe [b | b@(LambdaBinding _) <- bs]
  , not hasEmpty = do
      ctx' <- getContext
      let lambaIsKnownAndNotEnabled = HashMap.member funcName ctx'.knownAtoms && not (HashMap.member funcName ctx'.enabledAtoms)
      if lambaIsKnownAndNotEnabled
        then do
          logStep [fmt|Not evaluating the lambda '{funcName}' since it's disabled.|] (AsObject obj)
          pure (ctx', AsObject obj)
        else do
          logStep [fmt|Evaluating lambda '{funcName}' |] (AsObject obj)
          msplit (evaluateBuiltinFunChain funcName obj ()) >>= \case
            Nothing -> do
              ctx <- getContext
              return (ctx, AsObject obj)
            Just ((obj', _state), _alts) -> do
              ctx <- getContext
              return (ctx, AsObject obj')
  | Just (AlphaBinding Phi decoratee) <- listToMaybe [b | b@(AlphaBinding Phi _) <- bs]
  , not hasEmpty = do
      let decoratee' = substThis obj decoratee
      logStep "Dataizing inside phi" (AsObject decoratee')
      ctx <- getContext
      let extendedContext = (extendContextWith obj ctx){currentAttr = Phi}
      return (extendedContext, AsObject decoratee')
  | otherwise = do
      logStep "No change to formation" (AsObject obj)
      ctx <- getContext
      return (ctx, AsObject obj)
 where
  isEmpty (EmptyBinding _) = True
  isEmpty DeltaEmptyBinding = True
  isEmpty _ = False
  hasEmpty = any isEmpty bs
-- IMPORTANT: dataize the object being copied IF normalization is stuck on it!
dataizeStepChain (Application obj bindings) = incLogLevel $ do
  logStep "Dataizing inside application" (AsObject obj)
  modifyContext (\c -> c{dataizePackage = False}) $ do
    (ctx, obj') <- dataizeStepChain obj
    case obj' of
      Left obj'' -> return (ctx, AsObject (obj'' `Application` bindings))
      Right bytes -> return (ctx, AsObject (Formation [DeltaBinding bytes] `Application` bindings))
-- IMPORTANT: dataize the object being dispatched IF normalization is stuck on it!
dataizeStepChain (ObjectDispatch obj attr) = incLogLevel $ do
  logStep "Dataizing inside dispatch" (AsObject obj)
  modifyContext (\c -> c{dataizePackage = False}) $ do
    (ctx, obj') <- dataizeStepChain obj
    case obj' of
      Left obj'' -> return (ctx, AsObject (obj'' `ObjectDispatch` attr))
      Right bytes -> return (ctx, AsObject (Formation [DeltaBinding bytes] `ObjectDispatch` attr))
dataizeStepChain obj = do
  logStep "Nothing to dataize" (AsObject obj)
  ctx <- getContext
  return (ctx, AsObject obj)

dataizeRecursivelyChain' :: Context -> Object -> ([LogEntry (Either Object Bytes)], Either Object Bytes)
dataizeRecursivelyChain' ctx obj = head (runChain (dataizeRecursivelyChain False obj) ctx)

-- | Recursively perform normalization and dataization until we get bytes in the end,
-- reporting intermediate steps
dataizeRecursivelyChain :: Bool -> Object -> DataizeChain (Either Object Bytes)
dataizeRecursivelyChain = fmap minimizeObject' . go
 where
  go normalizeRequired obj = do
    logStep "Dataizing" (AsObject obj)
    ctx <- getContext
    let globalObject = NonEmpty.last (outerFormations ctx)
    let limits = defaultApplicationLimits (objectSize globalObject)
    let normalizedObj
          | builtinRules ctx = do
              let obj' = fastYegorInsideOut ctx obj
              logStep "Normalized" obj'
              return obj'
          | otherwise = applyRulesChainWith limits obj
    msplit (transformNormLogs normalizedObj) >>= \case
      Nothing -> do
        logStep "No rules applied" (AsObject obj)
        return (AsObject obj)
      -- We trust that all chains lead to the same result due to confluence
      Just (normObj, _alternatives)
        | normObj == obj && normalizeRequired -> return (AsObject obj)
        | otherwise -> do
            (ctx', step) <- dataizeStepChain normObj
            case step of
              (AsObject stillObj)
                | stillObj == normObj && ctx `sameContext` ctx' -> do
                    logStep "Dataization changed nothing" (AsObject stillObj)
                    return step -- dataization changed nothing
                | otherwise -> do
                    logStep "Dataization changed something" (AsObject stillObj)
                    withContext ctx' $ go False stillObj -- partially dataized
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
  lhs <- incLogLevel $ do
    logStep "Evaluating LHS" (AsObject o_rho)
    dataizeRecursivelyChain True o_rho
  rhs <- incLogLevel $ do
    logStep "Evaluating RHS" (AsObject o_a0)
    dataizeRecursivelyChain True o_a0
  result <- case (lhs, rhs) of
    (AsBytes l, AsBytes r) -> do
      let bytes = resultToBytes (bytesToParam r `func` bytesToParam l)
          resultObj = wrapBytes bytes
      logStep "Evaluated function" (AsObject resultObj)
      return resultObj
    _ -> fail "Couldn't find bytes in one or both of LHS and RHS"
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
  -- | Name of the atom.
  String ->
  Object ->
  EvaluationState ->
  DataizeChain (Object, EvaluationState)
evaluateBinaryDataizationFunChain resultToBytes bytesToParam wrapBytes arg1 arg2 func name obj _state = do
  let lhsArg = arg1 obj
  let rhsArg = arg2 obj
  lhs <- incLogLevel $ do
    logStep "Evaluating LHS" (AsObject lhsArg)
    dataizeRecursivelyChain True lhsArg
  rhs <- incLogLevel $ do
    logStep "Evaluating RHS" (AsObject rhsArg)
    dataizeRecursivelyChain True rhsArg
  result <- case (lhs, rhs) of
    (AsBytes l, AsBytes r) -> do
      let bytes = resultToBytes (bytesToParam l `func` bytesToParam r)
          resultObj = wrapBytes bytes
      logStep "Evaluated function" (AsObject resultObj)
      return resultObj
    (AsObject _l, AsObject _r) ->
      fail (name <> ": Couldn't find bytes in both LHS and RHS")
    (AsObject l, _) -> do
      fail (name <> ": Couldn't find bytes in LHS: " <> printTree (hideRho l))
    (_, AsObject r) -> do
      fail (name <> ": Couldn't find bytes in RHS: " <> printTree (hideRho r))
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
  String ->
  Object ->
  EvaluationState ->
  DataizeChain (Object, EvaluationState)
evaluateUnaryDataizationFunChain resultToBytes bytesToParam wrapBytes extractArg func =
  evaluateBinaryDataizationFunChain resultToBytes bytesToParam wrapBytes extractArg extractArg (const . func)

-- This should maybe get converted to a type class and some instances?
evaluateIntIntIntFunChain :: (Int -> Int -> Int) -> String -> Object -> EvaluationState -> DataizeChain (Object, EvaluationState)
evaluateIntIntIntFunChain = evaluateBinaryDataizationFunChain intToBytes bytesToInt wrapBytesInConstInt extractRho (extractLabel "x")

evaluateIntIntBoolFunChain :: (Int -> Int -> Bool) -> String -> Object -> EvaluationState -> DataizeChain (Object, EvaluationState)
evaluateIntIntBoolFunChain = evaluateBinaryDataizationFunChain boolToBytes bytesToInt wrapBytesAsBool extractRho (extractLabel "x")

-- Int because Bytes are just a string, but Int has a Bits instance
evaluateBytesBytesBytesFunChain :: (Int -> Int -> Int) -> String -> Object -> EvaluationState -> DataizeChain (Object, EvaluationState)
evaluateBytesBytesBytesFunChain = evaluateBinaryDataizationFunChain intToBytes bytesToInt wrapBytesInBytes extractRho (extractLabel "b")

evaluateBytesBytesFunChain :: (Int -> Int) -> String -> Object -> EvaluationState -> DataizeChain (Object, EvaluationState)
evaluateBytesBytesFunChain = evaluateUnaryDataizationFunChain intToBytes bytesToInt wrapBytesInBytes extractRho

evaluateFloatFloatFloatFunChain :: (Double -> Double -> Double) -> String -> Object -> EvaluationState -> DataizeChain (Object, EvaluationState)
evaluateFloatFloatFloatFunChain = evaluateBinaryDataizationFunChain floatToBytes bytesToFloat wrapBytesInConstFloat extractRho (extractLabel "x")

-- | Like `evaluateDataizationFunChain` but specifically for the built-in functions.
-- This function is not safe. It returns undefined for unknown functions
evaluateBuiltinFunChain :: String -> Object -> EvaluationState -> DataizeChain (Object, EvaluationState)
evaluateBuiltinFunChain name obj state = do
  ctx <- getContext
  case HashMap.lookup name ctx.knownAtoms of
    Just f -> f name obj state
    Nothing -> evaluateBuiltinFunChainUnknown name obj state

evaluateBuiltinFunChainUnknown :: String -> Object -> EvaluationState -> DataizeChain (Object, EvaluationState)
evaluateBuiltinFunChainUnknown atomName obj state = do
  logStep [fmt|[INFO]: unknown atom ({atomName})|] (AsObject obj)
  return (obj, state)

-- | Like `evaluateDataizationFun` but specifically for the built-in functions.
-- This function is not safe. It returns undefined for unknown functions
evaluateBuiltinFun :: Context -> String -> Object -> EvaluationState -> (Object, EvaluationState)
evaluateBuiltinFun ctx name obj state = snd $ head $ runChain (evaluateBuiltinFunChain name obj state) ctx -- FIXME: head is bad

evaluateIODataizationFunChain :: IO String -> Object -> EvaluationState -> DataizeChain (Object, EvaluationState)
evaluateIODataizationFunChain action _obj state =
  return (Formation [DeltaBinding (stringToBytes (unsafePerformIO action))], state)

extractRho :: Object -> Object
extractRho = (`ObjectDispatch` Rho)
extractAlpha0 :: Object -> Object
extractAlpha0 = (`ObjectDispatch` Alpha (AlphaIndex "α0"))
extractLabel :: String -> Object -> Object
extractLabel attrName = (`ObjectDispatch` Label (LabelId attrName))
