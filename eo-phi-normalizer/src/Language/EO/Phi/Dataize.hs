{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.EO.Phi.Dataize where

import Control.Arrow (ArrowChoice (left))
import Data.List (find, singleton)
import Data.Maybe (listToMaybe)
import Data.String.Interpolate (i)
import Language.EO.Phi (Binding (DeltaEmptyBinding, EmptyBinding), printTree)
import Language.EO.Phi.Rules.Common
import Language.EO.Phi.Syntax.Abs (
  AlphaIndex (AlphaIndex),
  Attribute (Alpha, Phi, Rho),
  Binding (AlphaBinding, DeltaBinding, LambdaBinding),
  Bytes (Bytes),
  Function (Function),
  Object (Application, Formation, ObjectDispatch, Termination),
 )

-- | Perform one step of dataization to the object (if possible).
dataizeStep :: Context -> Object -> (Context, Either Object Bytes)
dataizeStep ctx obj@(Formation bs)
  | Just (DeltaBinding bytes) <- find isDelta bs, not hasEmpty = (ctx, Right bytes)
  | Just (LambdaBinding (Function funcName)) <- find isLambda bs, not hasEmpty = (ctx, Left (fst $ evaluateBuiltinFun ctx funcName obj ()))
  | Just (AlphaBinding Phi decoratee) <- find isPhi bs
  , not hasEmpty =
      dataizeStep (extendContextWith obj ctx){currentAttr = Phi} decoratee
  | otherwise = (ctx, Left obj)
 where
  isDelta (DeltaBinding _) = True
  isDelta _ = False
  isLambda (LambdaBinding _) = True
  isLambda _ = False
  isPhi (AlphaBinding Phi _) = True
  isPhi _ = False
  isEmpty (EmptyBinding _) = True
  isEmpty DeltaEmptyBinding = True
  isEmpty _ = False
  hasEmpty = any isEmpty bs
dataizeStep ctx (Application obj bindings) = case dataizeStep ctx obj of
  (ctx', Left dataized) -> (ctx', Left $ Application dataized bindings)
  (_ctx', Right _) -> error ("Application on bytes upon dataizing:\n  " <> printTree obj)
dataizeStep ctx (ObjectDispatch obj attr) = case dataizeStep ctx obj of
  (ctx', Left dataized) -> (ctx', Left $ ObjectDispatch dataized attr)
  (_ctx', Right _) -> error ("Dispatch on bytes upon dataizing:\n  " <> printTree obj)
dataizeStep ctx obj = (ctx, Left obj)

dataizeStep' :: Context -> Object -> Either Object Bytes
dataizeStep' ctx obj = snd (dataizeStep ctx obj)

-- | State of evaluation is not needed yet, but it might be in the future
type EvaluationState = ()

-- | Recursively perform normalization and dataization until we get bytes in the end.
dataizeRecursively :: Context -> Object -> Either Object Bytes
dataizeRecursively ctx obj = case applyRules ctx obj of
  [normObj] -> case dataizeStep ctx normObj of
    (ctx', Left stillObj)
      | stillObj == normObj && ctx `sameContext` ctx' -> Left stillObj -- dataization changed nothing
      | otherwise -> dataizeRecursively ctx' stillObj -- partially dataized
    (_ctx', Right bytes) -> Right bytes
  objs -> error errMsg -- Left Termination
   where
    errMsg =
      "Expected 1 result from normalization but got "
        <> show (length objs)
        <> ":\n"
        <> unlines (map (("  - " ++) . printTree) objs)
        <> "\nFor the input:\n  "
        <> printTree obj

dataizeStepChain' :: Context -> Object -> ([(String, Either Object Bytes)], Either Object Bytes)
dataizeStepChain' ctx obj = snd <$> runChain (dataizeStepChain obj) ctx

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
      logStep "Dataizing inside phi" (Left decoratee)
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
dataizeStepChain (Application obj bindings) = do
  logStep "Dataizing inside application" (Left obj)
  (ctx, obj') <- dataizeStepChain obj
  return (ctx, left (`Application` bindings) obj')
dataizeStepChain (ObjectDispatch obj attr) = do
  logStep "Dataizing inside dispatch" (Left obj)
  (ctx, obj') <- dataizeStepChain obj
  return (ctx, left (`ObjectDispatch` attr) obj')
dataizeStepChain obj = do
  logStep "Nothing to dataize" (Left obj)
  ctx <- getContext
  return (ctx, Left obj)

dataizeRecursivelyChain' :: Context -> Object -> ([(String, Either Object Bytes)], Either Object Bytes)
dataizeRecursivelyChain' ctx obj = runChain (dataizeRecursivelyChain obj) ctx

-- | Recursively perform normalization and dataization until we get bytes in the end,
-- reporting intermediate steps
dataizeRecursivelyChain :: Object -> DataizeChain (Either Object Bytes)
dataizeRecursivelyChain obj = do
  ctx <- getContext
  chains <- transformNormLogs $ applyRulesChain obj
  case chains of
    [] -> do
      logStep "No rules applied" (Left obj)
      return (Left obj)
    -- We trust that all chains lead to the same result due to confluence
    (chain : _) -> do
      let normObj = last chain
      (ctx', step) <- dataizeStepChain normObj
      case step of
        (Left stillObj)
          | stillObj == normObj && ctx `sameContext` ctx' -> return step -- dataization changed nothing
          | otherwise -> withContext ctx' $ dataizeRecursivelyChain stillObj -- partially dataized
        bytes -> return bytes

-- | Given normalization context, a function on data (bytes interpreted as integers), an object,
-- and the current state of evaluation, returns the new object and a possibly modified state along with intermediate steps.
evaluateDataizationFunChain :: (Int -> Int -> Int) -> Object -> EvaluationState -> DataizeChain (Object, EvaluationState)
evaluateDataizationFunChain func obj _state = do
  let o_rho = ObjectDispatch obj Rho
  let o_a0 = ObjectDispatch obj (Alpha (AlphaIndex "α0"))
  logStep "Evaluating LHS" (Left o_rho)
  lhs <- dataizeRecursivelyChain o_rho
  logStep "Evaluating RHS" (Left o_a0)
  rhs <- dataizeRecursivelyChain o_a0
  result <- case (lhs, rhs) of
    (Right l, Right r) -> do
      let (Bytes bytes) = intToBytes (bytesToInt r `func` bytesToInt l)
          resultObj = [i|Φ.org.eolang.float(Δ ⤍ #{bytes})|]
      logStep "Evaluated function" (Left resultObj)
      return resultObj
    _ -> do
      logStep "Couldn't find bytes in one or both of LHS and RHS" (Left Termination)
      return Termination
  return (result, ())

-- | Like `evaluateDataizationFunChain` but specifically for the built-in functions.
-- This function is not safe. It returns undefined for unknown functions
evaluateBuiltinFunChain :: String -> Object -> EvaluationState -> DataizeChain (Object, EvaluationState)
evaluateBuiltinFunChain "Plus" obj = evaluateDataizationFunChain (+) obj
evaluateBuiltinFunChain "Times" obj = evaluateDataizationFunChain (*) obj
evaluateBuiltinFunChain "Package" (Formation bindings) = do
  \_state -> do
    let (packageBindings, restBindings) = span isPackage bindings
    bs <- mapM dataizeBindingChain restBindings
    logStep "Dataized 'Package' siblings" (Left $ Formation (bs ++ packageBindings))
    return (Formation (bs ++ packageBindings), ())
 where
  isPackage (LambdaBinding (Function "Package")) = True
  isPackage _ = False
  dataizeBindingChain (AlphaBinding attr o) = do
    dataizationResult <- dataizeRecursivelyChain o
    return (AlphaBinding attr (either id (Formation . singleton . DeltaBinding) dataizationResult))
  dataizeBindingChain b = Chain $ const ([], b)
evaluateBuiltinFunChain _ obj = const (Chain $ const (undefined, (obj, ())))

-- | Given normalization context, a function on data (bytes interpreted as integers), an object,
-- and the current state of evaluation, returns the new object and a possibly modified state.
evaluateDataizationFun :: Context -> (Int -> Int -> Int) -> Object -> EvaluationState -> (Object, EvaluationState)
evaluateDataizationFun ctx func obj _state = (result, ())
 where
  lhs = dataizeRecursively ctx (ObjectDispatch obj Rho)
  rhs = dataizeRecursively ctx (ObjectDispatch obj (Alpha (AlphaIndex "α0")))
  result = case (lhs, rhs) of
    (Right l, Right r) -> let (Bytes bytes) = intToBytes (bytesToInt r `func` bytesToInt l) in [i|Φ.org.eolang.float(Δ ⤍ #{bytes})|]
    _ -> Termination

-- | Like `evaluateDataizationFun` but specifically for the built-in functions.
-- This function is not safe. It returns undefined for unknown functions
evaluateBuiltinFun :: Context -> String -> Object -> EvaluationState -> (Object, EvaluationState)
evaluateBuiltinFun ctx "Plus" obj = evaluateDataizationFun ctx (+) obj
evaluateBuiltinFun ctx "Times" obj = evaluateDataizationFun ctx (*) obj
evaluateBuiltinFun ctx "Package" obj = (result,)
 where
  (Formation bindings) = obj
  isPackage (LambdaBinding (Function "Package")) = True
  isPackage _ = False
  (packageBindings, restBindings) = span isPackage bindings
  dataizeBinding (AlphaBinding attr o) = AlphaBinding attr (either id (Formation . singleton . DeltaBinding) (dataizeRecursively ctx o))
  dataizeBinding b = b
  result = Formation (map dataizeBinding restBindings ++ packageBindings)
evaluateBuiltinFun _ _ _ = const (undefined, ())
