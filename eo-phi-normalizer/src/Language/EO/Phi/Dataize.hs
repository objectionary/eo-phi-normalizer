{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Language.EO.Phi.Dataize (dataizeStep, dataizeRecursively, dataizeStepChain, dataizeRecursivelyChain) where

import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.String.Interpolate (i)
import Language.EO.Phi (printTree)
import Language.EO.Phi.Rules.Common (Context, applyRules, applyRulesChain, bytesToInt, extendContextWith, intToBytes)
import Language.EO.Phi.Syntax.Abs (
  AlphaIndex (AlphaIndex),
  Attribute (Alpha, Phi, Rho),
  Binding (AlphaBinding, DeltaBinding, LambdaBinding),
  Bytes (Bytes),
  Function (Function),
  Object (Application, Formation, ObjectDispatch, Termination),
 )

-- | Perform one step of dataization to the object (if possible).
dataizeStep :: Context -> Object -> Either Object Bytes
dataizeStep ctx obj@(Formation bs)
  | Just (DeltaBinding bytes) <- find isDelta bs = Right bytes
  | Just (LambdaBinding (Function funcName)) <- find isLambda bs = Left (fst $ evaluateBuiltinFun ctx funcName obj ())
  | Just (AlphaBinding Phi decoratee) <- find isPhi bs = dataizeStep (extendContextWith obj ctx) decoratee
  | otherwise = Left obj
 where
  isDelta (DeltaBinding _) = True
  isDelta _ = False
  isLambda (LambdaBinding _) = True
  isLambda _ = False
  isPhi (AlphaBinding Phi _) = True
  isPhi _ = False
dataizeStep ctx (Application obj bindings) = case dataizeStep ctx obj of
  Left dataized -> Left $ Application dataized bindings
  Right _ -> error ("Application on bytes upon dataizing:\n  " <> printTree obj)
dataizeStep ctx (ObjectDispatch obj attr) = case dataizeStep ctx obj of
  Left dataized -> Left $ ObjectDispatch dataized attr
  Right _ -> error ("Dispatch on bytes upon dataizing:\n  " <> printTree obj)
dataizeStep _ obj = Left obj

-- | State of evaluation is not needed yet, but it might be in the future
type EvaluationState = ()

-- | Recursively perform normalization and dataization until we get bytes in the end.
dataizeRecursively :: Context -> Object -> Either Object Bytes
dataizeRecursively ctx obj = case applyRules ctx obj of
  [normObj] -> case dataizeStep ctx normObj of
    Left stillObj
      | stillObj == normObj -> Left stillObj -- dataization changed nothing
      | otherwise -> dataizeRecursively ctx stillObj -- partially dataized
    Right bytes -> Right bytes
  objs -> error errMsg -- Left Termination
   where
    errMsg =
      "Expected 1 result from normalization but got "
        <> show (length objs)
        <> ":\n"
        <> unlines (map (("  - " ++) . printTree) objs)
        <> "\nFor the input:\n  "
        <> printTree obj

-- | Perform one step of dataization to the object (if possible), reporting back individiual steps.
dataizeStepChain :: Context -> Object -> [(String, Either Object Bytes)]
dataizeStepChain ctx obj@(Formation bs)
  | Just (DeltaBinding bytes) <- listToMaybe [b | b@(DeltaBinding _) <- bs] = [("Found bytes", Right bytes)]
  | Just (LambdaBinding (Function funcName)) <- listToMaybe [b | b@(LambdaBinding _) <- bs] =
      let evaluationChain = evaluateBuiltinFunChain ctx funcName obj ()
       in ("Evaluating lambda '" <> funcName <> "'", Left obj) : map (fmap Left . fst) evaluationChain
  | Just (AlphaBinding Phi decoratee) <- listToMaybe [b | b@(AlphaBinding Phi _) <- bs] =
      ("Dataizing inside phi", Left decoratee) : dataizeStepChain (extendContextWith obj ctx) decoratee
  | otherwise = [("No change to formation", Left obj)]
dataizeStepChain ctx (Application obj _bindings) = ("Dataizing inside application", Left obj) : dataizeStepChain ctx obj
dataizeStepChain ctx (ObjectDispatch obj _attr) = ("Dataizing inside dispatch", Left obj) : dataizeStepChain ctx obj
dataizeStepChain _ obj = [("Nothing to dataize", Left obj)]

-- | Recursively perform normalization and dataization until we get bytes in the end,
-- reporting intermediate steps
dataizeRecursivelyChain :: Context -> Object -> [(String, Either Object Bytes)]
dataizeRecursivelyChain ctx obj = case applyRulesChain ctx obj of
  [[]] -> [("No rules applied", Left obj)]
  [chain] ->
    let (_lastRule, normObj) = last chain
     in map (fmap Left) chain
          ++ case head (dataizeStepChain ctx normObj) of
            res@(_, Left stillObj)
              | stillObj == normObj -> [res] -- dataization changed nothing
              | otherwise -> res : dataizeRecursivelyChain ctx stillObj -- partially dataized
            bytes -> [bytes]
  chains -> [(errMsg, Left Termination)]
   where
    errMsg =
      "Expected 1 chain from normalization but got "
        <> show (length chains)
        <> ":\n"
        <> unlines (map (unlines . map (\(name, object) -> "  - " ++ name ++ ": " ++ printTree object)) chains)
        <> "\nFor the input:\n  "
        <> printTree obj

-- | Given normalization context, a function on data (bytes interpreted as integers), an object,
-- and the current state of evaluation, returns the new object and a possibly modified state along with intermediate steps.
evaluateDataizationFunChain :: Context -> (Int -> Int -> Int) -> Object -> EvaluationState -> [((String, Object), EvaluationState)]
evaluateDataizationFunChain ctx func obj _state = map (,()) result
 where
  lhs = let o_rho = ObjectDispatch obj Rho in ("Evaluating LHS", Left o_rho) : dataizeRecursivelyChain ctx o_rho
  rhs = let o_a0 = ObjectDispatch obj (Alpha (AlphaIndex "α0")) in ("Evaluating RHS", Left o_a0) : dataizeRecursivelyChain ctx o_a0
  lhsBytes = [(msg, bytes) | (msg, Right bytes) <- lhs]
  rhsBytes = [(msg, bytes) | (msg, Right bytes) <- rhs]
  lhsObjects = [(msg, object) | (msg, Left object) <- lhs]
  rhsObjects = [(msg, object) | (msg, Left object) <- rhs]
  result = case (lhsBytes, rhsBytes) of
    ([(_, l)], [(_, r)]) ->
      let (Bytes bytes) = intToBytes (bytesToInt r `func` bytesToInt l)
       in lhsObjects
            ++ rhsObjects
            ++ [("Evaluated function", [i|Φ.org.eolang.float(Δ ⤍ #{bytes})|])]
    _ -> lhsObjects ++ rhsObjects ++ [("Couldn't find bytes in one or both of LHS and RHS", Termination)]

-- | Like `evaluateDataizationFunChain` but specifically for the built-in functions.
-- This function is not safe. It returns undefined for unknown functions
evaluateBuiltinFunChain :: Context -> String -> Object -> EvaluationState -> [((String, Object), EvaluationState)]
evaluateBuiltinFunChain ctx "Plus" = evaluateDataizationFunChain ctx (+)
evaluateBuiltinFunChain ctx "Times" = evaluateDataizationFunChain ctx (*)
evaluateBuiltinFunChain _ _ = const $ const [(undefined, ())]

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
evaluateBuiltinFun ctx "Plus" = evaluateDataizationFun ctx (+)
evaluateBuiltinFun ctx "Times" = evaluateDataizationFun ctx (*)
evaluateBuiltinFun _ _ = const $ const (undefined, ())
