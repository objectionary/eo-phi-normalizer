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

module Language.EO.Phi.Rules.Fast where

import Data.List.NonEmpty qualified as NonEmpty
import Language.EO.Phi.Rules.Common
import Language.EO.Phi.Rules.Yaml qualified as Yaml
import Language.EO.Phi.Syntax

-- $setup
-- >>> :set -XOverloadedStrings

withBinding :: (Context -> Object -> Object) -> Context -> Binding -> Binding
withBinding f ctx = \case
  AlphaBinding Rho obj -> AlphaBinding Rho obj -- do not apply f inside ρ-bindings
  AlphaBinding a obj -> AlphaBinding a (f ctx{currentAttr = a} obj)
  binding -> binding

isLambdaBinding :: Binding -> Bool
isLambdaBinding LambdaBinding{} = True
isLambdaBinding _ = False

withSubObjects :: (Context -> Object -> Object) -> Context -> Object -> Object
withSubObjects f ctx = go
 where
  go = \case
    root@(Formation bindings)
      | not (any isEmptyBinding bindings) && not (any isLambdaBinding bindings) ->
          let extendedContext = (extendContextWith root ctx){insideFormation = True}
           in Formation
                [ withBinding f extendedContext binding
                | binding <- bindings
                ]
    Application obj bindings ->
      Application
        (f ctx obj)
        [ withBinding f ctx binding
        | binding <- bindings
        ]
    ObjectDispatch obj a -> ObjectDispatch (f ctx obj) a
    obj -> obj

-- | Normalize an object, following a version of call-by-value strategy:
--
-- 1. Apply rules in subobjects/subterms before applying a rule at root.
-- 2. Do not apply rules under formations with at least one void (empty) binding.
--
-- > runWithYegorRules applyRulesInsideOut "⟦ x ↦ ⟦⟧, y ↦ ⟦ z ↦ ⟦ w ↦ ξ.ρ.ρ.x ⟧ ⟧ ⟧.y.z.w"
-- ⟦ ρ ↦ ⟦ ρ ↦ ⟦ ⟧ ⟧ ⟧
applyRulesInsideOut :: Context -> Object -> Object
applyRulesInsideOut ctx obj = do
  let obj' = withSubObjects applyRulesInsideOut ctx obj
  case applyOneRuleAtRoot ctx obj' of
    [] ->
      -- trace ("No rule can be applied to object\n   " <> printTree obj') $
      obj'
    (_ruleName, obj'') : _ ->
      -- trace (ruleName <> ": \n   " <> printTree obj' <> "\n → " <> printTree obj'') $
      applyRulesInsideOut ctx obj''

fastYegorInsideOutAsRule :: NamedRule
fastYegorInsideOutAsRule = ("Yegor's rules (hardcoded)", \ctx obj -> [fastYegorInsideOut ctx obj])

fastYegorInsideOutBinding :: Context -> Binding -> Binding
fastYegorInsideOutBinding ctx (AlphaBinding a obj) = AlphaBinding a (fastYegorInsideOut ctx obj)
fastYegorInsideOutBinding _ binding = binding

fastYegorInsideOut :: Context -> Object -> Object
fastYegorInsideOut ctx = \case
  root | insideSubObject ctx -> root -- this rule is only applied at root
  root@GlobalObject
    | not (insideFormation ctx) ->
        NonEmpty.last (outerFormations ctx)
    | otherwise -> root
  root@ThisObject
    | not (insideFormation ctx) ->
        NonEmpty.head (outerFormations ctx)
    | otherwise -> root
  ObjectDispatch obj a ->
    case fastYegorInsideOut ctx obj of
      this@(Formation bindings) ->
        case lookupBinding a bindings of
          Just objA -> fastYegorInsideOut ctx (Yaml.substThis this objA)
          Nothing ->
            case lookupBinding Phi bindings of
              Just objPhi -> fastYegorInsideOut ctx (ObjectDispatch (Yaml.substThis this objPhi) a)
              Nothing
                | not (any isLambdaBinding bindings) -> Termination
                | otherwise -> ObjectDispatch this a
      this -> ObjectDispatch this a
  Application obj argBindings ->
    case fastYegorInsideOut ctx obj of
      obj'@(Formation bindings) -> do
        let argBindings' = map (fastYegorInsideOutBinding ctx) argBindings
        case argBindings' of
          [AlphaBinding (Alpha "α0") arg0, AlphaBinding (Alpha "α1") arg1, AlphaBinding (Alpha "α2") arg2] ->
            case filter isEmptyBinding bindings of
              EmptyBinding a0 : EmptyBinding a1 : EmptyBinding a2 : _ ->
                Formation
                  ( AlphaBinding a0 arg0
                      : AlphaBinding a1 arg1
                      : AlphaBinding a2 arg2
                      : [ binding
                        | binding <- bindings
                        , case binding of
                            EmptyBinding x | x `elem` [a0, a1, a2] -> False
                            _ -> True
                        ]
                  )
              _
                | not (any isLambdaBinding bindings) -> Termination
                | otherwise -> Application obj' argBindings'
          [AlphaBinding (Alpha "α0") arg0, AlphaBinding (Alpha "α1") arg1] ->
            case filter isEmptyBinding bindings of
              EmptyBinding a0 : EmptyBinding a1 : _ ->
                Formation
                  ( AlphaBinding a0 arg0
                      : AlphaBinding a1 arg1
                      : [ binding
                        | binding <- bindings
                        , case binding of
                            EmptyBinding x | x `elem` [a0, a1] -> False
                            _ -> True
                        ]
                  )
              _
                | not (any isLambdaBinding bindings) -> Termination
                | otherwise -> Application obj' argBindings'
          [AlphaBinding (Alpha "α0") arg0] ->
            case filter isEmptyBinding bindings of
              EmptyBinding a0 : _ ->
                Formation
                  ( AlphaBinding a0 arg0
                      : [ binding
                        | binding <- bindings
                        , case binding of
                            EmptyBinding x | x == a0 -> False
                            _ -> True
                        ]
                  )
              _
                | not (any isLambdaBinding bindings) -> Termination
                | otherwise -> Application obj' argBindings'
          [AlphaBinding a argA]
            | EmptyBinding a `elem` bindings ->
                Formation
                  ( AlphaBinding a argA
                      : [ binding
                        | binding <- bindings
                        , case binding of
                            EmptyBinding x | x == a -> False
                            _ -> True
                        ]
                  )
            | not (any isLambdaBinding bindings) -> Termination
          [DeltaBinding bytes]
            | DeltaEmptyBinding `elem` bindings -> do
                Formation
                  ( DeltaBinding bytes
                      : [ binding
                        | binding <- bindings
                        , case binding of
                            DeltaEmptyBinding -> False
                            _ -> True
                        ]
                  )
            | not (any isLambdaBinding bindings) -> Termination
          _ -> Application obj' argBindings'
      obj' -> Application obj' (map (fastYegorInsideOutBinding ctx) argBindings)
  root@(Formation bindings)
    | any isEmptyBinding bindings || any isLambdaBinding bindings -> root
    | otherwise ->
        Formation
          [ binding'
          | binding <- bindings
          , let binding' =
                  case binding of
                    AlphaBinding Rho _ -> binding
                    AlphaBinding a objA -> do
                      let ctx' = (extendContextWith root ctx){insideFormation = True, currentAttr = a}
                      AlphaBinding a (fastYegorInsideOut ctx' objA)
                    _ -> binding
          ]
  -- TODO #617:30m Should this be error?
  obj@GlobalObjectPhiOrg -> fastYegorInsideOut ctx (desugar obj)
  Termination -> Termination
  MetaSubstThis{} -> error "impossible MetaSubstThis!"
  MetaContextualize{} -> error "impossible MetaContextualize!"
  MetaObject{} -> error "impossible MetaObject!"
  MetaTailContext{} -> error "impossible MetaTailContext!"
  MetaFunction{} -> error "impossible MetaFunction!"
  obj@ConstString{} -> obj -- fastYegorInsideOut ctx (desugar obj)
  obj@ConstInt{} -> obj -- fastYegorInsideOut ctx (desugar obj)
  obj@ConstIntRaw{} -> obj -- fastYegorInsideOut ctx (desugar obj)
  obj@ConstFloat{} -> obj -- fastYegorInsideOut ctx (desugar obj)
  obj@ConstFloatRaw{} -> obj -- fastYegorInsideOut ctx (desugar obj)
