{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.EO.Phi.Rules.Fast where

import Data.List.NonEmpty qualified as NonEmpty
import Language.EO.Phi.Rules.Common
import Language.EO.Phi.Rules.Yaml qualified as Yaml
import Language.EO.Phi.Syntax (printTree)
import Language.EO.Phi.Syntax.Abs
import System.IO.Unsafe (unsafePerformIO)

-- $setup
-- >>> :set -XOverloadedStrings

runWithYegorRules :: (Context -> Object -> Object) -> Object -> IO ()
runWithYegorRules f obj = putStrLn (printTree (f (defaultContext yegorRules obj) obj))

yegorRuleSet :: Yaml.RuleSet
{-# NOINLINE yegorRuleSet #-}
yegorRuleSet =
  unsafePerformIO $
    Yaml.parseRuleSetFromFile "eo-phi-normalizer/test/eo/phi/rules/yegor.yaml"

yegorRules :: [NamedRule]
yegorRules = map Yaml.convertRuleNamed (Yaml.rules yegorRuleSet)

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
              Nothing -> ObjectDispatch this a
      this -> ObjectDispatch this a
  Application obj argBindings ->
    case fastYegorInsideOut ctx obj of
      obj'@(Formation bindings) -> do
        let argBindings' = map (fastYegorInsideOutBinding ctx) argBindings
        case argBindings' of
          [AlphaBinding (Alpha "α0") arg0, AlphaBinding (Alpha "α1") arg1] ->
            case filter isEmptyBinding bindings of
              EmptyBinding a0 : EmptyBinding a1 : _ -> do
                let arg0' = fastYegorInsideOut ctx arg0
                let arg1' = fastYegorInsideOut ctx arg1
                Formation
                  ( AlphaBinding a0 arg0'
                      : AlphaBinding a1 arg1'
                      : [ binding
                        | binding <- bindings
                        , case binding of
                            EmptyBinding x | x `elem` [a0, a1] -> False
                            _ -> True
                        ]
                  )
              _ -> Application obj' argBindings'
          [AlphaBinding (Alpha "α0") arg0] ->
            case filter isEmptyBinding bindings of
              EmptyBinding a0 : _ -> do
                let arg0' = fastYegorInsideOut ctx arg0
                Formation
                  ( AlphaBinding a0 arg0'
                      : [ binding
                        | binding <- bindings
                        , case binding of
                            EmptyBinding x | x == a0 -> False
                            _ -> True
                        ]
                  )
              _ -> Application obj' argBindings'
          [AlphaBinding a argA] | EmptyBinding a `elem` bindings -> do
            let argA' = fastYegorInsideOut ctx argA
            Formation
              ( AlphaBinding a argA'
                  : [ binding
                    | binding <- bindings
                    , case binding of
                        EmptyBinding x | x == a -> False
                        _ -> True
                    ]
              )
          [DeltaBinding bytes] | DeltaEmptyBinding `elem` bindings -> do
            Formation
              ( DeltaBinding bytes
                  : [ binding
                    | binding <- bindings
                    , case binding of
                        DeltaEmptyBinding -> False
                        _ -> True
                    ]
              )
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
  Termination -> Termination
  MetaSubstThis{} -> error "impossible MetaSubstThis!"
  MetaObject{} -> error "impossible MetaObject!"
  MetaFunction{} -> error "impossible MetaFunction!"
