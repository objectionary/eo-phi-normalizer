{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Language.EO.Phi.Rules.Fast where

-- import Debug.Trace (trace)

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
