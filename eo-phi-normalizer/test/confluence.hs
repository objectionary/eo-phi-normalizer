{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.List (permutations)
import Language.EO.Phi.Rules.Common (Context (Context), Rule, applyRules, intToBytes)
import Language.EO.Phi.Rules.Yaml (convertRule, parseRuleSetFromFile, rules)
import Language.EO.Phi.Syntax.Abs
import Test.QuickCheck

instance Arbitrary Attribute where
  arbitrary =
    oneof
      [ pure Phi
      , pure Rho
      , pure Sigma
      , pure VTX
      , Label . LabelId <$> listOf (elements ['a' .. 'z'])
      ]

instance Arbitrary Binding where
  arbitrary =
    oneof
      [ EmptyBinding <$> arbitrary
      , liftA2 AlphaBinding arbitrary arbitrary
      , DeltaBinding . intToBytes <$> arbitrary
      , LambdaBinding . Function <$> arbitrary
      ]

instance Arbitrary Object where
  arbitrary =
    oneof
      [ Formation <$> listOf arbitrary
      , liftA2 Application arbitrary (listOf arbitrary)
      , liftA2 ObjectDispatch arbitrary arbitrary
      , pure GlobalObject
      , pure ThisObject
      , pure Termination
      ]

confluence :: [Rule] -> Object -> Property
confluence originalRules input = conjoin $ flip map rulesInAllOrders $ \rulesPermutation -> applyRules (Context rulesPermutation [input]) input === referenceApplication
 where
  referenceApplication = applyRules (Context originalRules [input]) input
  rulesInAllOrders = permutations originalRules

main :: IO ()
main = do
  ruleset <- parseRuleSetFromFile "./test/eo/phi/rules/yegor.yaml"
  let rulesFromYaml = map convertRule (rules ruleset)
  quickCheck (confluence rulesFromYaml)
