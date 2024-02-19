{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.List (permutations)
import Language.EO.Phi.Rules.Common (Context (Context), Rule, applyRules, intToBytes)
import Language.EO.Phi.Rules.Yaml (convertRule, parseRuleSetFromFile, rules)
import Language.EO.Phi.Syntax.Abs as Phi
import Test.QuickCheck

instance Arbitrary Attribute where
  arbitrary =
    oneof
      [ pure Phi
      , pure Rho
      , pure Sigma
      , pure VTX
      , Label . LabelId <$> listOf1 (elements ['a' .. 'z'])
      ]

instance Arbitrary Bytes where
  arbitrary = Bytes <$> arbitrary
instance Arbitrary Phi.Function where
  arbitrary = Phi.Function <$> arbitrary
instance Arbitrary Phi.MetaId where
  arbitrary = Phi.MetaId <$> arbitrary
instance Arbitrary Phi.MetaFunctionName where
  arbitrary = Phi.MetaFunctionName <$> arbitrary

instance Arbitrary Binding where
  arbitrary =
    oneof
      [ EmptyBinding <$> arbitrary
      , liftA2 AlphaBinding arbitrary arbitrary
      , DeltaBinding . intToBytes <$> arbitrarySizedNatural
      , LambdaBinding . Function <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary Object where
  arbitrary = sized $ \n -> do
    let arbitraryBinding = resize (n `div` 2) arbitrary
    let arbitraryAttr = resize (n `div` 2) arbitrary
    let arbitraryObj = resize (n `div` 2) arbitrary
    if n > 0
      then
        oneof
          [ Formation <$> listOf arbitraryBinding
          , liftA2 Application arbitraryObj (listOf arbitraryBinding)
          , liftA2 ObjectDispatch arbitraryObj arbitraryAttr
          , pure GlobalObject
          , pure ThisObject
          , pure Termination
          ]
      else pure $ Formation []
  shrink = genericShrink

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
