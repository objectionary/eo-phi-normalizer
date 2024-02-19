{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.List (intercalate)
import Language.EO.Phi.Rules.Common (Context (Context), Rule, applyRules, intToBytes)
import Language.EO.Phi.Rules.Yaml (convertRule, parseRuleSetFromFile, rules)
import Language.EO.Phi.Syntax (printTree)
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
  shrink = genericShrink

instance Arbitrary LabelId where
  arbitrary = LabelId <$> arbitrary
instance Arbitrary AlphaIndex where
  arbitrary = AlphaIndex <$> arbitrary
instance Arbitrary Bytes where
  arbitrary = Bytes <$> arbitrary
instance Arbitrary Phi.Function where
  arbitrary = Phi.Function <$> listOf1 (elements ['a' .. 'z'])
instance Arbitrary Phi.MetaId where
  arbitrary = Phi.MetaId <$> arbitrary
instance Arbitrary Phi.MetaFunctionName where
  arbitrary = Phi.MetaFunctionName <$> arbitrary

instance Arbitrary Binding where
  arbitrary =
    oneof
      [ EmptyBinding <$> arbitrary
      , AlphaBinding <$> arbitrary <*> arbitrary
      , DeltaBinding . intToBytes <$> arbitrarySizedNatural
      , LambdaBinding <$> arbitrary
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
          , ObjectDispatch GlobalObject <$> arbitraryAttr
          , pure ThisObject
          , pure Termination
          ]
      else pure $ Formation []
  shrink = genericShrink

confluence :: [Rule] -> Object -> Bool
confluence originalRules input = length (applyRules ctx input) == 1
 where
  ctx = Context originalRules [input]

data PrettyObject = PrettyObject Context Object

instance Show PrettyObject where
  show (PrettyObject ctx obj) = intercalate "\n" (map printTree (obj : applyRules ctx obj))

main :: IO ()
main = do
  ruleset <- parseRuleSetFromFile "./test/eo/phi/rules/yegor.yaml"
  let rulesFromYaml = map convertRule (rules ruleset)
  let genPrettyObject = do
        obj <- arbitrary
        return (PrettyObject (Context rulesFromYaml [obj]) obj)
      shrinkPrettyObject (PrettyObject ctx obj) = PrettyObject ctx <$> shrink obj
  quickCheck $ within 10_000_000 (forAllShrink genPrettyObject shrinkPrettyObject $ \(PrettyObject _ obj) -> confluence rulesFromYaml obj)
