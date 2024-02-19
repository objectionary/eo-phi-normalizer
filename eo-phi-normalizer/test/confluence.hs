{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.List (intercalate)
import qualified Data.List as List
import Language.EO.Phi.Rules.Common (Context (Context), Rule, applyRules, intToBytes, applyOneRule)
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

data CriticalPair = CriticalPair
  { sourceTerm :: Object
  , criticalPair :: (Object, Object)
  -- add rules that were used to get the pair
  }

genCriticalPair :: [Rule] -> Gen CriticalPair
genCriticalPair rules = do
  (sourceTerm, results) <- fan `suchThat` \(_, rs) -> length rs > 1
  case results of
    x:y:_ -> return CriticalPair
      { sourceTerm = sourceTerm
      , criticalPair = (x, y)
      }
    _ -> error "IMPOSSIBLE HAPPENED"
  where
    fan = do
      obj <- Formation <$> listOf arbitrary
      return (obj, applyOneRule (Context rules [obj]) obj)

shrinkCriticalPair :: [Rule] -> CriticalPair -> [CriticalPair]
shrinkCriticalPair rules CriticalPair{..} =
  [ CriticalPair
      { sourceTerm = sourceTerm'
      , criticalPair = (x, y)
      }
  | sourceTerm'@Formation{} <- shrink sourceTerm
  , x:y:_ <- [applyOneRule (Context rules [sourceTerm']) sourceTerm']
  ]

descendantsN :: Int -> [Rule] -> [Object] -> [Object]
descendantsN maxDepth rules objs
  | maxDepth <= 0 = objs
  | otherwise = objs ++ descendantsN (maxDepth - 1) rules
      [ obj'
      | obj <- objs
      , obj' <- applyOneRule (Context rules [obj]) obj ]

confluentCriticalPairN :: Int -> [Rule] -> CriticalPair -> Bool
confluentCriticalPairN maxDepth rules CriticalPair{..} =
  not (null (descendantsN maxDepth rules [x] `List.intersect` descendantsN maxDepth rules [y]))
  where
    (x, y) = criticalPair

instance Show CriticalPair where
  show CriticalPair{criticalPair = (x, y), ..} = intercalate "\n"
    [ "Source term:"
    , "  " <> printTree sourceTerm
    , "Critical pair:"
    , "  " <> printTree x
    , "  " <> printTree y
    ]

main :: IO ()
main = do
  ruleset <- parseRuleSetFromFile "./test/eo/phi/rules/yegor.yaml"
  let rulesFromYaml = map convertRule (rules ruleset)
  let genPrettyObject = do
        obj <- arbitrary
        return (PrettyObject (Context rulesFromYaml [obj]) obj)
      shrinkPrettyObject (PrettyObject ctx obj) = PrettyObject ctx <$> shrink obj
  quickCheck $
    within 100_000 $
      forAllShrink (genCriticalPair rulesFromYaml) (shrinkCriticalPair rulesFromYaml) $
        confluentCriticalPairN 10 rulesFromYaml
  -- quickCheck $ within 10_000_000 (forAllShrink genPrettyObject shrinkPrettyObject $ \(PrettyObject _ obj) -> confluence rulesFromYaml obj)
