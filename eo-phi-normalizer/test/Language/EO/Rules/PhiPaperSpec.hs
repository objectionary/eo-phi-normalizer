{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.EO.Rules.PhiPaperSpec where

import Data.Data (Data (toConstr))
import Data.List (intercalate)
import Data.List qualified as List
import Language.EO.Phi.Rules.Common (Context (Context), Rule, applyOneRule, equalObject, intToBytes)
import Language.EO.Phi.Rules.Yaml (convertRule, parseRuleSetFromFile, rules)
import Language.EO.Phi.Syntax (printTree)
import Language.EO.Phi.Syntax.Abs as Phi
import Test.Hspec
import Test.QuickCheck

arbitraryNonEmptyString :: Gen String
arbitraryNonEmptyString = listOf1 (elements ['a' .. 'z'])

instance Arbitrary Attribute where
  arbitrary =
    oneof
      [ pure Phi
      , pure Rho
      , pure Sigma
      , pure VTX
      , Label <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary LabelId where
  arbitrary = LabelId <$> arbitraryNonEmptyString
  shrink = genericShrink
instance Arbitrary AlphaIndex where
  arbitrary = AlphaIndex <$> arbitraryNonEmptyString
  shrink = genericShrink
instance Arbitrary Bytes where
  arbitrary = intToBytes <$> arbitrarySizedNatural
  shrink = genericShrink
instance Arbitrary Phi.Function where
  arbitrary = Phi.Function <$> arbitraryNonEmptyString
  shrink = genericShrink
instance Arbitrary Phi.MetaId where
  arbitrary = Phi.MetaId . ("!" ++) <$> arbitraryNonEmptyString
  shrink = genericShrink
instance Arbitrary Phi.MetaFunctionName where
  arbitrary = Phi.MetaFunctionName . ("@" ++) <$> arbitraryNonEmptyString
  shrink = genericShrink

instance Arbitrary Binding where
  arbitrary =
    oneof
      [ EmptyBinding <$> arbitrary
      , AlphaBinding <$> arbitrary <*> arbitrary
      , DeltaBinding <$> arbitrary
      , LambdaBinding <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary Object where
  arbitrary = sized $ \n -> do
    let arbitraryBinding = resize (n `div` 2) arbitrary
    let arbitraryAttr = resize (n `div` 2) arbitrary
    let arbitraryObj = resize (n `div` 2) arbitrary
    let sameAttr (AlphaBinding attr1 _) (AlphaBinding attr2 _) = attr1 == attr2
        sameAttr (EmptyBinding attr1) (EmptyBinding attr2) = attr1 == attr2
        sameAttr b1 b2 = toConstr b1 == toConstr b2
    let arbitraryBindings = List.nubBy sameAttr <$> listOf arbitraryBinding
    if n > 0
      then
        oneof
          [ Formation <$> arbitraryBindings
          , liftA2 Application arbitraryObj arbitraryBindings
          , liftA2 ObjectDispatch arbitraryObj arbitraryAttr
          , ObjectDispatch GlobalObject <$> arbitraryAttr
          , pure ThisObject
          , pure Termination
          ]
      else pure $ Formation []
  shrink = genericShrink

data CriticalPair = CriticalPair
  { sourceTerm :: Object
  , criticalPair :: (Object, Object)
  -- add rules that were used to get the pair
  }

genCriticalPair :: [Rule] -> Gen CriticalPair
genCriticalPair rules = do
  (sourceTerm, results) <- fan `suchThat` \(_, rs) -> length rs > 1
  case results of
    x : y : _ ->
      return
        CriticalPair
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
  , x : y : _ <- [applyOneRule (Context rules [sourceTerm']) sourceTerm']
  ]

descendantsN :: Int -> [Rule] -> [Object] -> [Object]
descendantsN maxDepth rules objs
  | maxDepth <= 0 = objs
  | otherwise =
      objs
        ++ descendantsN
          (maxDepth - 1)
          rules
          [ obj'
          | obj <- objs
          , obj' <- applyOneRule (Context rules [obj]) obj
          ]

confluentCriticalPairN :: Int -> [Rule] -> CriticalPair -> Bool
confluentCriticalPairN maxDepth rules CriticalPair{..} =
  -- should normalize the VTXs before checking
  not (null (List.intersectBy equalObject (descendantsN maxDepth rules [x]) (descendantsN maxDepth rules [y])))
 where
  (x, y) = criticalPair

instance Show CriticalPair where
  show CriticalPair{criticalPair = (x, y), ..} =
    intercalate
      "\n"
      [ "Source term:"
      , "  " <> printTree sourceTerm
      , "Critical pair:"
      , "  " <> printTree x
      , "  " <> printTree y
      ]

confluent :: [Rule] -> Property
confluent rulesFromYaml =
  within 10_000_000 $
    forAllShrink (genCriticalPair rulesFromYaml) (shrinkCriticalPair rulesFromYaml) $
      confluentCriticalPairN 7 rulesFromYaml

spec :: Spec
spec = do
  ruleset <- runIO $ parseRuleSetFromFile "./test/eo/phi/rules/yegor.yaml"
  let rulesFromYaml = map convertRule (rules ruleset)
  describe "Yegor's rules" $
    it "Are confluent (via QuickCheck)" $
      confluent rulesFromYaml
