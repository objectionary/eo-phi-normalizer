{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.EO.Rules.PhiPaperSpec where

import Control.Monad (forM_, guard)
import Data.Aeson (FromJSON)
import Data.Data (Data (toConstr))
import Data.List (intercalate)
import Data.List qualified as List
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
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
    return (obj, applyOneRule (Context rules [obj] Phi.Sigma) obj)

findCriticalPairs :: [Rule] -> Object -> [CriticalPair]
findCriticalPairs rules obj = do
  let ctx = Context rules [obj] Phi.Sigma
  let results = applyOneRule ctx obj
  guard (length results > 1)
  case results of
    x : y : _ ->
      return
        CriticalPair
          { sourceTerm = obj
          , criticalPair = (x, y)
          }
    _ -> error "IMPOSSIBLE HAPPENED"

shrinkCriticalPair :: [Rule] -> CriticalPair -> [CriticalPair]
shrinkCriticalPair rules CriticalPair{..} =
  [ CriticalPair
    { sourceTerm = sourceTerm'
    , criticalPair = (x, y)
    }
  | sourceTerm'@Formation{} <- shrink sourceTerm
  , x : y : _ <- [applyOneRule (Context rules [sourceTerm'] Phi.Sigma) sourceTerm']
  ]

descendantsN :: Int -> [Rule] -> [Object] -> [Object]
descendantsN depth rules objs
  | depth <= 0 = objs
  | otherwise =
      objs
        ++ descendantsN
          (depth - 1)
          rules
          [ obj'
          | obj <- objs
          , obj' <- applyOneRule (Context rules [obj] Phi.Sigma) obj
          ]

confluentCriticalPairN :: Int -> [Rule] -> CriticalPair -> Bool
confluentCriticalPairN depth rules CriticalPair{..} =
  -- should normalize the VTXs before checking
  not (null (List.intersectBy equalObject (descendantsN depth rules [x]) (descendantsN depth rules [y])))
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

maxDepth :: Int
maxDepth = 7

confluent :: [Rule] -> Property
confluent rulesFromYaml =
  within 10_000_000 $
    forAllShrink (genCriticalPair rulesFromYaml) (shrinkCriticalPair rulesFromYaml) $
      confluentCriticalPairN maxDepth rulesFromYaml

confluentOnObject :: [Rule] -> Object -> Bool
confluentOnObject rules obj = all (confluentCriticalPairN maxDepth rules) (findCriticalPairs rules obj)

data ConfluenceTests = ConfluenceTests
  { title :: String
  , tests :: [Object]
  }
  deriving (Generic, FromJSON, Show)

parseTests :: String -> IO ConfluenceTests
parseTests = Yaml.decodeFileThrow

spec :: Spec
spec = do
  ruleset <- runIO $ parseRuleSetFromFile "./test/eo/phi/rules/yegor.yaml"
  let rulesFromYaml = map convertRule (rules ruleset)
  inputs <- runIO $ parseTests "./test/eo/phi/confluence.yaml"
  describe "Yegor's rules" $ do
    it "Are confluent (via QuickCheck)" (confluent rulesFromYaml)
    describe
      "Are confluent (regression tests)"
      $ forM_ (tests inputs)
      $ \input -> do
        it (printTree input) (input `shouldSatisfy` confluentOnObject rulesFromYaml)
