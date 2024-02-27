{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Language.EO.YamlSpec where

import Control.Monad (forM_)
import Language.EO.Phi.Rules.Common (Context (..), equalObject)
import Language.EO.Phi.Rules.Yaml (Rule (..), RuleSet (..), RuleTest (..), convertRule)
import Language.EO.Phi.Syntax.Abs (Attribute (Sigma))
import Test.EO.Yaml
import Test.Hspec

spec :: Spec
spec = describe "User-defined rules unit tests" do
  ruleset <- runIO $ fileTests "test/eo/phi/rules/yegor.yaml"
  describe ruleset.title do
    forM_ ruleset.rules $ \rule -> do
      describe rule.name do
        forM_ rule.tests $ \ruleTest -> do
          it ruleTest.name $
            let rule' = convertRule rule
                normalized = rule' (Context [rule'] [ruleTest.input] Sigma) ruleTest.input
                expected = ruleTest.output
                sameObjs objs1 objs2 = and ((length objs1 == length objs2) : zipWith equalObject objs2 objs1)
             in normalized `shouldSatisfy` sameObjs expected
