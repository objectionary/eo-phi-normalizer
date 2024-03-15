{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Language.EO.YamlSpec where

import Control.Monad (forM_)
import Language.EO.Phi.Rules.Common (applyOneRule, defaultContext, equalObject)
import Language.EO.Phi.Rules.Yaml (Rule (..), RuleSet (..), RuleTest (..), convertRuleNamed)
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
            let rule' = convertRuleNamed rule
                resultOneStep = applyOneRule (defaultContext [rule'] ruleTest.input) ruleTest.input
                expected = ruleTest.output
                sameObjs objs1 objs2 = and ((length objs1 == length objs2) : zipWith equalObject objs2 objs1)
             in resultOneStep `shouldSatisfy` sameObjs expected
