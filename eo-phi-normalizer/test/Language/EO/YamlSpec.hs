{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Language.EO.YamlSpec where

import Control.Monad (forM_)
import Language.EO.Phi.Rules.Common (Context (..))
import Language.EO.Phi.Rules.Yaml (Rule (..), RuleSet (..), RuleTest (..), convertRule)
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
            convertRule rule (Context [] []) ruleTest.input `shouldBe` [ruleTest.output | ruleTest.matches]
