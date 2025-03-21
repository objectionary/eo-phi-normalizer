{- FOURMOLU_DISABLE -}
-- The MIT License (MIT)

-- Copyright (c) 2016-2025 Objectionary.com

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
{- FOURMOLU_ENABLE -}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Language.EO.Test.YamlSpec where

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Language.EO.Phi.Dataize.Context (defaultContext)
import Language.EO.Phi.Rules.Common (applyOneRule)
import Language.EO.Phi.Rules.Yaml (Rule (..), RuleSet (..), RuleTest (..), RuleTestOption (..), convertRuleNamed)
import Language.EO.Test.Yaml
import Test.Hspec

spec :: [FilePath] -> Spec
spec testPaths = describe "User-defined rules unit tests" do
  forM_ testPaths $ \path -> do
    ruleset <- runIO $ fileTests path
    describe ruleset.title do
      forM_ ruleset.rules $ \rule -> do
        describe rule.name do
          let tests' = fromMaybe [] rule.tests
          forM_ tests' $ \ruleTest -> do
            it ruleTest.name $
              let rule' = convertRuleNamed rule
                  resultOneStep = applyOneRule (defaultContext [rule'] ruleTest.input) ruleTest.input
                  normalizationResult = maybe resultOneStep (\lst -> if TakeOne True `elem` lst then take 1 resultOneStep else resultOneStep) ruleTest.options
                  expected = ruleTest.output
               in map snd normalizationResult `shouldBe` expected
