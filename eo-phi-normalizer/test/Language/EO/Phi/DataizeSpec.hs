{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Language.EO.Phi.DataizeSpec where

import Control.Monad (forM_)
import Test.Hspec

import Language.EO.Phi qualified as Phi
import Language.EO.Phi.Dataize (dataizeRecursively)
import Language.EO.Phi.Rules.Common (defaultContext)
import Language.EO.Phi.Rules.Yaml (convertRuleNamed, parseRuleSetFromFile, rules)
import Test.EO.Phi (DataizeTest (..), DataizeTestGroup (..), dataizationTests)

spec :: Spec
spec = do
  DataizeTestGroup{..} <- runIO (dataizationTests "test/eo/phi/dataization.yaml")
  ruleset <- runIO $ parseRuleSetFromFile "test/eo/phi/rules/yegor.yaml"
  let rules = map convertRuleNamed ruleset.rules
  describe title $
    forM_ tests $
      \test -> do
        let inputObj = progToObj test.input
        it test.name $ dataizeRecursively (defaultContext rules inputObj) inputObj `shouldBe` Right test.output

progToObj :: Phi.Program -> Phi.Object
progToObj (Phi.Program bindings) = Phi.Formation bindings
