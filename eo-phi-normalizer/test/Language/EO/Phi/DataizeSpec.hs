{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Language.EO.Phi.DataizeSpec where

import Control.Monad (forM_)
import Test.Hspec

import Language.EO.Phi (printTree)
import Language.EO.Phi qualified as Phi
import Language.EO.Phi.Dataize (dataizeRecursively)
import Language.EO.Phi.Rules.Common (defaultContext, equalObject)
import Language.EO.Phi.Rules.Yaml (convertRuleNamed, parseRuleSetFromFile, rules)
import Test.EO.Phi (DataizationResult (Bytes, Object), DataizeTest (..), DataizeTestGroup (..), dataizationTests)

newtype ObjectOrBytes = ObjectOrBytes (Either Phi.Object Phi.Bytes)

instance Show ObjectOrBytes where
  show (ObjectOrBytes (Left obj)) = printTree obj
  show (ObjectOrBytes (Right bytes)) = printTree bytes

instance Eq ObjectOrBytes where
  ObjectOrBytes (Left x) == ObjectOrBytes (Left y) =
    x `equalObject` y
  ObjectOrBytes (Right x) == ObjectOrBytes (Right y) =
    x == y
  _ == _ = False

spec :: Spec
spec = do
  DataizeTestGroup{..} <- runIO (dataizationTests "test/eo/phi/dataization.yaml")
  ruleset <- runIO $ parseRuleSetFromFile "test/eo/phi/rules/yegor.yaml"
  let rules = map convertRuleNamed ruleset.rules
  describe title $
    forM_ tests $
      \test -> do
        let inputObj = progToObj test.input
        let expectedResult = case test.output of
              Object obj -> Left obj
              Bytes bytes -> Right bytes
        it test.name $ do
          let dataizedResult = dataizeRecursively (defaultContext rules inputObj) inputObj
          ObjectOrBytes dataizedResult `shouldBe` ObjectOrBytes expectedResult

progToObj :: Phi.Program -> Phi.Object
progToObj (Phi.Program bindings) = Phi.Formation bindings
