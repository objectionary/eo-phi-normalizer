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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Language.EO.Phi.DataizeSpec where

import Control.Monad (forM_)
import Test.Hspec

import Language.EO.Phi (printTree, printTreeNoSugar)
import Language.EO.Phi qualified as Phi
import Language.EO.Phi.Dataize (dataizeRecursively)
import Language.EO.Phi.Dataize.Context (defaultContext)
import Language.EO.Phi.Dependencies (deepMergePrograms)
import Language.EO.Phi.Rules.Common (equalObject)
import Language.EO.Phi.Rules.Yaml (convertRuleNamed, parseRuleSetFromFile, rules)
import Language.EO.Phi.Syntax (NoDesugar(..), SugarableFinally(..))
import Prettyprinter (Pretty (..))
import Test.EO.Phi (DataizationResult (Bytes, Object), DataizeTest (..), DataizeTestGroup (..), dataizationTests, progToObj)

newtype ObjectOrBytes = ObjectOrBytes (Either Phi.Object Phi.Bytes) deriving (Show)

instance Pretty ObjectOrBytes where
  pretty (ObjectOrBytes (Left obj)) = pretty $ printTree obj
  pretty (ObjectOrBytes (Right bytes)) = pretty $ printTree bytes

instance Eq ObjectOrBytes where
  ObjectOrBytes (Left x) == ObjectOrBytes (Left y) =
    x `equalObject` y
  ObjectOrBytes (Right x) == ObjectOrBytes (Right y) =
    x == y
  _ == _ = False

instance SugarableFinally ObjectOrBytes where
  sugarFinally (ObjectOrBytes x) = ObjectOrBytes $
    case x of
      Left obj -> Left (sugarFinally obj)
      Right bytes -> Right (sugarFinally bytes)

spec :: Spec
spec = do
  DataizeTestGroup{..} <- runIO (dataizationTests "test/eo/phi/dataization.yaml")
  describe title do
    forM_
      [ ("New Yegor's rules", "test/eo/phi/rules/new.yaml")
      ]
      $ \(rulesTitle, rulesFile) -> do
        ruleset <- runIO $ parseRuleSetFromFile rulesFile
        let rules = map convertRuleNamed ruleset.rules
        describe rulesTitle do
          forM_ tests $
            \test -> do
              deps <- runIO $ mapM Phi.unsafeParseProgramFromFile test.dependencies
              let input' = test.input
                  mergedProgs = case deepMergePrograms (input' : deps) of
                    Left err -> error ("Error merging programs: " ++ err)
                    Right prog -> prog
                  ctx = defaultContext rules (progToObj mergedProgs)
                  inputObj = progToObj input'
                  expectedResult = case test.output of
                    Object obj -> Left obj.noDesugar
                    Bytes bytes -> Right bytes
                  dataizedResult = dataizeRecursively ctx inputObj
                  expectedResult' = ObjectOrBytes expectedResult
                  dataizedResult' = ObjectOrBytes dataizedResult
              describe test.name do
                it "value" do
                  dataizedResult' `shouldBe` expectedResult'
                it "no sugar" do
                  printTreeNoSugar dataizedResult' `shouldBe` printTreeNoSugar expectedResult'
                it "sugar" do
                  printTree dataizedResult' `shouldBe` printTree expectedResult'
