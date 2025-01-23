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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Language.EO.Phi.RewriteSpec where

import Control.Monad (forM_)
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import Language.EO.Phi (Program (..))
import Language.EO.Phi.Dataize.Context (defaultContext)
import Language.EO.Phi.Rules.Common (applyRules)
import Language.EO.Phi.Rules.Yaml (convertRuleNamed, parseRuleSetFromFile, rules)
import Language.EO.Phi.Syntax (printTree)
import Language.EO.Phi.TH
import Test.EO.Phi (progToObj)
import Test.Hspec

data Test = Test
  { name :: String
  , input :: Program
  , output :: Program
  }
  deriving stock (Generic, Show)

$(deriveFromJSON ''Test)

data RewriteTests = RewriteTests
  { title :: String
  , tests :: [Test]
  }
  deriving stock (Generic, Show)

$(deriveFromJSON ''RewriteTests)

spec :: Spec
spec = do
  rewriteTests <- runIO (Yaml.decodeFileThrow @_ @RewriteTests "test/eo/phi/rewriting.yaml")
  describe rewriteTests.title do
    forM_
      [ ("New Yegor's rules", "test/eo/phi/rules/new.yaml")
      ]
      $ \(rulesTitle, rulesFile) -> do
        ruleset <- runIO $ parseRuleSetFromFile rulesFile
        let rules' = convertRuleNamed <$> ruleset.rules
        describe rulesTitle do
          forM_ rewriteTests.tests $
            \test -> it test.name do
              let
                inputObj = progToObj test.input
                expectedOutputObj = progToObj test.output
                ctx = defaultContext rules' inputObj
                outputObj = head $ applyRules ctx inputObj
              printTree outputObj `shouldBe` printTree expectedOutputObj
