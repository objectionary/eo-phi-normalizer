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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Language.EO.PhiSpec where

import Control.Monad (forM_)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.String (IsString (..))
import Data.Yaml (decodeFileThrow)
import Language.EO.Phi (Binding (..), Object, Program (..), normalize, printTree)
import Language.EO.Phi.Metrics.Collect (getProgramMetrics)
import Language.EO.Phi.Metrics.Data (BindingsByPathMetrics (..), ProgramMetrics (..))
import Language.EO.Phi.Rules.Common (equalProgram)
import Test.EO.Phi (PhiTest (..), PhiTestGroup (..), allPhiTests)
import Test.Hspec (Spec, describe, it, runIO, shouldBe, shouldSatisfy)
import Test.Metrics.Phi (MetricsTest (..), MetricsTestSet (..))

applyRule :: (Object -> [Object]) -> Program -> [Program]
applyRule rule = \case
  Program [AlphaBinding name obj] -> do
    r <- rule obj
    pure $ Program [AlphaBinding name r]
  _ -> []

spec :: Spec
spec = do
  describe "Programs translated from EO" $ do
    phiTests <- runIO (allPhiTests "test/eo/phi/from-eo/")
    forM_ phiTests $ \PhiTestGroup{..} ->
      describe title $
        forM_ tests $
          \PhiTest{..} -> do
            describe "normalize" $
              it name $
                normalize input `shouldSatisfy` equalProgram normalized
            describe "pretty-print" $
              it name $
                printTree input `shouldBe` trim prettified
  describe "Metrics" $ do
    metricsTests <- runIO $ decodeFileThrow @_ @MetricsTestSet "test/eo/phi/metrics.yaml"
    forM_ metricsTests.tests $ \test -> do
      it test.title $
        getProgramMetrics (fromString @Program test.phi) ((.path) <$> test.metrics.bindingsByPathMetrics) `shouldBe` Right test.metrics

trim :: String -> String
trim = dropWhileEnd isSpace
