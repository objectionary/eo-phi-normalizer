{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Language.EO.PhiSpec where

import Control.Monad (forM_)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.String (IsString (..))
import Data.String.Interpolate (i)
import Data.Yaml (decodeFileThrow)
import Language.EO.Phi
import Language.EO.Phi.Metrics.Collect (collectMetrics)
import Language.EO.Phi.Rules.Common (Context (..), Rule)
import Language.EO.Phi.Rules.PhiPaper (rule1, rule6)
import Test.EO.Phi
import Test.Hspec
import Test.Metrics.Phi (MetricsTest (..), MetricsTestSet (..))

applyRule :: (Object -> [Object]) -> Program -> [Program]
applyRule rule = \case
  Program [AlphaBinding name obj] -> do
    r <- rule obj
    pure $ Program [AlphaBinding name r]
  _ -> []

spec :: Spec
spec = do
  describe "Pre-defined rules" $
    forM_ ([(1, rule1), (6, rule6)] :: [(Int, Rule)]) $
      \(idx, rule) -> do
        PhiTestGroup{..} <- runIO (fileTests [i|test/eo/phi/rule-#{idx}.yaml|])
        describe title $
          forM_ tests $
            \PhiTest{..} ->
              it name $
                applyRule (rule (Context [] [progToObj input])) input `shouldBe` [normalized]
  describe "Programs translated from EO" $ do
    phiTests <- runIO (allPhiTests "test/eo/phi/from-eo/")
    forM_ phiTests $ \PhiTestGroup{..} ->
      describe title $
        forM_ tests $
          \PhiTest{..} -> do
            describe "normalize" $
              it name $
                normalize input `shouldBe` normalized
            describe "pretty-print" $
              it name $
                printTree input `shouldBe` trim prettified
  describe "Metrics" $ do
    metricsTests <- runIO $ decodeFileThrow @_ @MetricsTestSet "test/eo/phi/metrics.yaml"
    forM_ metricsTests.tests $ \test -> do
      it test.title $
        collectMetrics (fromString @Program test.phi) `shouldBe` test.metrics

trim :: String -> String
trim = dropWhileEnd isSpace

progToObj :: Program -> Object
progToObj (Program bindings) = Formation bindings
