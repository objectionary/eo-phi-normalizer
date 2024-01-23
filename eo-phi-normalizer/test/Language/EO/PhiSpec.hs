{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Language.EO.PhiSpec where

import Control.Monad (forM_)
import Data.String.Interpolate (i)
import Language.EO.Phi
import Language.EO.Phi.Rules.Common (Context (..), Rule)
import Language.EO.Phi.Rules.PhiPaper (rule1, rule6)
import Test.EO.Phi
import Test.Hspec

applyRule :: (Object -> [Object]) -> Program -> [Program]
applyRule rule = \case
  Program [AlphaBinding name obj] -> do
    r <- rule obj
    pure $ Program [AlphaBinding name r]
  _ -> []

spec :: Spec
spec = do
  describe "Pre-defined rules unit tests" $
    forM_ ([(1, rule1), (6, rule6)] :: [(Int, Rule)]) $
      \(idx, rule) -> do
        PhiTestGroup{..} <- runIO (fileTests [i|test/eo/phi/rule-#{idx}.yaml|])
        describe title $
          forM_ tests $
            \PhiTest{..} ->
              it name $
                applyRule (rule (Context [])) input `shouldBe` [normalized]
