{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Language.EO.PhiSpec where

import Control.Monad (forM_)
import Data.String.Interpolate (i)
import Language.EO.Phi
import Language.EO.Phi.Rules.Common (Context (..), Rule)
import Language.EO.Phi.Rules.PhiPaper (rule1, rule6)
import Test.EO.Phi
import Test.Hspec

applyRule :: (Object -> Maybe Object) -> Program -> Maybe Program
applyRule rule = \case
  Program [AlphaBinding name obj] -> do
    r <- rule obj
    pure $ Program [AlphaBinding name r]
  _ -> Nothing

spec :: Spec
spec = do
  describe "Rules unit tests" $
    forM_ ([(1, rule1), (6, rule6)] :: [(Int, Rule)]) $
      \(idx, rule) -> do
        PhiTestGroup{..} <- runIO (filePhiTests [i|test/eo/phi/rule-#{idx}.yaml|])
        describe title $
          forM_ tests $
            \PhiTest{..} ->
              it name do
                applyRule (rule (Context [])) input `shouldBe` Just normalized
