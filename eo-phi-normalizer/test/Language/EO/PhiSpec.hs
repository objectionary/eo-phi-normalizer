{-# LANGUAGE RecordWildCards #-}
module Language.EO.PhiSpec where

import Test.Hspec
import Control.Monad (forM_)
import Data.List (dropWhileEnd)
import Data.Char (isSpace)

import qualified Language.EO.Phi as Phi
import Test.EO.Phi

spec :: Spec
spec = do
  tests <- runIO (allPhiTests "test/eo/phi/")
  describe "Normalizer" $ do
    forM_ tests $ \PhiTest{..} -> do
      it name $ do
        Phi.printTree (Phi.normalize input) `shouldBe` Phi.printTree normalized
  describe "Prettify" $ do
    forM_ tests $ \PhiTest{..} -> do
      it name $ do
        Phi.printTree input `shouldBe` dropWhileEnd isSpace prettified
