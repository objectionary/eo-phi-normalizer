{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Test.EO.Phi where

import Data.Aeson (FromJSON(..))
import qualified Data.Yaml as Yaml
import System.Directory (listDirectory)
import System.FilePath ((</>))
import GHC.Generics (Generic)
import Control.Monad (forM)

import qualified Language.EO.Phi as Phi
import Language.EO.Phi (unsafeParseProgram)
import Data.List (sort)

data PhiTest = PhiTest
  { name       :: String
  , input      :: Phi.Program
  , normalized :: Phi.Program
  , prettified :: String
  } deriving (Generic, FromJSON)

allPhiTests :: FilePath -> IO [PhiTest]
allPhiTests dir = do
  paths <- listDirectory dir
  forM (sort paths) $ \path ->
    Yaml.decodeFileThrow (dir </> path)

-- * Orphan instances

-- | Parsing a $\varphi$-program from a JSON string.
instance FromJSON Phi.Program where
  parseJSON = fmap unsafeParseProgram . parseJSON
