{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.EO.Phi where

import Control.Monad (forM)
import Data.Aeson (FromJSON (..), SumEncoding (UntaggedValue), defaultOptions, genericParseJSON, sumEncoding)
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import System.Directory (listDirectory)
import System.FilePath ((</>))

import Data.List (sort)
import Language.EO.Phi (unsafeParseObject, unsafeParseProgram)
import Language.EO.Phi qualified as Phi

data PhiTestGroup = PhiTestGroup
  { title :: String
  , tests :: [PhiTest]
  }
  deriving (Generic, FromJSON)

data PhiTest = PhiTest
  { name :: String
  , input :: Phi.Program
  , normalized :: Phi.Program
  , prettified :: String
  }
  deriving (Generic, FromJSON)

data DataizeTestGroup = DataizeTestGroup
  { title :: String
  , tests :: [DataizeTest]
  }
  deriving (Generic, FromJSON)

data DataizeTest = DataizeTest
  { name :: String
  , input :: Phi.Program
  , output :: DataizationResult
  }
  deriving (Generic, FromJSON)
data DataizationResult
  = Bytes {bytes :: Phi.Bytes}
  | Object {object :: Phi.Object}
  deriving (Generic, Show)
instance FromJSON DataizationResult where
  parseJSON = genericParseJSON defaultOptions{sumEncoding = UntaggedValue}

fileTests :: FilePath -> IO PhiTestGroup
fileTests = Yaml.decodeFileThrow

dataizationTests :: FilePath -> IO DataizeTestGroup
dataizationTests = Yaml.decodeFileThrow

allPhiTests :: FilePath -> IO [PhiTestGroup]
allPhiTests dir = do
  paths <- listDirectory dir
  forM (sort paths) $ \path ->
    fileTests (dir </> path)

-- * Orphan instances

-- | Parsing a $\varphi$-program from a JSON string.
instance FromJSON Phi.Program where
  parseJSON = fmap unsafeParseProgram . parseJSON

-- | Parsing a $\varphi$-object from a JSON string.
instance FromJSON Phi.Object where
  parseJSON = fmap unsafeParseObject . parseJSON

deriving newtype instance FromJSON Phi.Bytes
