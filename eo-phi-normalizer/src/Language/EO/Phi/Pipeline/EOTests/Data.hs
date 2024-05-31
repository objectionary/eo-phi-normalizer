{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Language.EO.Phi.Pipeline.EOTests.Data where

import Control.Monad (guard)
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Yaml.Aeson
import GHC.Generics (Generic)
import Language.EO.Phi.TH (deriveJSON)
import Text.Read (readMaybe)

data Pos = Pos
  { file :: FilePath
  , line :: Int
  }
  deriving stock (Show)

instance ToJSON Pos where
  toJSON :: Pos -> Value
  toJSON Pos{..} = String (fromString (file <> ":" <> show line))

instance FromJSON Pos where
  parseJSON :: Value -> Parser Pos
  parseJSON = withText "Pos" $ \(T.unpack -> x) -> do
    let (file, rs) = span (/= ':') x
    guard (not . null $ file)
    guard (length rs > 1)
    let line' = readMaybe (drop 1 rs)
    maybe (fail $ x <> " is not a number") (\line -> pure Pos{..}) line'

data Program = Program
  { source :: Pos
  , name :: String
  , text :: String
  }
  deriving stock (Show, Generic)

$(deriveJSON ''Program)

data Test = Test
  { source :: String
  , license :: String
  , meta :: String
  , programs :: [Program]
  }
  deriving stock (Show, Generic)

$(deriveJSON ''Test)

data TestContent = TestContent
  { source :: FilePath
  , meta :: String
  , programs :: [Program]
  }
  deriving stock (Show, Generic)

$(deriveJSON ''TestContent)
