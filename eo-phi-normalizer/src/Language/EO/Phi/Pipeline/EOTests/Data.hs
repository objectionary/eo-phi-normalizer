{- FOURMOLU_DISABLE -}
-- The MIT License (MIT)

-- Copyright (c) 2016-2024 Objectionary.com

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
