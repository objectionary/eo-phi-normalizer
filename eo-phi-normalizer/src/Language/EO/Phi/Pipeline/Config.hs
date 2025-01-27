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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.EO.Phi.Pipeline.Config where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Yaml (decodeFileThrow)
import GHC.Generics (Generic)
import Language.EO.Phi.Metrics.Data
import Language.EO.Phi.TH (deriveJSON)
import PyF (fmt)
import Text.Printf (printf)

data TestSetPhi = TestSetPhi
  { initial :: FilePath
  , normalized :: FilePath
  , bindingsPathInitial :: Maybe String
  , bindingsPathNormalized :: Maybe String
  }
  deriving stock (Show, Generic)

$(deriveJSON ''TestSetPhi)

data MetricsChangeCategory a
  = MetricsChange'Good {change :: a}
  | MetricsChange'Bad {change :: a}
  | MetricsChange'NA
  deriving stock (Show, Generic, Eq)

$(deriveJSON ''MetricsChangeCategory)

type MetricsChange = Metrics Percent

newtype Percent = Percent {percent :: Double}
  deriving newtype
    (FromJSON, ToJSON, Num, Fractional, Floating, Eq, Ord, Real, RealFrac, RealFloat)

roundToStr :: Int -> Double -> String
roundToStr = printf "%0.*f%%"

instance Show Percent where
  show :: Percent -> String
  show Percent{..} = roundToStr 2 (percent * 100)

type MetricsChangeCategorized = Metrics (MetricsChangeCategory Percent)

data ReportInput = ReportInput
  { js :: Maybe FilePath
  , css :: Maybe FilePath
  }
  deriving stock (Show, Generic)

$(deriveJSON ''ReportInput)

data ReportOutput = ReportOutput
  { html :: Maybe FilePath
  , json :: Maybe FilePath
  , markdown :: Maybe FilePath
  }
  deriving stock (Show, Generic)

$(deriveJSON ''ReportOutput)

data ReportConfig = ReportConfig
  { input :: Maybe ReportInput
  , output :: ReportOutput
  , expectedMetricsChange :: MetricsChange
  , expectedImprovedProgramsPercentage :: Percent
  }
  deriving stock (Show, Generic)

$(deriveJSON ''ReportConfig)

data TestSetEO = TestSetEO
  { original :: FilePath
  , yaml :: FilePath
  , filtered :: FilePath
  , include :: Maybe [String]
  -- ^
  -- Program names to include.
  --
  -- `Nothing` is equivalent to all programs.
  , exclude :: Maybe [String]
  -- ^
  -- Program names to exclude
  --
  -- `Nothing` is equivalent to no programs.
  }
  deriving stock (Show, Generic)

$(deriveJSON ''TestSetEO)

data AtomsSet = AtomsSet
  { enable :: Maybe [String]
  , disable :: Maybe [String]
  }
  deriving stock (Show, Generic)

$(deriveJSON ''AtomsSet)

data PathPrefixEO = PathPrefixEO
  { original :: FilePath
  , yaml :: FilePath
  , filtered :: FilePath
  }
  deriving stock (Show, Generic)

$(deriveJSON ''PathPrefixEO)

data PathPrefixPhi = PathPrefixPhi
  { initial :: FilePath
  , normalized :: FilePath
  }
  deriving stock (Show, Generic)

$(deriveJSON ''PathPrefixPhi)

data PathPrefix = PathPrefix
  { eo :: PathPrefixEO
  , phi :: PathPrefixPhi
  }
  deriving stock (Show, Generic)

$(deriveJSON ''PathPrefix)

data Common = Common
  { pathPrefix :: PathPrefix
  , bindingsPath :: Maybe String
  }
  deriving stock (Show, Generic)

$(deriveJSON ''Common)

data Individual = Individual
  { name :: String
  , enable :: Maybe Bool
  , include :: Maybe [String]
  , exclude :: Maybe [String]
  , atoms :: Maybe AtomsSet
  }
  deriving stock (Show, Generic)

$(deriveJSON ''Individual)

data TestSet
  = TestSetExtended
      { eo :: TestSetEO
      , phi :: TestSetPhi
      , atoms :: Maybe AtomsSet
      , enable :: Maybe Bool
      -- ^
      -- Whether to enable this test set.
      }
  | TestSetCompact
      { common :: Common
      , individual :: [Individual]
      }
  deriving stock (Show, Generic)

$(deriveJSON ''TestSet)

data PipelineConfig = PipelineConfig
  { report :: ReportConfig
  , testSets :: [TestSet]
  }
  deriving stock (Show, Generic)

$(deriveJSON ''PipelineConfig)

data ReportFormat
  = ReportFormat'Html
  | -- | GitHub Flavored Markdown
    ReportFormat'Markdown
  deriving stock (Eq)

split :: forall a. (a -> Bool) -> [a] -> [[a]]
split cond xs = go xs [] []
 where
  go [] _ res = res
  go (y : ys) curSpan res
    | cond y = go ys [] (res <> [curSpan])
    | otherwise = go ys (curSpan <> [y]) res

-- >>> split @Int (== 3) [1,2,3,3,4,3]

toExtended :: PipelineConfig -> PipelineConfig
toExtended c@(PipelineConfig{testSets}) = c{testSets = concatMap go testSets}
 where
  go = \case
    e@TestSetExtended{} -> [e]
    TestSetCompact{..} -> go1 common <$> individual

  go1 (Common{..}) (Individual{..}) = TestSetExtended{..}
   where
    mkPath :: FilePath -> String -> String
    mkPath prefix extension = [fmt|{prefix}/{name}.{extension}|]
    eo =
      TestSetEO
        { original = mkPath pathPrefix.eo.original "eo"
        , yaml = mkPath pathPrefix.eo.yaml "yaml"
        , filtered = mkPath pathPrefix.eo.filtered "eo"
        , include
        , exclude
        }
    phi =
      TestSetPhi
        { initial = mkPath pathPrefix.phi.initial "phi"
        , normalized = mkPath pathPrefix.phi.normalized "phi"
        , bindingsPathInitial = bindingsPath <&> mkBindingsPathSuffix
        , bindingsPathNormalized = bindingsPath <&> mkBindingsPathSuffix
        }
     where
      name' = split (== '/') name
      mkBindingsPathSuffix x = x <> intercalate "." (if name' /= [] then init name' else [])

readPipelineConfig :: (MonadIO m) => FilePath -> m PipelineConfig
readPipelineConfig path = toExtended <$> decodeFileThrow @_ @PipelineConfig path
