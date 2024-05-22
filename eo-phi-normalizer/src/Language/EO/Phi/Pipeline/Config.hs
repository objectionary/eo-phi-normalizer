{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.EO.Phi.Pipeline.Config where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import GHC.Generics (Generic)
import Language.EO.Phi.Metrics.Data
import Language.EO.Phi.TH (deriveJSON)
import Text.Printf (printf)

data TestSetPhi = TestSetPhi
  { initial :: FilePath
  , normalized :: FilePath
  , bindingsPath :: Maybe String
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
  { source :: FilePath
  , yaml :: FilePath
  , destination :: FilePath
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

data TestSet = TestSet
  { eo :: TestSetEO
  , phi :: TestSetPhi
  , enable :: Maybe Bool
  -- ^
  -- Whether to enable this test set.
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
