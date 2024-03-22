{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.EO.Phi.Report.Data where

import GHC.Generics (Generic)
import Language.EO.Phi.Metrics (BindingMetrics (..), Metrics (..), MetricsCount, ProgramMetrics, SafeNumber (..))
import Language.EO.Phi.Metrics qualified as Metrics
import Language.EO.Phi.TH (deriveJSON)
import Prelude hiding (div, id, span)

data ReportItem = ReportItem
  { phi :: FilePath
  , phiNormalized :: FilePath
  , bindingsPathPhi :: Maybe String
  , bindingsPathPhiNormalized :: Maybe String
  }
  deriving stock (Show, Generic)

$(deriveJSON ''ReportItem)

data MetricsChangeCategory a
  = MetricsChange'Good {change :: a}
  | MetricsChange'Bad {change :: a}
  | MetricsChange'NaN
  deriving stock (Show, Generic, Eq)

$(deriveJSON ''MetricsChangeCategory)

type MetricsChange = Metrics (SafeNumber Double)

newtype Percent = Percent {percent :: Double}

$(deriveJSON ''Percent)

type MetricsChangeCategorized = Metrics (MetricsChangeCategory Percent)

data ReportPage = ReportPage
  { directory :: FilePath
  , html :: FilePath
  , js :: FilePath
  , css :: FilePath
  }
  deriving stock (Show, Generic)

$(deriveJSON ''ReportPage)

data ReportConfig = ReportConfig
  { reportPage :: ReportPage
  , reportJson :: Maybe FilePath
  , expectedMetricsChange :: MetricsChange
  , items :: [ReportItem]
  }
  deriving stock (Show, Generic)

$(deriveJSON ''ReportConfig)

data ReportRow = ReportRow
  { fileBefore :: Maybe FilePath
  , fileAfter :: Maybe FilePath
  , bindingsPathBefore :: Maybe Metrics.Path
  , bindingsPathAfter :: Maybe Metrics.Path
  , attributeBefore :: Maybe String
  , attributeAfter :: Maybe String
  , metricsChange :: MetricsChangeCategorized
  , metricsBefore :: Metrics Int
  , metricsAfter :: Metrics Int
  }

$(deriveJSON ''ReportRow)

data ProgramReport = ProgramReport
  { programRow :: ReportRow
  , bindingsRows :: [ReportRow]
  }

$(deriveJSON ''ProgramReport)

data Report = Report
  { totalRow :: ReportRow
  , programReports :: [ProgramReport]
  }

$(deriveJSON ''Report)

calculateMetricsChange :: MetricsChange -> MetricsCount -> MetricsCount -> MetricsChangeCategorized
calculateMetricsChange expectedMetricsChange before after =
  getMetricsChangeClassified <$> expectedMetricsChange <*> actualMetricsChange
 where
  getMetricsChangeClassified (SafeNumber'Number expected) (SafeNumber'Number actual)
    | actual >= expected = MetricsChange'Good (Percent actual)
    | otherwise = MetricsChange'Bad (Percent actual)
  getMetricsChangeClassified _ _ = MetricsChange'NaN
  actualMetricsChange :: MetricsChange
  actualMetricsChange = (before' - after') / before'
  before' = fromIntegral <$> before
  after' = fromIntegral <$> after

makeProgramReport :: ReportConfig -> ReportItem -> ProgramMetrics -> ProgramMetrics -> ProgramReport
makeProgramReport reportConfig reportItem metricsPhi metricsPhiNormalized =
  ProgramReport{..}
 where
  bindingsRows =
    case (metricsPhi.bindingsByPathMetrics, metricsPhiNormalized.bindingsByPathMetrics) of
      (Just bindingsMetricsAfter, Just bindingsMetricsBefore) ->
        [ ReportRow
          { fileBefore = Just reportItem.phi
          , fileAfter = Just reportItem.phiNormalized
          , bindingsPathBefore = Just bindingsMetricsAfter.path
          , bindingsPathAfter = Just bindingsMetricsAfter.path
          , attributeBefore = Just attributeBefore
          , attributeAfter = Just attributeAfter
          , metricsChange = calculateMetricsChange reportConfig.expectedMetricsChange metricsBefore metricsAfter
          , metricsBefore = metricsBefore
          , metricsAfter = metricsAfter
          }
        | BindingMetrics{name = attributeBefore, metrics = metricsBefore} <- bindingsMetricsBefore.bindingsMetrics
        | BindingMetrics{name = attributeAfter, metrics = metricsAfter} <- bindingsMetricsAfter.bindingsMetrics
        ]
      _ -> []
  programRow =
    ReportRow
      { fileBefore = Just reportItem.phi
      , fileAfter = Just reportItem.phiNormalized
      , bindingsPathBefore = Nothing
      , bindingsPathAfter = Nothing
      , attributeBefore = Nothing
      , attributeAfter = Nothing
      , metricsChange = calculateMetricsChange reportConfig.expectedMetricsChange metricsPhi.programMetrics metricsPhiNormalized.programMetrics
      , metricsBefore = metricsPhi.programMetrics
      , metricsAfter = metricsPhiNormalized.programMetrics
      }

makeReport :: ReportConfig -> [ProgramReport] -> Report
makeReport reportConfig programReports =
  Report{..}
 where
  programRows = (.programRow) <$> programReports
  metricsBefore = foldMap (.metricsBefore) programRows
  metricsAfter = foldMap (.metricsAfter) programRows
  metricsChange = calculateMetricsChange reportConfig.expectedMetricsChange metricsBefore metricsAfter
  totalRow =
    ReportRow
      { fileBefore = Nothing
      , fileAfter = Nothing
      , bindingsPathBefore = Nothing
      , bindingsPathAfter = Nothing
      , attributeBefore = Nothing
      , attributeAfter = Nothing
      , ..
      }
