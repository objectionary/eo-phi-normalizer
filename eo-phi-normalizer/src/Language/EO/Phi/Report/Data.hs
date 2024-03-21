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

module Language.EO.Phi.Report.Data where

import GHC.Generics (Generic)
import Language.EO.Phi.Metrics (BindingMetrics (..), Metrics (..), MetricsCount, ProgramMetrics, makeUnaryOperation)
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

type MetricsChange = Metrics Double

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
  , metricsChange :: MetricsChange
  , metricsBefore :: Metrics Int
  , metricsAfter :: Metrics Int
  }

data ProgramReport = ProgramReport
  { programRow :: ReportRow
  , bindingsRows :: [ReportRow]
  }

data Report = Report
  { totalRow :: ReportRow
  , programReports :: [ProgramReport]
  }

calculateMetricsChange :: MetricsCount -> MetricsCount -> MetricsChange
calculateMetricsChange before after =
  (before' - after') / before'
 where
  before' = makeUnaryOperation fromIntegral before
  after' = makeUnaryOperation fromIntegral after

makeProgramReport :: ReportItem -> ProgramMetrics -> ProgramMetrics -> ProgramReport
makeProgramReport reportItem metricsPhi metricsPhiNormalized =
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
          , metricsChange = calculateMetricsChange metricsBefore metricsAfter
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
      , metricsChange = calculateMetricsChange metricsPhi.programMetrics metricsPhiNormalized.programMetrics
      , metricsBefore = metricsPhi.programMetrics
      , metricsAfter = metricsPhiNormalized.programMetrics
      }

makeReport :: [ProgramReport] -> Report
makeReport programReports =
  Report{..}
 where
  programRows = (.programRow) <$> programReports
  metricsBefore = foldMap (.metricsBefore) programRows
  metricsAfter = foldMap (.metricsAfter) programRows
  metricsChange = calculateMetricsChange metricsBefore metricsAfter
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
