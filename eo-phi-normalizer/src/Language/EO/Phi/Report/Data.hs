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
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Language.EO.Phi.Metrics.Data (BindingMetrics (..), Metrics (..), MetricsCount, ProgramMetrics)
import Language.EO.Phi.Metrics.Data qualified as Metrics
import Language.EO.Phi.Pipeline.Config
import Language.EO.Phi.TH (deriveJSON)
import Prelude hiding (div, id, span)

data ReportRow = ReportRow
  { fileInitial :: Maybe FilePath
  , fileNormalized :: Maybe FilePath
  , bindingsPathInitial :: Maybe Metrics.Path
  , bindingsPathNormalized :: Maybe Metrics.Path
  , attributeInitial :: Maybe String
  , attributeNormalized :: Maybe String
  , metricsChange :: MetricsChangeCategorized
  , metricsInitial :: Metrics Int
  , metricsNormalized :: Metrics Int
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

-- >>> calculateMetricsChange Metrics { dataless = 0.1, applications = 0.2, formations = 0.2, dispatches = 0.2 } Metrics { dataless = 100, applications = 0, formations = 100, dispatches = 100 } Metrics { dataless = 90, applications = 0, formations = 93, dispatches = 60 }
-- Metrics {formations = MetricsChange'Bad {change = 7.00%}, dataless = MetricsChange'Good {change = 10.00%}, applications = MetricsChange'NA, dispatches = MetricsChange'Good {change = 40.00%}}
calculateMetricsChange :: MetricsChange -> MetricsCount -> MetricsCount -> MetricsChangeCategorized
calculateMetricsChange expectedMetricsChange countInitial countNormalized =
  getMetricsChangeClassified <$> expectedMetricsChange <*> actualMetricsChange
 where
  isFinite :: (RealFloat a) => a -> Bool
  isFinite x = not (isNaN x || isInfinite x)

  getMetricsChangeClassified :: Percent -> Percent -> MetricsChangeCategory Percent
  getMetricsChangeClassified expected actual
    | isFinite expected && isFinite actual =
        if actual >= expected
          then MetricsChange'Good actual
          else MetricsChange'Bad actual
    | otherwise = MetricsChange'NA

  actualMetricsChange :: MetricsChange
  actualMetricsChange = (initial - normalized) / initial
  initial = fromIntegral <$> countInitial
  normalized = fromIntegral <$> countNormalized

makeProgramReport :: PipelineConfig -> TestSetPhi -> ProgramMetrics -> ProgramMetrics -> ProgramReport
makeProgramReport pipelineConfig testSet metricsPhi metricsPhiNormalized =
  ProgramReport{..}
 where
  bindingsRows =
    case (metricsPhi.bindingsByPathMetrics, metricsPhiNormalized.bindingsByPathMetrics) of
      (Just bindingsMetricsInitial, Just bindingsMetricsNormalized) ->
        [ ReportRow
          { fileInitial = Just testSet.initial
          , fileNormalized = Just testSet.normalized
          , bindingsPathInitial = Just bindingsMetricsInitial.path
          , bindingsPathNormalized = Just bindingsMetricsNormalized.path
          , attributeInitial = Just attributeInitial
          , attributeNormalized = Just attributeNormalized
          , metricsChange = calculateMetricsChange pipelineConfig.report.expectedMetricsChange metricsInitial metricsNormalized
          , metricsInitial = metricsInitial
          , metricsNormalized = metricsNormalized
          }
        | BindingMetrics{name = attributeInitial, metrics = metricsInitial} <- bindingsMetricsInitial.bindingsMetrics
        | BindingMetrics{name = attributeNormalized, metrics = metricsNormalized} <- bindingsMetricsNormalized.bindingsMetrics
        ]
      _ -> []
  programRow =
    ReportRow
      { fileInitial = Just testSet.initial
      , fileNormalized = Just testSet.normalized
      , bindingsPathInitial = Nothing
      , bindingsPathNormalized = Nothing
      , attributeInitial = Nothing
      , attributeNormalized = Nothing
      , metricsChange = calculateMetricsChange pipelineConfig.report.expectedMetricsChange metricsPhi.programMetrics metricsPhiNormalized.programMetrics
      , metricsInitial = metricsPhi.programMetrics
      , metricsNormalized = metricsPhiNormalized.programMetrics
      }

makeReport :: PipelineConfig -> [ProgramReport] -> Report
makeReport pipelineConfig programReports =
  Report{..}
 where
  programRows = (.programRow) <$> programReports
  metricsInitial = foldMap (.metricsInitial) programRows
  metricsNormalized = foldMap (.metricsNormalized) programRows
  metricsChange = calculateMetricsChange pipelineConfig.report.expectedMetricsChange metricsInitial metricsNormalized
  totalRow =
    ReportRow
      { fileInitial = Nothing
      , fileNormalized = Nothing
      , bindingsPathInitial = Nothing
      , bindingsPathNormalized = Nothing
      , attributeInitial = Nothing
      , attributeNormalized = Nothing
      , ..
      }
