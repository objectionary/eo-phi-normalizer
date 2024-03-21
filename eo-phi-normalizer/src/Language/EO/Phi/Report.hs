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

module Language.EO.Phi.Report where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Language.EO.Phi.Metrics (BindingMetrics (..), Metrics (..), MetricsCount, ProgramMetrics, makeUnaryOperation, nan)
import Language.EO.Phi.Metrics qualified as Metrics
import Language.EO.Phi.TH (deriveJSON)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
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

data ReportConfig = ReportConfig
  { reportDirectory :: FilePath
  , reportHtml :: FilePath
  , reportJs :: FilePath
  , reportCss :: FilePath
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

toHtmlReportHeader :: Html
toHtmlReportHeader =
  thead $
    toHtml
      [ tr $
          toHtml
            [ th ! colspan "2" ! class_ "no-sort" $ "attribute"
            , th ! colspan "4" ! class_ "no-sort" $ "(before - after) / before"
            , th ! colspan "4" ! class_ "no-sort" $ "before"
            , th ! colspan "4" ! class_ "no-sort" $ "after"
            , th ! colspan "4" ! class_ "no-sort" $ "location"
            ]
      , tr . toHtml $
          th
            <$> [ "attribute before"
                , "attribute after"
                ]
            <> ( concat . replicate 3 $
                  [ "dataless"
                  , "applications"
                  , "formations"
                  , "dispatches"
                  ]
               )
            <> [ "file before"
               , "bindings path before"
               , "file after"
               , "bindings path after"
               ]
      ]

toHtmlMetricsChange :: MetricsChange -> MetricsChange -> [Html]
toHtmlMetricsChange expectedChange actualChange =
  [ makeTd x y
  | x <- [expectedChange.formations, expectedChange.dataless, expectedChange.applications, expectedChange.dispatches]
  | y <- [actualChange.formations, actualChange.dataless, actualChange.applications, actualChange.dispatches]
  ]
 where
  makeTd expected actual
    | expected == nan || actual == nan = td ! class_ "number nan" $ "NaN"
    | expected > actual = td ! class_ "number bad" $ toHtml actual
    | otherwise = td ! class_ "number good" $ toHtml actual

toHtmlMetrics :: MetricsCount -> [Html]
toHtmlMetrics metrics =
  (td ! class_ "number")
    . toHtml
    <$> [ metrics.formations
        , metrics.dataless
        , metrics.applications
        , metrics.dispatches
        ]

toHtmlReportRow :: MetricsChange -> ReportRow -> Html
toHtmlReportRow expectedMetricsChange reportRow =
  tr . toHtml $
    ( ( td
          . toHtml
          <$> [ fromMaybe "[N/A]" reportRow.attributeBefore
              , fromMaybe "[N/A]" reportRow.attributeAfter
              ]
      )
        <> toHtmlMetricsChange expectedMetricsChange reportRow.metricsChange
        <> toHtmlMetrics reportRow.metricsBefore
        <> toHtmlMetrics reportRow.metricsAfter
        <> ( td
              . toHtml
              <$> [ fromMaybe "[all files]" reportRow.fileBefore
                  , intercalate "." $ fromMaybe ["[whole program]"] reportRow.bindingsPathBefore
                  , fromMaybe "[all files]" reportRow.fileAfter
                  , intercalate "." $ fromMaybe ["[whole program]"] reportRow.bindingsPathAfter
                  ]
           )
    )

toHtmlReport :: ReportConfig -> Report -> Html
toHtmlReport reportConfig report =
  toHtml
    [ table ! class_ "sortable" $
        toHtml
          [ toHtmlReportHeader
          , tbody . toHtml $
              toHtmlReportRow reportConfig.expectedMetricsChange
                <$> ( report.totalRow
                        : concat
                          [ programReport.programRow : programReport.bindingsRows
                          | programReport <- report.programReports
                          ]
                    )
          ]
    , link ! href (toValue reportConfig.reportCss) ! rel "stylesheet"
    , script ! src (toValue reportConfig.reportJs) $ ""
    ]
