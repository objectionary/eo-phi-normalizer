{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.EO.Phi.Report.Html where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Language.EO.Phi.Metrics (Metrics (..), MetricsCount, nan)
import Language.EO.Phi.Report.Data
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Prelude hiding (div, id, span)

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

toHtmlReportRow :: ReportConfig -> ReportRow -> Html
toHtmlReportRow reportConfig reportRow =
  tr . toHtml $
    ( td
        . toHtml
        <$> [ fromMaybe "[N/A]" reportRow.attributeBefore
            , fromMaybe "[N/A]" reportRow.attributeAfter
            ]
    )
      <> toHtmlMetricsChange reportConfig.expectedMetricsChange reportRow.metricsChange
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

toHtmlReport :: ReportConfig -> Report -> Html
toHtmlReport reportConfig report =
  toHtml
    [ table ! class_ "sortable" $
        toHtml
          [ toHtmlReportHeader
          , tbody . toHtml $
              toHtmlReportRow reportConfig
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
