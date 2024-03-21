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
import Language.EO.Phi.Report.Data (MetricsChange, ProgramReport (..), Report (..), ReportRow (..))
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes (class_, colspan, type_)
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

-- >>> import Text.Blaze.Html.Renderer.String (renderHtml)
--
-- >>> renderHtml $ toHtmlChange 0.5 0.2
-- "<td class=\"number bad\">0.2</td>"
--
-- >>> renderHtml $ toHtmlChange 0.2 0.5
-- "<td class=\"number good\">0.5</td>"
-- >>> renderHtml $ toHtmlChange nan 0.1
-- "<td class=\"number nan\">NaN</td>"
--
-- >>> renderHtml $ toHtmlChange 0.1 nan
-- "<td class=\"number nan\">NaN</td>"
--
-- >>> renderHtml $ toHtmlChange nan nan
-- "<td class=\"number nan\">NaN</td>"
toHtmlChange :: (Fractional a, Ord a, ToMarkup a) => a -> a -> Html
toHtmlChange expected actual
  | expected == nan || actual == nan = td ! class_ "number nan" $ "NaN"
  | expected > actual = td ! class_ "number bad" $ toHtml actual
  | otherwise = td ! class_ "number good" $ toHtml actual

toHtmlMetricsChange :: MetricsChange -> MetricsChange -> [Html]
toHtmlMetricsChange expectedChange actualChange =
  [ toHtmlChange expected actual
  | expected <- [expectedChange.formations, expectedChange.dataless, expectedChange.applications, expectedChange.dispatches]
  | actual <- [actualChange.formations, actualChange.dataless, actualChange.applications, actualChange.dispatches]
  ]

toHtmlMetrics :: MetricsCount -> [Html]
toHtmlMetrics metrics =
  (td ! class_ "number")
    . toHtml
    <$> [ metrics.formations
        , metrics.dataless
        , metrics.applications
        , metrics.dispatches
        ]

data ReportConfig = ReportConfig
  { expectedMetricsChange :: MetricsChange
  , css :: String
  , js :: String
  }

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
    , style ! type_ "text/css" $ toHtml reportConfig.css
    , script $ toHtml reportConfig.js
    ]

toStringReport :: ReportConfig -> Report -> String
toStringReport reportConfig report = renderHtml $ toHtmlReport reportConfig report
