{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.EO.Phi.Report.Html where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate
import Language.EO.Phi.Metrics.Data (Metrics (..), MetricsCount, SafeNumber (..), toListMetrics)
import Language.EO.Phi.Report.Data (MetricsChange, MetricsChangeCategorized, MetricsChangeCategory (..), Percent (..), ProgramReport (..), Report (..), ReportRow (..))
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 hiding (i)
import Text.Blaze.Html5.Attributes (class_, colspan, type_)
import Text.Printf (printf)
import Prelude hiding (div, span)

metricsNames :: Metrics String
metricsNames =
  Metrics
    { dataless = "Dataless formations"
    , applications = "Applications"
    , formations = "Formations"
    , dispatches = "Dispatches"
    }

toHtmlReportHeader :: Html
toHtmlReportHeader =
  thead $
    toHtml
      [ tr $
          toHtml
            [ th ! colspan "2" ! class_ "no-sort" $ "Attribute"
            , th ! colspan "4" ! class_ "no-sort" $ "Improvement = (Initial - Normalized) / Initial"
            , th ! colspan "4" ! class_ "no-sort" $ "Initial"
            , th ! colspan "4" ! class_ "no-sort" $ "Normalized"
            , th ! colspan "4" ! class_ "no-sort" $ "Location"
            ]
      , tr . toHtml $
          th
            <$> [ "Attribute Initial"
                , "Attribute Normalized"
                ]
              <> ( concat
                    . replicate 3
                    . (toHtml <$>)
                    $ toListMetrics metricsNames
                 )
              <> [ "File Initial"
                 , "Bindings Path Initial"
                 , "File Normalized"
                 , "Bindings Path Normalized"
                 ]
      ]

data ReportFormat
  = ReportFormat'Html
      { css :: String
      , js :: String
      }
  | -- | GitHub Flavored Markdown
    ReportFormat'Markdown
  deriving stock (Eq)

data ReportConfig = ReportConfig
  { expectedMetricsChange :: MetricsChange
  , format :: ReportFormat
  }

instance (ToMarkup a) => ToMarkup (SafeNumber a) where
  toMarkup :: SafeNumber a -> Markup
  toMarkup (SafeNumber'Number n) = toMarkup n
  toMarkup SafeNumber'NaN = toMarkup ("NaN" :: String)

roundToStr :: Int -> Double -> String
roundToStr = printf "%0.*f%%"

instance ToMarkup Percent where
  toMarkup :: Percent -> Markup
  toMarkup Percent{..} = toMarkup $ roundToStr 2 (percent * 100)

-- >>> import Text.Blaze.Html.Renderer.String (renderHtml)
-- >>> reportConfig = ReportConfig { expectedMetricsChange = 0, format = ReportFormat'Markdown }
--
-- >>> renderHtml $ toHtmlChange reportConfig (MetricsChange'Bad 0.2)
-- "<td class=\"number bad\">0.2\128308</td>"
--
-- >>> renderHtml $ toHtmlChange reportConfig (MetricsChange'Good 0.5)
-- "<td class=\"number good\">0.5\128994</td>"
-- >>> renderHtml $ toHtmlChange reportConfig (MetricsChange'NA :: MetricsChangeCategory Double)
-- "<td class=\"number not-applicable\">N/A\128995</td>"
toHtmlChange :: (ToMarkup a) => ReportConfig -> MetricsChangeCategory a -> Html
toHtmlChange reportConfig = \case
  MetricsChange'NA -> td ! class_ "number not-applicable" $ toHtml ("N/A" :: String) <> toHtml ['🟣' | isMarkdown]
  MetricsChange'Bad{..} -> td ! class_ "number bad" $ toHtml change <> toHtml ['🔴' | isMarkdown]
  MetricsChange'Good{..} -> td ! class_ "number good" $ toHtml change <> toHtml ['🟢' | isMarkdown]
 where
  isMarkdown = reportConfig.format == ReportFormat'Markdown

toHtmlMetricsChange :: ReportConfig -> MetricsChangeCategorized -> [Html]
toHtmlMetricsChange reportConfig change = toHtmlChange reportConfig <$> toListMetrics change

toHtmlMetrics :: MetricsCount -> [Html]
toHtmlMetrics metrics =
  (td ! class_ "number")
    . toHtml
    <$> toListMetrics metrics

toHtmlReportRow :: ReportConfig -> ReportRow -> Html
toHtmlReportRow reportConfig reportRow =
  tr . toHtml $
    ( td
        . toHtml
        <$> [ fromMaybe "[N/A]" reportRow.attributeInitial
            , fromMaybe "[N/A]" reportRow.attributeNormalized
            ]
    )
      <> toHtmlMetricsChange reportConfig reportRow.metricsChange
      <> toHtmlMetrics reportRow.metricsInitial
      <> toHtmlMetrics reportRow.metricsNormalized
      <> ( td
            . toHtml
            <$> [ fromMaybe "[all files]" reportRow.fileInitial
                , intercalate "." $ fromMaybe ["[whole program]"] reportRow.bindingsPathInitial
                , fromMaybe "[all files]" reportRow.fileNormalized
                , intercalate "." $ fromMaybe ["[whole program]"] reportRow.bindingsPathNormalized
                ]
         )

-- | Number of tests where metrics changes were better than expected
countGoodMetricsChanges :: Report -> MetricsCount
countGoodMetricsChanges report = metricsCount
 where
  metricsChanges = (.metricsChange) <$> concatMap (.bindingsRows) report.programReports
  metricsCount = foldMap ((\case MetricsChange'Good _ -> 1; _ -> 0) <$>) metricsChanges

-- | Number of tests in all programs
countTests :: Report -> Int
countTests report = length $ concatMap (.bindingsRows) report.programReports

toHtmlReport :: ReportConfig -> Report -> Html
toHtmlReport reportConfig report =
  (if isMarkdown then details else id) . toHtml $
    [ summary "Report"
    | reportConfig.format == ReportFormat'Markdown
    ]
      <> [ h2 "Overview"
            <> p
              [i|
                We translate EO files into PHI programs.
                Next, we normalize these programs.
                Then, we collect metrics for initial and normalized PHI programs.
              |]
            <> p
              [i|
                An EO file contains multiple test objects.
                After conversion, these test objects become attributes in PHI programs.
                We call these attributes "tests".
              |]
            <> p
              [i|
                In the report below, we present total metrics for all programs,
                for programs,
                and for tests.
              |]
            <> h2 "Metrics"
            <> p
              [i|
                We collect metrics on the number of dataless formations, applications, formations, and dispatches.
                We want normalized PHI programs to have a smaller number of such elements than initial PHI programs.
              |]
            <> p [i|These numbers are expected to reduce as follows:|]
            <> ul
              ( toHtml . toListMetrics $
                  makeMetricsItem
                    <$> metricsNames
                    <*> ((Percent <$>) <$> reportConfig.expectedMetricsChange)
              )
            <> h2 "Statistics"
            <> p [i|Total number of tests: #{countTests report}.|]
            <> p
              [i|
                For each metric, number of tests where the metric was reduced as expected
                (not counting the tests where the metric was initially 0):
              |]
            <> ul
              ( toHtml . toListMetrics $
                  makeMetricsItem
                    <$> metricsNames
                    <*> countGoodMetricsChanges report
              )
            <> h2 "Detailed results"
         ]
      <> [ table ! class_ "sortable" $
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
         ]
      <> ( case reportConfig.format of
            ReportFormat'Html{..} ->
              [ style ! type_ "text/css" $ toHtml css
              , script $ toHtml js
              ]
            ReportFormat'Markdown -> []
         )
 where
  isMarkdown = reportConfig.format == ReportFormat'Markdown
  makeMetricsItem :: (ToMarkup a) => String -> a -> Html
  makeMetricsItem name x = li $ b (toHtml ([i|#{name}: |] :: String)) <> toHtml x

toStringReport :: ReportConfig -> Report -> String
toStringReport reportConfig report = renderHtml $ toHtmlReport reportConfig report
