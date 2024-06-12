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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.EO.Phi.Report.Html where

import Data.FileEmbed (embedFileRelative)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Language.EO.Phi.Metrics.Data (Metrics (..), MetricsCount, toListMetrics)
import Language.EO.Phi.Pipeline.Config
import Language.EO.Phi.Report.Data
import PyF (fmt)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 hiding (i)
import Text.Blaze.Html5 qualified as TBH
import Text.Blaze.Html5.Attributes (charset, class_, colspan, content, id, lang, onclick, type_, value)
import Text.Blaze.Html5.Attributes qualified as TBHA
import Prelude hiding (div, id, span)
import Prelude qualified

-- $setup
-- >>> import Text.Blaze.Html.Renderer.String (renderHtml)

-- | JavaScript file to embed into HTML reports
reportJS :: String
reportJS = T.unpack $ T.decodeUtf8 $(embedFileRelative "report/main.js")

-- | CSS file to embed into HTML reports
reportCSS :: String
reportCSS = T.unpack $ T.decodeUtf8 $(embedFileRelative "report/styles.css")

metricsNames :: Metrics String
metricsNames =
  Metrics
    { dataless = "Dataless formations"
    , applications = "Applications"
    , formations = "Formations"
    , dispatches = "Dispatches"
    }

toHtmlReportTableHeader :: Html
toHtmlReportTableHeader =
  thead $
    toHtml
      [ tr $
          toHtml
            [ th ! colspan "1" ! class_ "no-sort" $ ""
            , th ! colspan "2" ! class_ "no-sort" $ "Attribute"
            , th ! colspan "4" ! class_ "no-sort" $ "Change"
            , th ! colspan "4" ! class_ "no-sort" $ "Initial"
            , th ! colspan "4" ! class_ "no-sort" $ "Normalized"
            , th ! colspan "4" ! class_ "no-sort" $ "Location"
            ]
      , tr . toHtml $
          th
            <$> [ "Test #"
                , "Attribute Initial"
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

instance ToMarkup Percent where
  toMarkup :: Percent -> Markup
  toMarkup = toMarkup . show

-- >>> pipelineConfig = ReportConfig { general = ReportGeneralConfig { expectedMetricsChange = 0, format = ReportFormat'Markdown } }
--
-- >>> renderHtml $ toHtmlChange pipelineConfig (MetricsChange'Bad 0.2)
-- "<td class=\"number bad\">0.2\128308</td>"
--
-- >>> renderHtml $ toHtmlChange pipelineConfig (MetricsChange'Good 0.5)
-- "<td class=\"number good\">0.5\128994</td>"
-- >>> renderHtml $ toHtmlChange pipelineConfig (MetricsChange'NA :: MetricsChangeCategory Double)
-- "<td class=\"number not-applicable\">N/A\128995</td>"
toHtmlChange :: (ToMarkup a) => ReportFormat -> MetricsChangeCategory a -> Html
toHtmlChange reportFormat = \case
  MetricsChange'NA -> td ! class_ "number not-applicable" $ toHtml ("N/A" :: String) <> toHtml ['🟣' | isMarkdown]
  MetricsChange'Bad{..} -> td ! class_ "number bad" $ toHtml change <> toHtml ['🔴' | isMarkdown]
  MetricsChange'Good{..} -> td ! class_ "number good" $ toHtml change <> toHtml ['🟢' | isMarkdown]
 where
  isMarkdown = reportFormat == ReportFormat'Markdown

toHtmlMetricsChange :: ReportFormat -> MetricsChangeCategorized -> [Html]
toHtmlMetricsChange reportFormat change = toHtmlChange reportFormat <$> toListMetrics change

toHtmlMetrics :: MetricsCount -> [Html]
toHtmlMetrics metrics =
  (td ! class_ "number")
    . toHtml
    <$> toListMetrics metrics

toHtmlReportRow :: ReportFormat -> Int -> ReportRow -> Html
toHtmlReportRow reportFormat index reportRow =
  tr . toHtml $
    ( td
        . toHtml
        <$> [ [fmt|{index}|]
            , fromMaybe "[N/A]" reportRow.attributeInitial
            , fromMaybe "[N/A]" reportRow.attributeNormalized
            ]
    )
      <> toHtmlMetricsChange reportFormat reportRow.metricsChange
      <> toHtmlMetrics reportRow.metricsInitial
      <> toHtmlMetrics reportRow.metricsNormalized
      <> ( td
            . toHtml
            <$> [ fromMaybe "[all programs]" reportRow.fileInitial
                , intercalate "." $ fromMaybe ["[whole program]"] reportRow.bindingsPathInitial
                , fromMaybe "[all programs]" reportRow.fileNormalized
                , intercalate "." $ fromMaybe ["[whole program]"] reportRow.bindingsPathNormalized
                ]
         )

toHtmlReport :: ReportFormat -> PipelineConfig -> Report -> Html
toHtmlReport reportFormat pipelineConfig report =
  toHtml
    [ memptyIfMarkdown docType
    , idIfMarkdown (TBH.html ! lang "en-US") $
        toHtml
          [ idIfMarkdown TBH.head $
              memptyIfMarkdown . toHtml $
                [ meta ! charset "utf-8"
                , meta ! TBHA.name "viewport" ! content "width=device-width, initial-scale=1.0"
                , TBH.title "Report"
                , -- https://stackoverflow.com/a/55743302
                  -- https://stackoverflow.com/a/3169849
                  script ! type_ "text/javascript" $
                    [fmt|
                      function copytable(el) {{
                        var urlField = document.getElementById(el)
                        var range = document.createRange()
                        range.selectNode(urlField)
                        window.getSelection().addRange(range)
                        document.execCommand('copy')

                        if (window.getSelection().empty) {{  // Chrome
                          window.getSelection().empty();
                        }} else if (window.getSelection().removeAllRanges) {{  // Firefox
                          window.getSelection().removeAllRanges();
                        }}
                      }}
                    |]
                ]
                  <> catMaybes
                    [ pipelineConfig.report.input >>= (.js) >>= \js -> pure (script $ toHtml js)
                    , pipelineConfig.report.input >>= (.css) >>= \css -> pure (style ! type_ "text/css" $ toHtml css)
                    ]
          , idIfMarkdown body $
              toHtml . (toHtml <$>) $
                [
                  [ h2 "Overview"
                  , p
                      [fmt|
                        We translate EO files into initial PHI programs.
                        Next, we normalize these programs and get normalized PHI programs.
                        Then, we collect metrics for initial and normalized PHI programs.
                      |]
                  , h2 "Metrics"
                  , p
                      [fmt|
                        An EO file contains multiple test objects.
                        After translation, these test objects become attributes in PHI programs.
                        We call these attributes "tests".
                      |]
                  , p
                      [fmt|
                        We collect metrics on the number of {intercalate ", " (toListMetrics metricsNames)} in tests.
                        We want normalized tests to have less such elements than initial tests do.
                      |]
                  , p "A metric change for a test is calculated by the formula"
                  , p (code "(metric_initial - metric_normalized) / metric_initial")
                  , p "where:"
                  , ul
                      ( toHtml
                          [ li $ code "metric_initial" <> " is the metric for the initial test"
                          , li $ code "metric_normalized" <> " is the metric for the normalized test"
                          ]
                      )
                  , h3 "Expected"
                  , p [fmt|Metric changes are expected to be as follows or greater:|]
                  , ul
                      ( toHtml . toListMetrics $
                          mkPercentItem
                            <$> metricsNames
                            <*> pipelineConfig.report.expectedMetricsChange
                      )
                  , p
                      ( let expectedImprovedProgramsPercentage = pipelineConfig.report.expectedImprovedProgramsPercentage
                         in [fmt|We expect such changes for at least {expectedImprovedProgramsPercentage:s} of tests.|]
                      )
                  , h3 "Actual"
                  , p [fmt|We normalized {testsCount} tests.|]
                  , p [fmt|All metrics were improved for {mkNumber allGoodMetricsCount testsCount} tests.|]
                  , p [fmt|Tests where a particular metric was improved:|]
                  , ul
                      ( toHtml . toListMetrics $
                          mkItem'
                            <$> metricsNames
                            <*> particularMetricsChangeGoodCount
                      )
                  , h2 "Table"
                  , p [fmt|The table below provides detailed information about tests.|]
                  ]
                , memptyIfMarkdown
                    [ TBH.input ! type_ "button" ! value "Copy to Clipboard" ! onclick "copytable('table')"
                    , h3 "Columns"
                    , p "Columns in this table are sortable."
                    , p "Hover over a header cell from the second row of header cells (Attribute Initial, etc.) to see a triangle demonstrating the sorting order."
                    , ul
                        ( toHtml
                            [ li "▾: descending"
                            , li "▴: ascending"
                            , li "▸: unordered"
                            ]
                        )
                    , p "Click on the triangle to change the sorting order in the corresponding column."
                    ]
                ,
                  [ table ! class_ "sortable" ! id "table" $
                      toHtml
                        [ toHtmlReportTableHeader
                        , tbody . toHtml $
                            uncurry (toHtmlReportRow reportFormat)
                              <$> zip [1 ..] (concat [programReport.bindingsRows | programReport <- report.programReports])
                        ]
                  ]
                ]
          ]
    ]
 where
  isMarkdown = reportFormat == ReportFormat'Markdown
  idIfMarkdown f = if isMarkdown then Prelude.id else f
  memptyIfMarkdown f = if isMarkdown then mempty else f

  tests = concatMap (.bindingsRows) report.programReports

  testsCount = length tests

  metricsChanges = (.metricsChange) <$> concatMap (.bindingsRows) report.programReports

  isGood = \case
    MetricsChange'Good _ -> True
    _ -> False

  countAllMetricsSatisfyingCondition cond = length $ filter (all cond) metricsChanges
  allGoodMetricsCount = countAllMetricsSatisfyingCondition isGood

  particularMetricsChangeGoodCount :: Metrics Int
  particularMetricsChangeGoodCount = sum $ ((\x -> if x then 1 else 0) . isGood <$>) <$> metricsChanges

  mkItem' = mkItem testsCount

-- |
-- >>> renderHtml (mkItem 10 "foo" 4)
-- "<li><b>foo: </b>4 (40.00%)</li>"
mkItem :: Int -> String -> Int -> Html
mkItem total name part = li $ b [fmt|{name}: |] <> toHtml (mkNumber part total)

mkPercentItem :: String -> Percent -> Html
mkPercentItem name percent = li $ b [fmt|{name}: |] <> toHtml percent

mkPercentage :: Int -> Int -> Percent
mkPercentage part total = Percent $ fromIntegral part / fromIntegral total

-- |
-- >>> mkNumber 3 5
-- "3 (60.00%)"
mkNumber :: Int -> Int -> String
mkNumber part total = [fmt|{part} ({mkPercentage part total:s})|]

toStringReport :: ReportFormat -> PipelineConfig -> Report -> String
toStringReport reportFormat pipelineConfig report = renderHtml $ toHtmlReport reportFormat pipelineConfig report
