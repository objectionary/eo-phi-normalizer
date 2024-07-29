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
import Data.Functor ((<&>))

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

class (Num a) => ToDataSort a where
  toDataSort :: a -> Integer

-- TODO #389:30m
-- I couldn't make PyF pad doubles with zeros using {n:05} syntax
-- because PyF also counts the digits after .
instance ToDataSort Double where
  toDataSort :: Double -> Integer
  toDataSort number = round $ number * 1000

instance ToDataSort Percent where
  toDataSort :: Percent -> Integer
  toDataSort (Percent number) = toDataSort number

instance ToDataSort Integer where
  toDataSort :: Integer -> Integer
  toDataSort x = x

mkDataSortAttribute :: AttributeValue -> Attribute
mkDataSortAttribute = dataAttribute "sort"

-- >>> pipelineConfig = ReportFormat'Markdown
--
-- >>> renderHtml $ toHtmlChange pipelineConfig (MetricsChange'Bad (Percent 0.2))
-- "<td class=\"number bad\" data-sort=\"1000200\">20.00%\128308</td>"
--
-- >>> renderHtml $ toHtmlChange pipelineConfig (MetricsChange'Good (Percent 0.5))
-- "<td class=\"number good\" data-sort=\"1000500\">50.00%\128994</td>"
-- >>> renderHtml $ toHtmlChange pipelineConfig (MetricsChange'NA :: MetricsChangeCategory Percent)
-- "<td class=\"number not-applicable\" data-sort=\"0000000\">N/A\128995</td>"
toHtmlChange :: forall a. (ToMarkup a, ToDataSort a) => ReportFormat -> MetricsChangeCategory a -> Html
toHtmlChange reportFormat = \case
  MetricsChange'NA -> td ! class_ [fmt|{number} not-applicable|] ! mkDataSortAttributeNA $ toHtml na <> toHtml ['🟣' | isMarkdown]
  MetricsChange'Bad{..} -> td ! class_ [fmt|{number} bad|] ! mkDataSortAttributeChange change $ toHtml change <> toHtml ['🔴' | isMarkdown]
  MetricsChange'Good{..} -> td ! class_ [fmt|{number} good|] ! mkDataSortAttributeChange change $ toHtml change <> toHtml ['🟢' | isMarkdown]
 where
  isMarkdown = reportFormat == ReportFormat'Markdown
  mkDataSortAttributeChange change = mkDataSortAttribute [fmt|1{toDataSort change:06}|]
  mkDataSortAttributeNA = mkDataSortAttribute [fmt|0{(toDataSort (Percent 0.0)):06}|]
  na :: String
  na = "N/A"
  number :: String
  number = "number"

toHtmlMetricsChange :: ReportFormat -> MetricsChangeCategorized -> [Html]
toHtmlMetricsChange reportFormat change = toHtmlChange reportFormat <$> toListMetrics change

toHtmlMetrics :: MetricsCount -> [Html]
toHtmlMetrics metrics =
  toListMetrics metrics <&>
    (\x -> td ! class_ "number" ! mkDataSortAttribute [fmt|{x:06}|] $ toHtml x)

toHtmlReportRow :: ReportFormat -> Int -> ReportRow -> Html
toHtmlReportRow reportFormat index reportRow =
  tr . toHtml $
    ( td
        . toHtml
        <$> [ [fmt|{index}|]
            , fromMaybe na reportRow.attributeInitial
            , fromMaybe na reportRow.attributeNormalized
            ]
    )
      <> toHtmlMetricsChange reportFormat reportRow.metricsChange
      <> toHtmlMetrics reportRow.metricsInitial
      <> toHtmlMetrics reportRow.metricsNormalized
      <> ( td
            . toHtml
            <$> [ fromMaybe allPrograms reportRow.fileInitial
                , intercalate "." $ fromMaybe [wholeProgram] reportRow.bindingsPathInitial
                , fromMaybe allPrograms reportRow.fileNormalized
                , intercalate "." $ fromMaybe [wholeProgram] reportRow.bindingsPathNormalized
                ]
         )
 where
  wholeProgram = "[whole program]"
  allPrograms = "[all programs]"
  na = "[N/A]"

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
