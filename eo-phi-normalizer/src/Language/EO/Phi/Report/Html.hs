{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.EO.Phi.Report.Html where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Language.EO.Phi.Metrics.Data (Metrics (..), MetricsCount, toListMetrics)
import Language.EO.Phi.Report.Data (MetricsChange, MetricsChangeCategorized, MetricsChangeCategory (..), Percent (..), ProgramReport (..), Report (..), ReportRow (..))
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes (class_, colspan, type_)
import Text.Printf (printf)
import Prelude hiding (div, id, span)

toHtmlReportHeader :: Html
toHtmlReportHeader =
  thead $
    toHtml
      [ tr $
          toHtml
            [ th ! colspan "2" ! class_ "no-sort" $ "Attribute"
            , th ! colspan "4" ! class_ "no-sort" $ "Improvement = (Before - After) / Before"
            , th ! colspan "4" ! class_ "no-sort" $ "Before"
            , th ! colspan "4" ! class_ "no-sort" $ "After"
            , th ! colspan "4" ! class_ "no-sort" $ "Location"
            ]
      , tr . toHtml $
          th
            <$> [ "Attribute Before"
                , "Attribute After"
                ]
              <> ( concat . replicate 3 $
                    [ "Dataless"
                    , "Applications"
                    , "Formations"
                    , "Dispatches"
                    ]
                 )
              <> [ "File Before"
                 , "Bindings Path Before"
                 , "File After"
                 , "Bindings Path After"
                 ]
      ]

roundToStr :: Int -> Double -> String
roundToStr = printf "%0.*f%%"

instance ToMarkup Percent where
  toMarkup :: Percent -> Markup
  toMarkup Percent{..} = toMarkup $ roundToStr 2 (percent * 100)

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

-- >>> import Text.Blaze.Html.Renderer.String (renderHtml)
--
-- >>> renderHtml $ toHtmlChange (MetricsChange'Bad 0.2)
-- "<td class=\"number bad\">0.2</td>"
--
-- >>> renderHtml $ toHtmlChange (MetricsChange'Good 0.5)
-- "<td class=\"number good\">0.5</td>"
-- >>> renderHtml $ toHtmlChange (MetricsChange'NaN :: MetricsChangeCategory Double)
-- "<td class=\"number nan\">NaN</td>"
toHtmlChange :: (ToMarkup a) => ReportConfig -> MetricsChangeCategory a -> Html
toHtmlChange reportConfig = \case
  MetricsChange'NaN -> td ! class_ "number nan" $ toHtml @String "NaN" <> toHtml ['🟣' | isMarkdown]
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
      <> toHtmlMetricsChange reportConfig reportRow.metricsChange
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
  toHtml $
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
    ]
      <> ( case reportConfig.format of
            ReportFormat'Html{..} ->
              [ style ! type_ "text/css" $ toHtml css
              , script $ toHtml js
              ]
            ReportFormat'Markdown -> []
         )

toStringReport :: ReportConfig -> Report -> String
toStringReport reportConfig report = renderHtml $ toHtmlReport reportConfig report
