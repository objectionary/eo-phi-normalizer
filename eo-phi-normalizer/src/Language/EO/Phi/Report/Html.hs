{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

-- >>> import Text.Blaze.Html.Renderer.String (renderHtml)
--
-- >>> renderHtml $ toHtmlChange (MetricsChange'Bad 0.2)
-- "<td class=\"number bad\">0.2</td>"
--
-- >>> renderHtml $ toHtmlChange (MetricsChange'Good 0.5)
-- "<td class=\"number good\">0.5</td>"
-- >>> renderHtml $ toHtmlChange (MetricsChange'NaN :: MetricsChangeCategory Double)
-- "<td class=\"number nan\">NaN</td>"
toHtmlChange :: (ToMarkup a) => MetricsChangeCategory a -> Html
toHtmlChange = \case
  MetricsChange'NaN -> td ! class_ "number nan" $ "NaN"
  MetricsChange'Bad{..} -> td ! class_ "number bad" $ toHtml change
  MetricsChange'Good{..} -> td ! class_ "number good" $ toHtml change

toHtmlMetricsChange :: MetricsChangeCategorized -> [Html]
toHtmlMetricsChange change = toHtmlChange <$> toListMetrics change

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

toHtmlReportRow :: ReportRow -> Html
toHtmlReportRow reportRow =
  tr . toHtml $
    ( td
        . toHtml
        <$> [ fromMaybe "[N/A]" reportRow.attributeBefore
            , fromMaybe "[N/A]" reportRow.attributeAfter
            ]
    )
      <> toHtmlMetricsChange reportRow.metricsChange
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
              toHtmlReportRow
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
