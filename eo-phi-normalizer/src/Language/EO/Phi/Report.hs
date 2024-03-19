{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.EO.Phi.Report where

import GHC.Generics (Generic)
import Language.EO.Phi.TH (deriveJSON)

data ReportItem = ReportItem
  { phi :: String
  , phiNormalized :: String
  , bindingsPathPhi :: Maybe String
  , bindingsPathPhiNormalized :: Maybe String
  , metricsPhi :: Maybe String
  , metricsPhiNormalized :: Maybe String
  }
  deriving (Show, Generic)

$(deriveJSON ''ReportItem)

data ReportConfig = ReportConfig
  { outputFile :: FilePath
  , items :: [ReportItem]
  }
  deriving (Show, Generic)

$(deriveJSON ''ReportConfig)
