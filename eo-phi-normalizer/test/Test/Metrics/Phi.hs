{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Test.Metrics.Phi where

import Data.Yaml (FromJSON)
import GHC.Generics (Generic)
import Language.EO.Phi.Metrics (ProgramMetrics)

data MetricsTestSet = MetricsTestSet
  { title :: String
  , tests :: [MetricsTest]
  }
  deriving (Generic, FromJSON)

data MetricsTest = MetricsTest
  { title :: String
  , phi :: String
  , metrics :: ProgramMetrics
  }
  deriving (Generic, FromJSON)
