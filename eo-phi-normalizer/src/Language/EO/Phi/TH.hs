module Language.EO.Phi.TH where

import Data.Aeson (Options (..), camelTo2)
import Data.Aeson.TH as TH (deriveJSON)
import Data.Aeson.Types (defaultOptions)
import Language.Haskell.TH (Dec, Name, Q)

defaultOptions' :: Options
defaultOptions' = defaultOptions{fieldLabelModifier = camelTo2 '-'}

deriveJSON :: Name -> Q [Dec]
deriveJSON = TH.deriveJSON defaultOptions'
