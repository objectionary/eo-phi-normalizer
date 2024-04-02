{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.EO.Phi.Metrics.SpecialDouble (
  SpecialDouble,
  defaultSpecialDouble,
  unSpecialDouble,
) where

import Data.Aeson
import Data.Aeson.Types (parseFail)
import Data.Scientific (toRealFloat)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Blaze.Html5 hiding (i)
import Text.Printf (FieldFormatter, PrintfArg (formatArg))

newtype SpecialDouble = SpecialDouble {unSpecialDouble :: Double} deriving newtype (Num, Fractional, Floating, Real, RealFrac, RealFloat)

defaultSpecialDouble :: SpecialDouble
defaultSpecialDouble = SpecialDouble (-1.0 / 0.0)

-- >>> (0.0 / 0.0) :: SpecialDouble
-- -Infinity

-- >>> (1.0 / 0.0) :: SpecialDouble
-- -Infinity

-- >>> (-1.0 / 0.0) :: SpecialDouble
-- -Infinity

normalizeSpecialDouble :: SpecialDouble -> SpecialDouble
normalizeSpecialDouble (SpecialDouble x)
  | isNaN x || isInfinite x = defaultSpecialDouble
  | otherwise = SpecialDouble x

instance Show SpecialDouble where
  show :: SpecialDouble -> String
  show (unSpecialDouble . normalizeSpecialDouble -> x) = show x

instance Eq SpecialDouble where
  (==) :: SpecialDouble -> SpecialDouble -> Bool
  (unSpecialDouble . normalizeSpecialDouble -> x) == (unSpecialDouble . normalizeSpecialDouble -> y) = x == y

instance Ord SpecialDouble where
  (<=) :: SpecialDouble -> SpecialDouble -> Bool
  (unSpecialDouble . normalizeSpecialDouble -> x) <= (unSpecialDouble . normalizeSpecialDouble -> y) = x <= y

minusInfinityText :: Text
minusInfinityText = "-Infinity"

instance FromJSON SpecialDouble where
  parseJSON (String s) =
    withText
      (T.unpack minusInfinityText)
      ( \case
          x | x == minusInfinityText -> pure defaultSpecialDouble
          _ -> parseFail [i|String is not #{minusInfinityText}|]
      )
      (String s)
  parseJSON (Number n) =
    withScientific
      "Double"
      (pure . toRealFloat)
      (Number n)
  parseJSON _ = parseFail [i|Value is not a ${minusInfinityText} or a Double|]

instance ToJSON SpecialDouble where
  toJSON :: SpecialDouble -> Value
  toJSON (unSpecialDouble . normalizeSpecialDouble -> x) = toJSON x

instance PrintfArg SpecialDouble where
  formatArg :: SpecialDouble -> Text.Printf.FieldFormatter
  formatArg (unSpecialDouble . normalizeSpecialDouble -> x) = formatArg x

instance ToMarkup SpecialDouble where
  toMarkup :: SpecialDouble -> Markup
  toMarkup x
    | x == defaultSpecialDouble = toMarkup ("N/A" :: String)
    | otherwise = toMarkup $ unSpecialDouble x
