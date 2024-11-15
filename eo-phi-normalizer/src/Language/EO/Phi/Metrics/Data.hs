-- The MIT License (MIT)

-- Copyright (c) 2016-2024 Objectionary.com

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.EO.Phi.Metrics.Data where

import Data.Aeson (ToJSON (..), Value (..), withObject, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (FromJSON (..), Parser)
import Data.Generics.Labels ()
import Data.List (groupBy, intercalate)
import GHC.Generics (Generic)
import Language.EO.Phi.Rules.Common ()
import Language.EO.Phi.TH (deriveJSON)

data Metrics a = Metrics
  { dataless :: a
  , applications :: a
  , formations :: a
  , dispatches :: a
  }
  deriving stock (Show, Generic, Eq, Functor, Foldable, Traversable)

$(deriveJSON ''Metrics)

toListMetrics :: Metrics a -> [a]
toListMetrics = foldMap (: [])

instance Applicative Metrics where
  pure :: a -> Metrics a
  pure a =
    Metrics
      { dataless = a
      , applications = a
      , formations = a
      , dispatches = a
      }

  (<*>) :: Metrics (a -> b) -> Metrics a -> Metrics b
  x <*> y =
    Metrics
      { dataless = x.dataless y.dataless
      , applications = x.applications y.applications
      , formations = x.formations y.formations
      , dispatches = x.dispatches y.dispatches
      }

instance (Num a) => Num (Metrics a) where
  (+) :: Metrics a -> Metrics a -> Metrics a
  (+) x y = (+) <$> x <*> y
  (-) :: Metrics a -> Metrics a -> Metrics a
  (-) x y = (-) <$> x <*> y
  (*) :: Metrics a -> Metrics a -> Metrics a
  (*) x y = (*) <$> x <*> y
  negate :: Metrics a -> Metrics a
  negate = (negate <$>)
  abs :: Metrics a -> Metrics a
  abs = (abs <$>)
  signum :: Metrics a -> Metrics a
  signum = (signum <$>)
  fromInteger :: Integer -> Metrics a
  fromInteger x = pure $ fromInteger x

instance (Fractional a) => Fractional (Metrics a) where
  fromRational :: Rational -> Metrics a
  fromRational _ = 0
  (/) :: Metrics a -> Metrics a -> Metrics a
  (/) x y = (/) <$> x <*> y

instance (Num a) => Semigroup (Metrics a) where
  (<>) :: Metrics a -> Metrics a -> Metrics a
  (<>) = (+)

instance (Num a) => Monoid (Metrics a) where
  mempty :: Metrics a
  mempty = 0

type MetricsCount = Metrics Int

data BindingMetrics = BindingMetrics
  { name :: String
  , metrics :: MetricsCount
  }
  deriving stock (Show, Generic, Eq)

$(deriveJSON ''BindingMetrics)

type Path = [String]

data BindingsByPathMetrics = BindingsByPathMetrics
  { path :: Path
  , bindingsMetrics :: [BindingMetrics]
  }
  deriving (Show, Generic, Eq)

-- >>> splitStringOn '.' "abra.cada.bra"
-- ["abra","cada","bra"]
--
-- >>> splitStringOn '.' ""
-- []
splitStringOn :: Char -> String -> Path
splitStringOn sep = filter (/= [sep]) . groupBy (\a b -> a /= sep && b /= sep)

splitPath :: String -> Path
splitPath = splitStringOn '.'

instance FromJSON BindingsByPathMetrics where
  parseJSON :: Value -> Parser BindingsByPathMetrics
  parseJSON = withObject "BindingsByPathMetrics" $ \obj -> do
    path <- splitPath <$> (obj .: "path")
    bindingsMetrics <- obj .: "bindings-metrics"
    pure BindingsByPathMetrics{..}

instance ToJSON BindingsByPathMetrics where
  toJSON :: BindingsByPathMetrics -> Value
  toJSON BindingsByPathMetrics{..} =
    Aeson.object
      [ "path" .= intercalate "." path
      , "bindings-metrics" .= bindingsMetrics
      ]

data ObjectMetrics = ObjectMetrics
  { bindingsByPathMetrics :: Maybe BindingsByPathMetrics
  , thisObjectMetrics :: MetricsCount
  }
  deriving (Show, Generic, Eq)

$(deriveJSON ''ObjectMetrics)

data ProgramMetrics = ProgramMetrics
  { bindingsByPathMetrics :: Maybe BindingsByPathMetrics
  , programMetrics :: MetricsCount
  }
  deriving (Show, Generic, Eq)

$(deriveJSON ''ProgramMetrics)
