{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

{- FOURMOLU_DISABLE -}

-- $setup
-- >>> :set -XOverloadedStrings

{- FOURMOLU_ENABLE -}

module Language.EO.Phi.Metrics where

import Control.Lens ((+=))
import Control.Monad (forM_)
import Control.Monad.State (State, runState)
import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Maybe (catMaybes)
import Data.Traversable (forM)
import GHC.Generics (Generic)
import Language.EO.Phi.Rules.Common ()
import Language.EO.Phi.Syntax.Abs

data ObjectMetrics = ObjectMetrics
  { dataless :: Int
  , applications :: Int
  , formations :: Int
  , dispatches :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, Eq)

defaultObjectMetrics :: ObjectMetrics
defaultObjectMetrics =
  ObjectMetrics
    { dataless = 0
    , applications = 0
    , formations = 0
    , dispatches = 0
    }

instance Semigroup ObjectMetrics where
  (<>) :: ObjectMetrics -> ObjectMetrics -> ObjectMetrics
  x <> y =
    ObjectMetrics
      { dataless = x.dataless + y.dataless
      , applications = x.applications + y.applications
      , formations = x.formations + y.formations
      , dispatches = x.dispatches + y.dispatches
      }

instance Monoid ObjectMetrics where
  mempty :: ObjectMetrics
  mempty = defaultObjectMetrics

data BindingMetrics = BindingMetrics
  { name :: String
  , metrics :: ObjectMetrics
  }
  deriving (Show, Generic, FromJSON, ToJSON, Eq)

count :: (a -> Bool) -> [a] -> Int
count x = length . filter x

getHeight :: (Ord b, Num b) => [Binding] -> [Maybe b] -> b
getHeight bindings heights
  | hasDeltaAttribute = 1
  | otherwise = heightAttributes
 where
  heightAttributes = heights & catMaybes & (\case [] -> 0; x -> minimum x + 1)
  hasDeltaAttribute = not $ null [x | x@(DeltaBinding _, _) <- zip bindings heights]

countDataless :: Int -> Int
countDataless x
  | x == 0 || x > 2 = 1
  | otherwise = 0

class Inspectable a where
  inspect :: a -> State ObjectMetrics (Maybe Int)

instance Inspectable Binding where
  inspect :: Binding -> State ObjectMetrics (Maybe Int)
  inspect = \case
    AlphaBinding _ obj -> do
      inspect obj
    _ -> pure Nothing

instance Inspectable Object where
  inspect :: Object -> State ObjectMetrics (Maybe Int)
  inspect = \case
    Formation bindings -> do
      #formations += 1
      heights <- forM bindings inspect
      let height = getHeight bindings heights
      #dataless += countDataless height
      pure (Just height)
    Application obj bindings -> do
      #applications += 1
      _ <- inspect obj
      forM_ bindings inspect
      pure Nothing
    ObjectDispatch obj _ -> do
      #dispatches += 1
      _ <- inspect obj
      pure Nothing
    _ -> pure Nothing

data ProgramMetrics = ProgramMetrics
  { attributes :: [BindingMetrics]
  , program :: ObjectMetrics
  }
  deriving (Show, Generic, FromJSON, ToJSON, Eq)

defaultMetrics :: ProgramMetrics
defaultMetrics =
  ProgramMetrics
    { attributes = []
    , program = defaultObjectMetrics
    }

-- | Count dataless formations in a list of bindings
--
-- >>> collectMetrics "{⟦ α0 ↦ ξ, α0 ↦ Φ.org.eolang.bytes( Δ ⤍ 00- ) ⟧}"
-- ProgramMetrics {attributes = [BindingMetrics {name = "\945\&0", metrics = ObjectMetrics {dataless = 0, applications = 0, formations = 0, dispatches = 0}},BindingMetrics {name = "\945\&0", metrics = ObjectMetrics {dataless = 0, applications = 1, formations = 0, dispatches = 3}}], program = ObjectMetrics {dataless = 1, applications = 1, formations = 1, dispatches = 3}}
--
-- >>> collectMetrics "{⟦ α0 ↦ ξ, Δ ⤍ 00- ⟧}"
-- ProgramMetrics {attributes = [BindingMetrics {name = "\945\&0", metrics = ObjectMetrics {dataless = 0, applications = 0, formations = 0, dispatches = 0}}], program = ObjectMetrics {dataless = 0, applications = 0, formations = 1, dispatches = 0}}
--
--
-- >>> collectMetrics "{⟦ α0 ↦ ξ, α1 ↦ ⟦ Δ ⤍ 00- ⟧ ⟧}"
-- ProgramMetrics {attributes = [BindingMetrics {name = "\945\&0", metrics = ObjectMetrics {dataless = 0, applications = 0, formations = 0, dispatches = 0}},BindingMetrics {name = "\945\&1", metrics = ObjectMetrics {dataless = 0, applications = 0, formations = 1, dispatches = 0}}], program = ObjectMetrics {dataless = 0, applications = 0, formations = 2, dispatches = 0}}
--
--
-- >>> collectMetrics "{⟦ α0 ↦ ξ, α1 ↦ ⟦ α2 ↦ ⟦ Δ ⤍ 00- ⟧ ⟧ ⟧}"
-- ProgramMetrics {attributes = [BindingMetrics {name = "\945\&0", metrics = ObjectMetrics {dataless = 0, applications = 0, formations = 0, dispatches = 0}},BindingMetrics {name = "\945\&1", metrics = ObjectMetrics {dataless = 0, applications = 0, formations = 2, dispatches = 0}}], program = ObjectMetrics {dataless = 1, applications = 0, formations = 3, dispatches = 0}}
--
-- >>> collectMetrics "{⟦ Δ ⤍ 00- ⟧}"
-- ProgramMetrics {attributes = [], program = ObjectMetrics {dataless = 0, applications = 0, formations = 1, dispatches = 0}}
--
-- >>> collectMetrics "{⟦ α0 ↦ ⟦ α0 ↦ ∅ ⟧ ⟧}"
-- ProgramMetrics {attributes = [BindingMetrics {name = "\945\&0", metrics = ObjectMetrics {dataless = 1, applications = 0, formations = 1, dispatches = 0}}], program = ObjectMetrics {dataless = 1, applications = 0, formations = 2, dispatches = 0}}
--
-- >>> collectMetrics "{⟦ α0 ↦ ⟦ α0 ↦ ⟦ α0 ↦ ∅ ⟧ ⟧ ⟧}"
-- ProgramMetrics {attributes = [BindingMetrics {name = "\945\&0", metrics = ObjectMetrics {dataless = 1, applications = 0, formations = 2, dispatches = 0}}], program = ObjectMetrics {dataless = 1, applications = 0, formations = 3, dispatches = 0}}
--
-- >>> collectMetrics "{⟦ α0 ↦ ⟦ α0 ↦ ⟦ α0 ↦ ⟦ α0 ↦ ∅ ⟧ ⟧ ⟧ ⟧}"
-- ProgramMetrics {attributes = [BindingMetrics {name = "\945\&0", metrics = ObjectMetrics {dataless = 1, applications = 0, formations = 3, dispatches = 0}}], program = ObjectMetrics {dataless = 2, applications = 0, formations = 4, dispatches = 0}}
--
-- >>> collectMetrics "{ ⟦ org ↦ ⟦ ⟧ ⟧ }"
-- ProgramMetrics {attributes = [BindingMetrics {name = "org", metrics = ObjectMetrics {dataless = 1, applications = 0, formations = 1, dispatches = 0}}], program = ObjectMetrics {dataless = 1, applications = 0, formations = 2, dispatches = 0}}
collectMetrics :: Program -> ProgramMetrics
collectMetrics (Program bindings) =
  let attributes' = flip runState mempty . inspect <$> bindings
      (heights, objectMetrics) = unzip attributes'
      attributes = do
        x <- zip bindings objectMetrics
        case x of
          (AlphaBinding (Alpha (AlphaIndex name)) _, metrics) -> [BindingMetrics{..}]
          (AlphaBinding (Label (LabelId name)) _, metrics) -> [BindingMetrics{..}]
          _ -> []
      height = getHeight bindings heights
      program =
        fold objectMetrics
          & \x ->
            x
              { dataless = x.dataless + countDataless height
              , formations = x.formations + 1
              }
   in ProgramMetrics{..}
